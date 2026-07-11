(function () {
  "use strict";

  let draggedCard = null;
  let activeResize = null;
  let helpLastFocus = null;
  let helpLanguage = "zh";
  let helpScrollFrame = null;
  const pendingInputTimers = new Map();
  const resizeConfig = {
    library: { property: "--bp-library-width", selector: ".bp-library-panel", min: 220 },
    inspector: { property: "--bp-inspector-width", selector: ".bp-inspector-panel", min: 340 },
    preview: { property: "--bp-preview-width", selector: ".bp-preview-panel", min: 300 },
    workspace: { property: "--bp-workspace-height", selector: ".bp-workspace", min: 340 }
  };

  function sendInput(name, payload) {
    if (!window.Shiny || typeof window.Shiny.setInputValue !== "function") return;
    const value = Object.assign({}, payload, {
      nonce: Date.now() + Math.random()
    });
    window.Shiny.setInputValue(name, value, { priority: "event" });
  }

  function closest(target, selector) {
    return target instanceof Element ? target.closest(selector) : null;
  }

  function clamp(value, minimum, maximum) {
    return Math.min(Math.max(value, minimum), maximum);
  }

  function resizeBounds(target) {
    const config = resizeConfig[target];
    const workspace = document.querySelector(".bp-workspace");
    const lower = document.querySelector(".bp-lower-workspace");
    const shell = document.querySelector(".bp-app-shell");
    if (!config || !workspace || !lower || !shell) return null;

    if (target === "library" || target === "inspector") {
      const library = document.querySelector(".bp-library-panel");
      const inspector = document.querySelector(".bp-inspector-panel");
      if (!library || !inspector) return null;
      const handleSpace = Array.from(workspace.querySelectorAll(".bp-resize-vertical")).reduce(function (sum, handle) {
        return sum + handle.getBoundingClientRect().width;
      }, 0);
      const fixedPanelWidth = target === "library"
        ? inspector.getBoundingClientRect().width
        : library.getBoundingClientRect().width;
      return {
        min: config.min,
        max: Math.max(config.min, workspace.getBoundingClientRect().width - fixedPanelWidth - 420 - handleSpace)
      };
    }

    if (target === "preview") {
      const handle = lower.querySelector(".bp-resize-vertical");
      const handleWidth = handle ? handle.getBoundingClientRect().width : 0;
      return {
        min: config.min,
        max: Math.max(config.min, lower.getBoundingClientRect().width - 380 - handleWidth)
      };
    }

    const topbar = document.querySelector(".bp-topbar");
    const statusbar = document.querySelector(".bp-statusbar");
    const horizontalHandle = document.querySelector('.bp-resize-handle[data-resize-target="workspace"]');
    const reservedHeight = (topbar ? topbar.getBoundingClientRect().height : 60)
      + (statusbar ? statusbar.getBoundingClientRect().height : 36)
      + (horizontalHandle ? horizontalHandle.getBoundingClientRect().height : 7)
      + 220;
    return {
      min: config.min,
      max: Math.max(config.min, shell.getBoundingClientRect().height - reservedHeight)
    };
  }

  function currentResizeValue(target) {
    const config = resizeConfig[target];
    const element = config ? document.querySelector(config.selector) : null;
    if (!element) return 0;
    const rect = element.getBoundingClientRect();
    return Math.round(target === "workspace" ? rect.height : rect.width);
  }

  function refreshResizeHandles() {
    document.querySelectorAll(".bp-resize-handle[data-resize-target]").forEach(function (handle) {
      const target = handle.dataset.resizeTarget;
      const bounds = resizeBounds(target);
      if (!bounds) return;
      const value = currentResizeValue(target);
      handle.setAttribute("aria-valuemin", String(Math.round(bounds.min)));
      handle.setAttribute("aria-valuemax", String(Math.round(bounds.max)));
      handle.setAttribute("aria-valuenow", String(value));
      handle.setAttribute("aria-valuetext", value + " pixels");
    });
  }

  function setResizeValue(target, value) {
    const config = resizeConfig[target];
    const shell = document.querySelector(".bp-app-shell");
    const bounds = resizeBounds(target);
    if (!config || !shell || !bounds) return;
    shell.style.setProperty(config.property, Math.round(clamp(value, bounds.min, bounds.max)) + "px");
    refreshResizeHandles();
  }

  function resizeFromPointer(target, clientX, clientY, grabOffset) {
    const workspace = document.querySelector(".bp-workspace");
    const lower = document.querySelector(".bp-lower-workspace");
    if (!workspace || !lower) return;
    const workspaceRect = workspace.getBoundingClientRect();
    const lowerRect = lower.getBoundingClientRect();
    const offset = Number.isFinite(grabOffset) ? grabOffset : 0;

    if (target === "library") {
      setResizeValue(target, clientX - workspaceRect.left - offset);
    } else if (target === "inspector") {
      setResizeValue(target, workspaceRect.right - clientX - offset);
    } else if (target === "preview") {
      setResizeValue(target, clientX - lowerRect.left - offset);
    } else if (target === "workspace") {
      setResizeValue(target, clientY - workspaceRect.top - offset);
    }
  }

  function constrainResizeLayout() {
    if (window.innerWidth <= 1030) {
      refreshResizeHandles();
      return;
    }
    const shell = document.querySelector(".bp-app-shell");
    if (!shell) return;
    Object.keys(resizeConfig).forEach(function (target) {
      const config = resizeConfig[target];
      if (!shell.style.getPropertyValue(config.property)) return;
      const bounds = resizeBounds(target);
      const value = currentResizeValue(target);
      if (bounds && (value < bounds.min || value > bounds.max)) {
        shell.style.setProperty(config.property, Math.round(clamp(value, bounds.min, bounds.max)) + "px");
      }
    });
    refreshResizeHandles();
  }

  function finishResize() {
    if (!activeResize) return;
    activeResize.handle.classList.remove("is-resizing");
    document.body.classList.remove("bp-is-resizing", "bp-is-resizing-vertical", "bp-is-resizing-horizontal");
    activeResize = null;
    refreshResizeHandles();
  }

  function helpView() {
    return document.getElementById("bp-help-view");
  }

  function isHelpOpen() {
    const view = helpView();
    return Boolean(view && !view.hidden);
  }

  function updateHelpCopy(language) {
    const view = helpView();
    if (!view) return;
    const chinese = language === "zh";
    const title = document.getElementById("bp-help-title");
    const search = document.getElementById("bp-help-search-input");
    const close = view.querySelector(".bp-help-close");
    const closeLabel = view.querySelector(".bp-help-close-label");
    const noResults = document.getElementById("bp-help-no-results");
    if (title) title.textContent = chinese ? "使用手册" : "User manual";
    if (search) {
      search.placeholder = chinese ? "搜索手册" : "Search the manual";
      search.setAttribute("aria-label", search.placeholder);
    }
    if (close) {
      const label = chinese ? "关闭手册并返回工作台" : "Close manual and return to workspace";
      close.setAttribute("aria-label", label);
      close.title = label + " (Esc)";
    }
    if (closeLabel) closeLabel.textContent = chinese ? "返回工作台" : "Back to workspace";
    if (noResults) {
      const heading = noResults.querySelector("strong");
      const body = noResults.querySelector("p");
      if (heading) heading.textContent = chinese ? "没有找到相关内容" : "No matching content";
      if (body) body.textContent = chinese ? "请尝试其他关键词。" : "Try a different search term.";
    }
  }

  function setHelpActiveLink(targetId) {
    const view = helpView();
    if (!view) return;
    view.querySelectorAll('.bp-help-nav[data-help-lang="' + helpLanguage + '"] .bp-help-nav-link').forEach(function (link) {
      link.classList.toggle("is-active", link.dataset.helpTarget === targetId);
    });
  }

  function navigateHelp(targetId) {
    const target = document.getElementById(targetId);
    if (target) target.scrollIntoView({ behavior: "smooth", block: "start" });
    setHelpActiveLink(targetId);
  }

  function initializeHelpNavigation() {
    const view = helpView();
    if (!view) return;
    view.querySelectorAll(".bp-help-nav-link[data-help-target]").forEach(function (button) {
      if (button.dataset.helpNavigationBound === "true") return;
      button.dataset.helpNavigationBound = "true";
      button.addEventListener("click", function (event) {
        event.preventDefault();
        event.stopPropagation();
        navigateHelp(button.dataset.helpTarget);
      });
    });
  }

  function updateHelpActiveNav() {
    if (!isHelpOpen()) return;
    const view = helpView();
    const main = view.querySelector(".bp-help-main");
    const article = view.querySelector('.bp-help-document[data-help-lang="' + helpLanguage + '"]');
    if (!main || !article || article.classList.contains("is-search-empty")) return;
    const sections = Array.from(article.querySelectorAll(".bp-help-section:not([hidden])"));
    if (!sections.length) return;
    const threshold = main.getBoundingClientRect().top + 92;
    let current = sections[0];
    sections.forEach(function (section) {
      if (section.getBoundingClientRect().top <= threshold) current = section;
    });
    setHelpActiveLink(current.id);
  }

  function filterHelp(query) {
    const view = helpView();
    if (!view) return;
    const article = view.querySelector('.bp-help-document[data-help-lang="' + helpLanguage + '"]');
    const nav = view.querySelector('.bp-help-nav[data-help-lang="' + helpLanguage + '"]');
    const noResults = document.getElementById("bp-help-no-results");
    if (!article || !nav || !noResults) return;
    const normalized = String(query || "").trim().toLocaleLowerCase(helpLanguage === "zh" ? "zh-CN" : "en");
    let visibleCount = 0;
    article.querySelectorAll(".bp-help-section").forEach(function (section) {
      const matches = !normalized || section.textContent.toLocaleLowerCase().includes(normalized);
      section.hidden = !matches;
      if (matches) visibleCount += 1;
      const link = nav.querySelector('[data-help-target="' + section.id + '"]');
      if (link) link.hidden = !matches;
    });
    const empty = Boolean(normalized) && visibleCount === 0;
    article.classList.toggle("is-search-empty", empty);
    noResults.hidden = !empty;
    const main = view.querySelector(".bp-help-main");
    if (main) main.scrollTop = 0;
    if (!empty) {
      const first = article.querySelector(".bp-help-section:not([hidden])");
      if (first) setHelpActiveLink(first.id);
    }
  }

  function setHelpLanguage(language) {
    const view = helpView();
    if (!view || !["zh", "en"].includes(language)) return;
    helpLanguage = language;
    view.dataset.helpLanguage = language;
    view.setAttribute("lang", language === "zh" ? "zh-CN" : "en");
    view.querySelectorAll("[data-help-language]").forEach(function (button) {
      const active = button.dataset.helpLanguage === language;
      button.classList.toggle("is-active", active);
      button.setAttribute("aria-pressed", String(active));
    });
    view.querySelectorAll(".bp-help-document[data-help-lang], .bp-help-nav[data-help-lang]").forEach(function (element) {
      element.hidden = element.dataset.helpLang !== language;
    });
    view.querySelectorAll(".bp-help-document").forEach(function (article) {
      article.classList.remove("is-search-empty");
      article.querySelectorAll(".bp-help-section").forEach(function (section) { section.hidden = false; });
    });
    view.querySelectorAll(".bp-help-nav-link").forEach(function (link) { link.hidden = false; });
    const search = document.getElementById("bp-help-search-input");
    if (search) search.value = "";
    const noResults = document.getElementById("bp-help-no-results");
    if (noResults) noResults.hidden = true;
    updateHelpCopy(language);
    const main = view.querySelector(".bp-help-main");
    if (main) main.scrollTop = 0;
    const firstLink = view.querySelector('.bp-help-nav[data-help-lang="' + language + '"] .bp-help-nav-link');
    if (firstLink) setHelpActiveLink(firstLink.dataset.helpTarget);
  }

  function openHelp() {
    const view = helpView();
    if (!view || isHelpOpen()) return;
    helpLastFocus = document.activeElement;
    view.hidden = false;
    view.setAttribute("aria-hidden", "false");
    document.body.classList.add("bp-help-open");
    const trigger = document.getElementById("open-help-button");
    if (trigger) trigger.setAttribute("aria-expanded", "true");
    window.requestAnimationFrame(function () {
      const search = document.getElementById("bp-help-search-input");
      if (search) search.focus();
    });
  }

  function closeHelp() {
    const view = helpView();
    if (!view || !isHelpOpen()) return;
    view.hidden = true;
    view.setAttribute("aria-hidden", "true");
    document.body.classList.remove("bp-help-open");
    const trigger = document.getElementById("open-help-button");
    if (trigger) trigger.setAttribute("aria-expanded", "false");
    if (helpLastFocus && typeof helpLastFocus.focus === "function") helpLastFocus.focus();
    helpLastFocus = null;
  }

  function trapHelpFocus(event) {
    const view = helpView();
    if (!view) return;
    const focusable = Array.from(view.querySelectorAll('a[href], button, input, summary, [tabindex]:not([tabindex="-1"])')).filter(function (element) {
      return !element.disabled && !element.hidden && element.offsetParent !== null;
    });
    if (!focusable.length) return;
    const first = focusable[0];
    const last = focusable[focusable.length - 1];
    if (event.shiftKey && document.activeElement === first) {
      event.preventDefault();
      last.focus();
    } else if (!event.shiftKey && document.activeElement === last) {
      event.preventDefault();
      first.focus();
    }
  }

  function inputKey(payload) {
    return [payload.kind, payload.instance_id || "", payload.param || "", payload.aes_key || ""].join(":");
  }

  function scheduleInput(name, payload) {
    const key = inputKey(payload);
    if (pendingInputTimers.has(key)) window.clearTimeout(pendingInputTimers.get(key));
    pendingInputTimers.set(key, window.setTimeout(function () {
      pendingInputTimers.delete(key);
      sendInput(name, payload);
    }, 500));
  }

  function flushInput(name, payload) {
    const key = inputKey(payload);
    if (pendingInputTimers.has(key)) {
      window.clearTimeout(pendingInputTimers.get(key));
      pendingInputTimers.delete(key);
    }
    sendInput(name, payload);
  }

  function copyGeneratedCode() {
    const source = document.getElementById("generated_code_raw");
    const button = document.getElementById("copy-generated-code");
    if (!source) return;
    const text = source.value || source.textContent || "";
    const done = function () {
      if (!button) return;
      const label = button.querySelector("span");
      const previous = label ? label.textContent : "Copy";
      button.classList.add("is-copied");
      if (label) label.textContent = "Copied";
      window.setTimeout(function () {
        button.classList.remove("is-copied");
        if (label) label.textContent = previous;
      }, 1300);
    };
    if (navigator.clipboard && window.isSecureContext) {
      navigator.clipboard.writeText(text).then(done);
      return;
    }
    const fallback = document.createElement("textarea");
    fallback.value = text;
    fallback.style.position = "fixed";
    fallback.style.opacity = "0";
    document.body.appendChild(fallback);
    fallback.select();
    document.execCommand("copy");
    fallback.remove();
    done();
  }

  document.addEventListener("click", function (event) {
    if (closest(event.target, "#open-help-button")) {
      openHelp();
      return;
    }

    if (closest(event.target, ".bp-help-close")) {
      closeHelp();
      return;
    }

    const language = closest(event.target, "[data-help-language]");
    if (language) {
      setHelpLanguage(language.dataset.helpLanguage);
      const search = document.getElementById("bp-help-search-input");
      if (search) search.focus();
      return;
    }

    const filter = closest(event.target, ".bp-filter-button");
    if (filter) {
      sendInput("library_filter", { value: filter.dataset.filter });
      return;
    }

    const add = closest(event.target, ".bp-add-module");
    if (add) {
      sendInput("add_module", { module_id: add.dataset.moduleId });
      return;
    }

    const template = closest(event.target, ".bp-load-template");
    if (template) {
      sendInput("load_template", { template_id: template.dataset.templateId });
      return;
    }

    const layerAction = closest(event.target, ".bp-layer-action");
    if (layerAction) {
      sendInput("module_action", {
        action: layerAction.dataset.action,
        instance_id: layerAction.dataset.instanceId
      });
      return;
    }

    const codeLine = closest(event.target, ".bp-code-line");
    if (codeLine) {
      sendInput("select_from_code", { instance_id: codeLine.dataset.instanceId });
      return;
    }

    const tab = closest(event.target, ".bp-inspector-tab");
    if (tab) {
      sendInput("parameter_tab", { value: tab.dataset.paramTab });
      return;
    }

    const expression = closest(event.target, ".bp-expression-button");
    if (expression) {
      sendInput("expression_open", {
        instance_id: expression.dataset.instanceId,
        param: expression.dataset.param
      });
      return;
    }

    if (closest(event.target, "#copy-generated-code")) {
      copyGeneratedCode();
      return;
    }

    if (closest(event.target, "#open-project-button")) {
      const input = document.getElementById("project_file");
      if (input) input.click();
      return;
    }

    if (closest(event.target, ".bp-focus-library")) {
      const input = document.getElementById("module_search");
      if (input) {
        input.focus();
        input.scrollIntoView({ behavior: "smooth", block: "center" });
      }
      return;
    }

    if (closest(event.target, ".bp-close-inspector")) {
      const shell = document.querySelector(".bp-app-shell");
      if (shell) shell.classList.toggle("is-inspector-collapsed");
    }
  });

  document.addEventListener("change", function (event) {
    const state = closest(event.target, ".bp-param-state");
    if (state) {
      sendInput("param_change", {
        kind: "state",
        instance_id: state.dataset.instanceId,
        param: state.dataset.param,
        value: state.value
      });
      return;
    }

    const value = closest(event.target, ".bp-param-value");
    if (value) {
      flushInput("param_change", {
        kind: "value",
        instance_id: value.dataset.instanceId,
        param: value.dataset.param,
        control: value.dataset.control,
        value: value.value
      });
      return;
    }

    const aes = closest(event.target, ".bp-aes-value");
    if (aes) {
      flushInput("param_change", {
        kind: "aes",
        instance_id: aes.dataset.instanceId,
        param: aes.dataset.param,
        aes_key: aes.dataset.aesKey,
        value: aes.value
      });
      return;
    }

    const enabled = closest(event.target, ".bp-assignment-enabled");
    if (enabled) {
      sendInput("assignment_change", { kind: "enabled", value: enabled.checked });
      return;
    }

    const target = closest(event.target, ".bp-assignment-target");
    if (target) {
      flushInput("assignment_change", { kind: "target", value: target.value });
    }
  });

  document.addEventListener("input", function (event) {
    const helpSearch = closest(event.target, "#bp-help-search-input");
    if (helpSearch) {
      filterHelp(helpSearch.value);
      return;
    }

    const value = closest(event.target, ".bp-param-value");
    if (value) {
      scheduleInput("param_change", {
        kind: "value",
        instance_id: value.dataset.instanceId,
        param: value.dataset.param,
        control: value.dataset.control,
        value: value.value
      });
      return;
    }

    const aes = closest(event.target, ".bp-aes-value");
    if (aes) {
      scheduleInput("param_change", {
        kind: "aes",
        instance_id: aes.dataset.instanceId,
        param: aes.dataset.param,
        aes_key: aes.dataset.aesKey,
        value: aes.value
      });
      return;
    }

    const target = closest(event.target, ".bp-assignment-target");
    if (target) {
      scheduleInput("assignment_change", { kind: "target", value: target.value });
    }
  });

  document.addEventListener("dragstart", function (event) {
    const card = closest(event.target, ".bp-layer-card");
    if (!card) return;
    draggedCard = card;
    card.classList.add("is-dragging");
    event.dataTransfer.effectAllowed = "move";
    event.dataTransfer.setData("text/plain", card.dataset.instanceId || "");
  });

  document.addEventListener("dragover", function (event) {
    const stack = closest(event.target, ".bp-layer-stack");
    if (!stack || !draggedCard) return;
    event.preventDefault();
    const nodes = Array.from(stack.querySelectorAll(".bp-layer-node"));
    const targetNode = closest(event.target, ".bp-layer-node");
    const draggedNode = draggedCard.closest(".bp-layer-node");
    if (!targetNode || !draggedNode || targetNode === draggedNode) return;
    const targetRect = targetNode.getBoundingClientRect();
    const before = event.clientY < targetRect.top + targetRect.height / 2;
    stack.insertBefore(draggedNode, before ? targetNode : targetNode.nextSibling);
  });

  document.addEventListener("drop", function (event) {
    const stack = closest(event.target, ".bp-layer-stack");
    if (!stack || !draggedCard) return;
    event.preventDefault();
    const ids = Array.from(stack.querySelectorAll(".bp-layer-card")).map(function (card) {
      return card.dataset.instanceId;
    });
    sendInput("reorder_modules", { ids: ids });
  });

  document.addEventListener("dragend", function () {
    if (draggedCard) draggedCard.classList.remove("is-dragging");
    draggedCard = null;
  });

  document.addEventListener("pointerdown", function (event) {
    const handle = closest(event.target, ".bp-resize-handle[data-resize-target]");
    if (!handle || event.button !== 0 || window.innerWidth <= 1030) return;
    event.preventDefault();
    activeResize = {
      handle: handle,
      pointerId: event.pointerId,
      target: handle.dataset.resizeTarget,
      grabOffset: handle.dataset.resizeTarget === "inspector"
        ? handle.getBoundingClientRect().right - event.clientX
        : handle.getAttribute("aria-orientation") === "vertical"
          ? event.clientX - handle.getBoundingClientRect().left
          : event.clientY - handle.getBoundingClientRect().top
    };
    handle.classList.add("is-resizing");
    document.body.classList.add("bp-is-resizing", "bp-is-resizing-" + handle.getAttribute("aria-orientation"));
    if (typeof handle.setPointerCapture === "function") handle.setPointerCapture(event.pointerId);
  });

  document.addEventListener("pointermove", function (event) {
    if (!activeResize || event.pointerId !== activeResize.pointerId) return;
    event.preventDefault();
    resizeFromPointer(activeResize.target, event.clientX, event.clientY, activeResize.grabOffset);
  });

  document.addEventListener("pointerup", function (event) {
    if (!activeResize || event.pointerId !== activeResize.pointerId) return;
    finishResize();
  });

  document.addEventListener("pointercancel", finishResize);

  document.addEventListener("dblclick", function (event) {
    const handle = closest(event.target, ".bp-resize-handle[data-resize-target]");
    const shell = document.querySelector(".bp-app-shell");
    const config = handle ? resizeConfig[handle.dataset.resizeTarget] : null;
    if (!handle || !shell || !config || window.innerWidth <= 1030) return;
    event.preventDefault();
    shell.style.removeProperty(config.property);
    window.requestAnimationFrame(refreshResizeHandles);
  });

  document.addEventListener("keydown", function (event) {
    if (isHelpOpen()) {
      if (event.key === "Escape") {
        event.preventDefault();
        closeHelp();
        return;
      }
      if (event.key === "Tab") trapHelpFocus(event);
      return;
    }

    const resizeHandle = closest(event.target, ".bp-resize-handle[data-resize-target]");
    if (resizeHandle && window.innerWidth > 1030) {
      const orientation = resizeHandle.getAttribute("aria-orientation");
      const horizontalKey = orientation === "vertical" && (event.key === "ArrowLeft" || event.key === "ArrowRight");
      const verticalKey = orientation === "horizontal" && (event.key === "ArrowUp" || event.key === "ArrowDown");
      if (horizontalKey || verticalKey) {
        event.preventDefault();
        const delta = (event.key === "ArrowRight" || event.key === "ArrowDown") ? 16 : -16;
        const target = resizeHandle.dataset.resizeTarget;
        const direction = target === "inspector" ? -1 : 1;
        setResizeValue(target, currentResizeValue(target) + delta * direction);
        return;
      }
    }

    const modifier = event.ctrlKey || event.metaKey;
    if (!modifier) return;
    const key = event.key.toLowerCase();
    if (key === "enter") {
      event.preventDefault();
      const run = document.getElementById("run_preview");
      if (run) run.click();
      return;
    }
    if (key === "z" && !event.shiftKey) {
      event.preventDefault();
      const undo = document.getElementById("undo");
      if (undo) undo.click();
      return;
    }
    if (key === "y" || (key === "z" && event.shiftKey)) {
      event.preventDefault();
      const redo = document.getElementById("redo");
      if (redo) redo.click();
    }
  });

  window.BioPlotBlocks = {
    copyGeneratedCode: copyGeneratedCode,
    openHelp: openHelp,
    closeHelp: closeHelp,
    setHelpLanguage: setHelpLanguage,
    sendInput: sendInput,
    refreshResizeHandles: refreshResizeHandles
  };

  document.addEventListener("scroll", function (event) {
    if (!closest(event.target, ".bp-help-main") || helpScrollFrame) return;
    helpScrollFrame = window.requestAnimationFrame(function () {
      helpScrollFrame = null;
      updateHelpActiveNav();
    });
  }, true);

  function initializeInterface() {
    refreshResizeHandles();
    setHelpLanguage("zh");
    initializeHelpNavigation();
  }

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeInterface, { once: true });
  } else {
    window.requestAnimationFrame(initializeInterface);
  }
  window.addEventListener("resize", constrainResizeLayout);
  document.addEventListener("shiny:connected", refreshResizeHandles);
})();
