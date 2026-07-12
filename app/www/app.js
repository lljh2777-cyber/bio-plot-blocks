(function () {
  "use strict";

  let draggedCard = null;
  let dragScrollStack = null;
  let dragAutoScrollFrame = null;
  let dragAutoScrollSpeed = 0;
  let dragPointerX = 0;
  let dragPointerY = 0;
  let activeResize = null;
  let activePickerScroll = null;
  let suppressPickerClickUntil = 0;
  let pickerCloseTimer = null;
  let pickerPositionFrame = null;
  let helpLastFocus = null;
  let helpLanguage = "zh";
  let helpScrollFrame = null;
  let focusedTextInput = null;
  let textInputRestoreActive = false;
  let textInputObserver = null;
  let projectPersistenceStarted = false;
  let projectPersistenceReady = false;
  let projectPersistenceTimer = null;
  let projectPersistenceRetry = null;
  let projectPersistenceObserver = null;
  let previewViewHandlerStarted = false;
  let parameterValueHandlerStarted = false;
  let visualChartHandlerStarted = false;
  const projectStorageKey = "bioplotblocks.project.v0.2";
  const interfaceModeStorageKey = "bioplotblocks.interface-mode.v1";
  const pendingInputTimers = new Map();
  const resizeConfig = {
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

  function storedInterfaceMode() {
    try {
      return window.localStorage.getItem(interfaceModeStorageKey) === "advanced" ? "advanced" : "visual";
    } catch (error) {
      return "visual";
    }
  }

  function setInterfaceMode(mode, persist, notify) {
    const next = mode === "advanced" ? "advanced" : "visual";
    const root = document.documentElement;
    root.classList.toggle("bp-interface-visual", next === "visual");
    root.classList.toggle("bp-interface-advanced", next === "advanced");
    document.body.classList.toggle("bp-mode-visual", next === "visual");
    document.body.classList.toggle("bp-mode-advanced", next === "advanced");
    document.querySelectorAll(".bp-mode-button[data-interface-mode]").forEach(function (button) {
      const active = button.dataset.interfaceMode === next;
      button.classList.toggle("is-active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });
    if (persist !== false) {
      try {
        window.localStorage.setItem(interfaceModeStorageKey, next);
      } catch (error) {
        // The interface still works when storage is unavailable.
      }
    }
    if (notify !== false) sendInput("interface_mode", { value: next });
    window.requestAnimationFrame(function () {
      constrainResizeLayout();
      window.dispatchEvent(new Event("resize"));
      if (window.jQuery) {
        const shown = next === "visual" ? ".bp-visual-surface" : ".bp-advanced-surface";
        const hidden = next === "visual" ? ".bp-advanced-surface" : ".bp-visual-surface";
        window.jQuery(hidden).trigger("hidden");
        window.jQuery(shown).trigger("shown");
      }
    });
  }

  function setVisualStep(sectionId) {
    document.querySelectorAll(".bp-visual-step[data-visual-section]").forEach(function (button) {
      button.classList.toggle("is-active", button.dataset.visualSection === sectionId);
    });
  }

  function setVisualChartType(chartType) {
    const next = chartType === "volcano" ? "volcano" : "scatter";
    document.body.dataset.visualChartType = next;
    document.querySelectorAll(".bp-visual-chart-card[data-chart-type]").forEach(function (button) {
      const active = button.dataset.chartType === next;
      button.classList.toggle("is-active", active);
      button.setAttribute("aria-pressed", active ? "true" : "false");
    });
    document.querySelectorAll(".bp-volcano-only").forEach(function (element) {
      element.hidden = next !== "volcano";
    });
    document.querySelectorAll(".bp-scatter-only").forEach(function (element) {
      element.hidden = next !== "scatter";
    });
    const labels = next === "volcano"
      ? { x: "倍数变化字段 *", y: "显著性字段 *", color: "已有状态分组（可选）" }
      : { x: "X 轴字段 *", y: "Y 轴字段 *", color: "颜色/状态分组" };
    Object.keys(labels).forEach(function (field) {
      const label = document.querySelector('.bp-visual-field-control[data-visual-field="' + field + '"] label');
      if (label) label.textContent = labels[field];
    });
    const eyebrow = document.querySelector(".bp-visual-builder-heading .bp-visual-eyebrow");
    if (eyebrow) eyebrow.textContent = next === "volcano" ? "VOLCANO BUILDER · 火山图向导" : "SCATTER BUILDER · 散点图向导";
  }

  function updateVisualColorSwatch(input) {
    if (!input) return;
    const control = input.closest(".bp-visual-color-control");
    if (!control) return;
    const value = String(input.value || "").trim();
    if (/^#[0-9a-f]{6}$/i.test(value)) control.style.setProperty("--bp-current-color", value);
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

    if (target === "inspector") {
      const inspector = document.querySelector(".bp-inspector-panel");
      if (!inspector) return null;
      const handleSpace = Array.from(workspace.querySelectorAll(".bp-resize-vertical")).reduce(function (sum, handle) {
        return sum + handle.getBoundingClientRect().width;
      }, 0);
      return {
        min: config.min,
        max: Math.max(config.min, workspace.getBoundingClientRect().width - 520 - handleSpace)
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
    positionOpenModulePickerMenus();
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

    if (target === "inspector") {
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

  function finishPickerScroll(pointerId) {
    if (!activePickerScroll || (pointerId != null && pointerId !== activePickerScroll.pointerId)) return false;
    activePickerScroll.scroller.classList.remove("is-dragging");
    if (activePickerScroll.moved) suppressPickerClickUntil = performance.now() + 140;
    activePickerScroll = null;
    return true;
  }

  function ensurePickerTriggerVisible(group) {
    if (!group) return;
    const scroller = group.closest("#module_picker");
    const trigger = group.querySelector(".bp-picker-trigger");
    if (!scroller || !trigger || scroller.scrollWidth <= scroller.clientWidth) return;
    const scrollerRect = scroller.getBoundingClientRect();
    const triggerRect = trigger.getBoundingClientRect();
    const padding = 5;
    if (triggerRect.right > scrollerRect.right - padding) {
      scroller.scrollLeft += triggerRect.right - scrollerRect.right + padding;
    } else if (triggerRect.left < scrollerRect.left + padding) {
      scroller.scrollLeft -= scrollerRect.left - triggerRect.left + padding;
    }
  }

  function positionModulePickerMenu(group) {
    if (!group) return;
    const menu = group.querySelector(".bp-picker-menu");
    const picker = document.querySelector(".bp-module-picker");
    if (!menu || !picker) return;
    if (window.innerWidth <= 760) {
      menu.style.removeProperty("--bp-picker-menu-top");
      menu.style.removeProperty("--bp-picker-menu-left");
      menu.style.removeProperty("--bp-picker-menu-width");
      return;
    }
    const pickerRect = picker.getBoundingClientRect();
    const gutter = 18;
    const width = Math.max(260, Math.min(390, pickerRect.width - gutter * 2));
    const alignRight = ["structure", "scales", "templates"].includes(group.dataset.pickerGroup);
    const left = alignRight ? pickerRect.right - gutter - width : pickerRect.left + gutter;
    menu.style.setProperty("--bp-picker-menu-top", Math.round(pickerRect.bottom + 5) + "px");
    menu.style.setProperty("--bp-picker-menu-left", Math.round(left) + "px");
    menu.style.setProperty("--bp-picker-menu-width", Math.round(width) + "px");
  }

  function positionOpenModulePickerMenus() {
    document.querySelectorAll(".bp-picker-group.is-open").forEach(positionModulePickerMenu);
  }

  function clearPickerCloseTimer() {
    if (pickerCloseTimer == null) return;
    window.clearTimeout(pickerCloseTimer);
    pickerCloseTimer = null;
  }

  function closeModulePickers(except) {
    clearPickerCloseTimer();
    document.querySelectorAll(".bp-picker-group").forEach(function (group) {
      if (group === except) return;
      group.classList.remove("is-open", "is-pinned");
      const trigger = group.querySelector(".bp-picker-trigger");
      const menu = group.querySelector(".bp-picker-menu");
      if (trigger) trigger.setAttribute("aria-expanded", "false");
      if (menu) menu.hidden = true;
    });
  }

  function openModulePicker(group, pinned) {
    if (!group) return;
    clearPickerCloseTimer();
    closeModulePickers(group);
    ensurePickerTriggerVisible(group);
    const trigger = group.querySelector(".bp-picker-trigger");
    const menu = group.querySelector(".bp-picker-menu");
    group.classList.add("is-open");
    group.classList.toggle("is-pinned", Boolean(pinned));
    if (trigger) trigger.setAttribute("aria-expanded", "true");
    if (menu) {
      positionModulePickerMenu(group);
      menu.hidden = false;
    }
  }

  function closeModulePickerGroup(group) {
    if (!group) return;
    group.classList.remove("is-open", "is-pinned");
    const trigger = group.querySelector(".bp-picker-trigger");
    const menu = group.querySelector(".bp-picker-menu");
    if (trigger) trigger.setAttribute("aria-expanded", "false");
    if (menu) menu.hidden = true;
  }

  function scheduleModulePickerClose(group) {
    clearPickerCloseTimer();
    if (!group || group.classList.contains("is-pinned")) return;
    pickerCloseTimer = window.setTimeout(function () {
      closeModulePickerGroup(group);
      pickerCloseTimer = null;
    }, 220);
  }

  function filterModulePicker(input) {
    const group = closest(input, ".bp-picker-group");
    if (!group) return;
    const query = input.value.trim().toLowerCase();
    let visibleCount = 0;
    group.querySelectorAll(".bp-picker-list .bp-library-row").forEach(function (row) {
      const matches = !query || (row.dataset.searchText || "").includes(query);
      row.hidden = !matches;
      if (matches) visibleCount += 1;
    });
    const empty = group.querySelector(".bp-picker-empty");
    if (empty) empty.hidden = visibleCount > 0;
  }

  function reorderDraggedAtPoint(stack, clientY) {
    if (!draggedCard || !stack) return;
    const draggedNode = draggedCard.closest(".bp-layer-node");
    if (!draggedNode) return;
    const nodes = Array.from(stack.querySelectorAll(".bp-layer-node")).filter(function (node) {
      return node !== draggedNode;
    });
    if (!nodes.length) return;

    let target = null;
    let insertBefore = false;
    for (const node of nodes) {
      const rect = node.getBoundingClientRect();
      if (clientY < rect.top + rect.height / 2) {
        target = node;
        insertBefore = true;
        break;
      }
    }
    if (!target) target = nodes[nodes.length - 1];
    const reference = insertBefore ? target : target.nextSibling;
    if (reference !== draggedNode && draggedNode.nextSibling !== reference) {
      stack.insertBefore(draggedNode, reference);
    }
  }

  function stopDragAutoScrollLoop() {
    if (dragAutoScrollFrame != null) window.cancelAnimationFrame(dragAutoScrollFrame);
    dragAutoScrollFrame = null;
    dragAutoScrollSpeed = 0;
    if (dragScrollStack) {
      dragScrollStack.classList.remove("is-auto-scrolling-up", "is-auto-scrolling-down");
    }
  }

  function clearDragScrollState() {
    stopDragAutoScrollLoop();
    if (dragScrollStack) dragScrollStack.classList.remove("is-drag-active");
    dragScrollStack = null;
    dragPointerX = 0;
    dragPointerY = 0;
  }

  function runDragAutoScroll() {
    dragAutoScrollFrame = null;
    if (!draggedCard || !dragScrollStack || dragAutoScrollSpeed === 0) return;
    const before = dragScrollStack.scrollTop;
    dragScrollStack.scrollTop += dragAutoScrollSpeed;
    if (dragScrollStack.scrollTop === before) {
      stopDragAutoScrollLoop();
      return;
    }
    reorderDraggedAtPoint(dragScrollStack, dragPointerY);
    dragAutoScrollFrame = window.requestAnimationFrame(runDragAutoScroll);
  }

  function updateDragAutoScroll(stack, clientX, clientY) {
    if (!stack) return;
    if (dragScrollStack && dragScrollStack !== stack) {
      dragScrollStack.classList.remove("is-drag-active", "is-auto-scrolling-up", "is-auto-scrolling-down");
    }
    dragScrollStack = stack;
    dragScrollStack.classList.add("is-drag-active");
    dragPointerX = clientX;
    dragPointerY = clientY;

    const rect = stack.getBoundingClientRect();
    const edge = Math.min(72, rect.height * 0.22);
    const outside = 56;
    let speed = 0;
    const withinHorizontalRange = clientX >= rect.left - 72 && clientX <= rect.right + 72;
    if (withinHorizontalRange && clientY >= rect.top - outside && clientY < rect.top + edge) {
      const strength = clamp((rect.top + edge - clientY) / (edge + outside), 0, 1);
      speed = -(4 + 18 * strength);
    } else if (withinHorizontalRange && clientY <= rect.bottom + outside && clientY > rect.bottom - edge) {
      const strength = clamp((clientY - (rect.bottom - edge)) / (edge + outside), 0, 1);
      speed = 4 + 18 * strength;
    }

    dragAutoScrollSpeed = speed;
    stack.classList.toggle("is-auto-scrolling-up", speed < 0);
    stack.classList.toggle("is-auto-scrolling-down", speed > 0);
    if (speed === 0) {
      if (dragAutoScrollFrame != null) window.cancelAnimationFrame(dragAutoScrollFrame);
      dragAutoScrollFrame = null;
      return;
    }
    if (dragAutoScrollFrame == null) dragAutoScrollFrame = window.requestAnimationFrame(runDragAutoScroll);
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

  function readStoredProject() {
    try {
      return window.localStorage.getItem(projectStorageKey);
    } catch (error) {
      return null;
    }
  }

  function removeStoredProject() {
    try {
      window.localStorage.removeItem(projectStorageKey);
    } catch (error) {
      // Storage can be unavailable in privacy-restricted browser contexts.
    }
  }

  function persistProjectState() {
    projectPersistenceTimer = null;
    if (!projectPersistenceReady) return;
    const transport = document.getElementById("project_state_raw");
    const value = transport ? (transport.value || transport.textContent || "") : "";
    if (!value) return;
    try {
      window.localStorage.setItem(projectStorageKey, value);
      document.documentElement.dataset.bpPersistence = "saved";
    } catch (error) {
      document.documentElement.dataset.bpPersistence = "unavailable";
      // The editor remains usable when storage is disabled or its quota is full.
    }
  }

  function scheduleProjectPersistence() {
    if (!projectPersistenceReady) return;
    if (projectPersistenceTimer) window.clearTimeout(projectPersistenceTimer);
    projectPersistenceTimer = window.setTimeout(persistProjectState, 120);
  }

  function initializeProjectPersistence() {
    if (projectPersistenceStarted) return;
    const socket = window.Shiny && window.Shiny.shinyapp ? window.Shiny.shinyapp.$socket : null;
    if (!socket || socket.readyState !== 1) {
      document.documentElement.dataset.bpPersistence = "waiting";
      if (!projectPersistenceRetry) {
        projectPersistenceRetry = window.setTimeout(function () {
          projectPersistenceRetry = null;
          initializeProjectPersistence();
        }, 50);
      }
      return;
    }
    projectPersistenceStarted = true;
    document.documentElement.dataset.bpPersistence = "starting";
    window.Shiny.addCustomMessageHandler("bp_project_restore_status", function (message) {
      if (!message || !message.ok) removeStoredProject();
      projectPersistenceReady = true;
      document.documentElement.dataset.bpPersistence = message && message.ok ? "restored" : "reset";
      document.documentElement.classList.remove("bp-restoring-project");
      scheduleProjectPersistence();
    });
    projectPersistenceObserver = new MutationObserver(scheduleProjectPersistence);
    projectPersistenceObserver.observe(document.body, { childList: true, subtree: true });

    const saved = readStoredProject();
    if (saved) {
      document.documentElement.classList.add("bp-restoring-project");
      sendInput("restore_project", { json: saved });
      return;
    }
    projectPersistenceReady = true;
    document.documentElement.dataset.bpPersistence = "ready";
    scheduleProjectPersistence();
  }

  function isEditableTextInput(element) {
    if (!(element instanceof HTMLInputElement) && !(element instanceof HTMLTextAreaElement)) return false;
    if (element.disabled || element.readOnly) return false;
    if (element instanceof HTMLTextAreaElement) return true;
    return !["button", "checkbox", "color", "file", "hidden", "image", "radio", "range", "reset", "submit"].includes(element.type);
  }

  function textInputIdentity(element) {
    if (!isEditableTextInput(element)) return null;
    if (element.id) return { kind: "id", id: element.id };
    if (element.classList.contains("bp-param-value")) {
      return {
        kind: "parameter",
        instanceId: element.dataset.instanceId || "",
        parameter: element.dataset.param || ""
      };
    }
    if (element.classList.contains("bp-aes-value")) {
      return {
        kind: "aesthetic",
        instanceId: element.dataset.instanceId || "",
        parameter: element.dataset.param || "",
        aesthetic: element.dataset.aesKey || ""
      };
    }
    if (element.classList.contains("bp-assignment-target")) return { kind: "assignment" };
    return null;
  }

  function sameTextInputIdentity(left, right) {
    return Boolean(left && right) && left.kind === right.kind &&
      left.id === right.id && left.instanceId === right.instanceId &&
      left.parameter === right.parameter && left.aesthetic === right.aesthetic;
  }

  function findTextInput(identity) {
    if (!identity) return null;
    if (identity.kind === "id") return document.getElementById(identity.id);
    if (identity.kind === "assignment") return document.querySelector(".bp-assignment-target");
    const selector = identity.kind === "aesthetic" ? ".bp-aes-value" : ".bp-param-value";
    return Array.from(document.querySelectorAll(selector)).find(function (element) {
      if ((element.dataset.instanceId || "") !== identity.instanceId ||
          (element.dataset.param || "") !== identity.parameter) return false;
      return identity.kind !== "aesthetic" || (element.dataset.aesKey || "") === identity.aesthetic;
    }) || null;
  }

  function rememberTextInput(element) {
    const identity = textInputIdentity(element);
    if (!identity) return;
    let selectionStart = null;
    let selectionEnd = null;
    let selectionDirection = "none";
    try {
      selectionStart = element.selectionStart;
      selectionEnd = element.selectionEnd;
      selectionDirection = element.selectionDirection || "none";
    } catch (error) {
      selectionStart = null;
      selectionEnd = null;
    }
    focusedTextInput = {
      identity: identity,
      value: element.value,
      selectionStart: selectionStart,
      selectionEnd: selectionEnd,
      selectionDirection: selectionDirection,
      scrollLeft: element.scrollLeft,
      scrollTop: element.scrollTop
    };
  }

  function scheduleTextInputRestore() {
    if (!focusedTextInput || textInputRestoreActive) return;
    textInputRestoreActive = true;
    try {
      const snapshot = focusedTextInput;
      if (!snapshot) return;
      const active = document.activeElement;
      if (isEditableTextInput(active)) {
        if (sameTextInputIdentity(textInputIdentity(active), snapshot.identity)) rememberTextInput(active);
        return;
      }
      if (active && active !== document.body && active !== document.documentElement) return;
      const replacement = findTextInput(snapshot.identity);
      if (!isEditableTextInput(replacement)) return;
      const parentDetails = replacement.closest("details");
      if (parentDetails) parentDetails.open = true;
      const pageX = window.scrollX;
      const pageY = window.scrollY;
      if (replacement.value !== snapshot.value) replacement.value = snapshot.value;
      replacement.focus({ preventScroll: true });
      if (snapshot.selectionStart !== null && typeof replacement.setSelectionRange === "function") {
        const end = replacement.value.length;
        try {
          replacement.setSelectionRange(
            Math.min(snapshot.selectionStart, end),
            Math.min(snapshot.selectionEnd, end),
            snapshot.selectionDirection
          );
        } catch (error) {
          // Number-like inputs do not expose a text selection API.
        }
      }
      replacement.scrollLeft = snapshot.scrollLeft;
      replacement.scrollTop = snapshot.scrollTop;
      if (window.scrollX !== pageX || window.scrollY !== pageY) window.scrollTo(pageX, pageY);
    } finally {
      textInputRestoreActive = false;
    }
  }

  function initializeTextInputContinuity() {
    if (textInputObserver || !document.body) return;
    document.addEventListener("focusin", function (event) {
      if (isEditableTextInput(event.target)) rememberTextInput(event.target);
    }, true);
    document.addEventListener("input", function (event) {
      if (isEditableTextInput(event.target)) rememberTextInput(event.target);
    }, true);
    ["keyup", "mouseup", "select"].forEach(function (eventName) {
      document.addEventListener(eventName, function () {
        if (isEditableTextInput(document.activeElement)) rememberTextInput(document.activeElement);
      }, true);
    });
    document.addEventListener("focusout", function (event) {
      if (!isEditableTextInput(event.target)) return;
      const previous = event.target;
      window.setTimeout(function () {
        if (!previous.isConnected) {
          scheduleTextInputRestore();
          return;
        }
        const active = document.activeElement;
        if (isEditableTextInput(active)) rememberTextInput(active);
        else focusedTextInput = null;
      }, 0);
    }, true);
    textInputObserver = new MutationObserver(scheduleTextInputRestore);
    textInputObserver.observe(document.body, { childList: true, subtree: true });
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

  function setPreviewView(view, focusButton) {
    if (view !== "plot" && view !== "data") return;
    document.querySelectorAll(".bp-preview-view-button[data-preview-view]").forEach(function (button) {
      const active = button.dataset.previewView === view;
      button.classList.toggle("is-active", active);
      button.setAttribute("aria-selected", active ? "true" : "false");
      button.tabIndex = active ? 0 : -1;
      if (active && focusButton) button.focus();
    });
    const plot = document.getElementById("preview_plot_view");
    const data = document.getElementById("preview_data_view");
    if (plot) plot.hidden = view !== "plot";
    if (data) data.hidden = view !== "data";
    document.querySelectorAll(".bp-plot-preview-control").forEach(function (control) {
      control.hidden = view !== "plot";
    });
  }

  function initializePreviewViewHandler() {
    if (previewViewHandlerStarted || !window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") return;
    previewViewHandlerStarted = true;
    window.Shiny.addCustomMessageHandler("bp_set_preview_view", function (message) {
      setPreviewView(message && message.view === "data" ? "data" : "plot", false);
    });
  }

  function initializeParameterValueHandler() {
    if (parameterValueHandlerStarted || !window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") return;
    parameterValueHandlerStarted = true;
    window.Shiny.addCustomMessageHandler("bp_restore_parameter_value", function (message) {
      if (!message || !message.instance_id || !message.param) return;
      const inputs = Array.from(document.querySelectorAll(".bp-param-value"));
      const input = inputs.find(function (candidate) {
        return candidate.dataset.instanceId === message.instance_id && candidate.dataset.param === message.param;
      });
      if (!input) return;
      const payload = { kind: "value", instance_id: message.instance_id, param: message.param };
      const key = inputKey(payload);
      if (pendingInputTimers.has(key)) {
        window.clearTimeout(pendingInputTimers.get(key));
        pendingInputTimers.delete(key);
      }
      input.value = message.value == null ? "" : String(message.value);
      rememberTextInput(input);
    });
  }

  function initializeVisualChartHandler() {
    if (visualChartHandlerStarted || !window.Shiny || typeof window.Shiny.addCustomMessageHandler !== "function") return;
    visualChartHandlerStarted = true;
    window.Shiny.addCustomMessageHandler("bp_visual_chart_type", function (message) {
      setVisualChartType(message && message.value === "volcano" ? "volcano" : "scatter");
    });
  }

  document.addEventListener("click", function (event) {
    const modeButton = closest(event.target, ".bp-mode-button[data-interface-mode]");
    if (modeButton) {
      setInterfaceMode(modeButton.dataset.interfaceMode, true, true);
      return;
    }

    const chartCard = closest(event.target, ".bp-visual-chart-card[data-chart-type]");
    if (chartCard && !chartCard.disabled && !chartCard.classList.contains("is-disabled")) {
      setVisualChartType(chartCard.dataset.chartType);
    }

    const visualStep = closest(event.target, ".bp-visual-step[data-visual-section]");
    if (visualStep) {
      const sectionId = visualStep.dataset.visualSection;
      const section = document.getElementById(sectionId);
      if (section) section.scrollIntoView({ behavior: "smooth", block: "start" });
      setVisualStep(sectionId);
      return;
    }

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

    if (closest(event.target, "#module_picker") && performance.now() < suppressPickerClickUntil) {
      event.preventDefault();
      return;
    }

    const pickerTrigger = closest(event.target, ".bp-picker-trigger");
    if (pickerTrigger) {
      const group = pickerTrigger.closest(".bp-picker-group");
      const shouldClose = group && group.classList.contains("is-open") && group.classList.contains("is-pinned");
      if (shouldClose) {
        closeModulePickerGroup(group);
      } else {
        openModulePicker(group, true);
      }
      return;
    }

    const add = closest(event.target, ".bp-add-module");
    if (add) {
      sendInput("add_module", { module_id: add.dataset.moduleId });
      closeModulePickers();
      return;
    }

    const template = closest(event.target, ".bp-load-template");
    if (template) {
      sendInput("load_template", { template_id: template.dataset.templateId });
      closeModulePickers();
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

    const previewView = closest(event.target, ".bp-preview-view-button[data-preview-view]");
    if (previewView) {
      setPreviewView(previewView.dataset.previewView, false);
      return;
    }

    const aesSuggestion = closest(event.target, ".bp-aes-suggestion-button[data-aes-input-id]");
    if (aesSuggestion) {
      const input = document.getElementById(aesSuggestion.dataset.aesInputId);
      if (!input) return;
      input.focus();
      if (typeof input.showPicker === "function") {
        try {
          input.showPicker();
        } catch (error) {
          input.click();
        }
      } else {
        input.click();
      }
      return;
    }

    const dataSourceAction = closest(event.target, ".bp-data-source-action[data-source-id][data-action]");
    if (dataSourceAction) {
      sendInput("data_source_action", {
        source_id: dataSourceAction.dataset.sourceId,
        action: dataSourceAction.dataset.action
      });
      return;
    }

    if (closest(event.target, "#open-project-button")) {
      const input = document.getElementById("project_file");
      if (input) input.click();
      return;
    }

    if (closest(event.target, ".bp-close-inspector")) {
      const shell = document.querySelector(".bp-app-shell");
      if (shell) shell.classList.toggle("is-inspector-collapsed");
    }
  });

  document.addEventListener("click", function (event) {
    if (!closest(event.target, ".bp-picker-group")) closeModulePickers();
  });

  document.addEventListener("mouseover", function (event) {
    const group = closest(event.target, ".bp-picker-group");
    if (!group || (event.relatedTarget && group.contains(event.relatedTarget))) return;
    openModulePicker(group, group.classList.contains("is-pinned"));
  });

  document.addEventListener("mouseout", function (event) {
    const group = closest(event.target, ".bp-picker-group");
    if (!group || (event.relatedTarget && group.contains(event.relatedTarget))) return;
    scheduleModulePickerClose(group);
  });

  document.addEventListener("focusin", function (event) {
    const group = closest(event.target, ".bp-picker-group");
    if (group) openModulePicker(group, group.classList.contains("is-pinned"));
  });

  document.addEventListener("focusout", function (event) {
    const group = closest(event.target, ".bp-picker-group");
    if (!group || (event.relatedTarget && group.contains(event.relatedTarget))) return;
    scheduleModulePickerClose(group);
  });

  document.addEventListener("keydown", function (event) {
    const group = closest(event.target, ".bp-picker-group");
    if (!group || event.key !== "Escape" || !group.classList.contains("is-open")) return;
    event.preventDefault();
    const trigger = group.querySelector(".bp-picker-trigger");
    if (trigger) trigger.focus();
    closeModulePickerGroup(group);
  }, true);

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
    const visualColor = closest(event.target, "#visual_point_color, #visual_reference_color");
    if (visualColor) updateVisualColorSwatch(visualColor);

    const moduleSearch = closest(event.target, "#module_search");
    if (moduleSearch) {
      filterModulePicker(moduleSearch);
      return;
    }

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
    dragScrollStack = card.closest(".bp-layer-stack");
    if (dragScrollStack) dragScrollStack.classList.add("is-drag-active");
    dragPointerX = event.clientX;
    dragPointerY = event.clientY;
    card.classList.add("is-dragging");
    event.dataTransfer.effectAllowed = "move";
    event.dataTransfer.setData("text/plain", card.dataset.instanceId || "");
  });

  document.addEventListener("dragover", function (event) {
    if (!draggedCard) return;
    const directStack = closest(event.target, ".bp-layer-stack");
    const stack = directStack || dragScrollStack || draggedCard.closest(".bp-layer-stack");
    if (!stack) return;
    if (directStack) event.preventDefault();
    updateDragAutoScroll(stack, event.clientX, event.clientY);
    reorderDraggedAtPoint(stack, event.clientY);
  });

  document.addEventListener("drop", function (event) {
    if (!draggedCard) return;
    const directStack = closest(event.target, ".bp-layer-stack");
    const stack = directStack || dragScrollStack;
    if (!stack) return;
    const rect = stack.getBoundingClientRect();
    const withinDropZone = event.clientX >= rect.left && event.clientX <= rect.right
      && event.clientY >= rect.top - 32 && event.clientY <= rect.bottom + 32;
    if (!withinDropZone) return;
    event.preventDefault();
    reorderDraggedAtPoint(stack, event.clientY);
    const ids = Array.from(stack.querySelectorAll(".bp-layer-card")).map(function (card) {
      return card.dataset.instanceId;
    });
    clearDragScrollState();
    sendInput("reorder_modules", { ids: ids });
  });

  document.addEventListener("dragend", function () {
    if (draggedCard) draggedCard.classList.remove("is-dragging");
    clearDragScrollState();
    draggedCard = null;
  });

  document.addEventListener("wheel", function (event) {
    if (!draggedCard) return;
    const stack = closest(event.target, ".bp-layer-stack") || dragScrollStack || draggedCard.closest(".bp-layer-stack");
    if (!stack) return;
    const rawDelta = event.deltaY || event.deltaX;
    if (!rawDelta) return;
    const multiplier = event.deltaMode === 1 ? 24 : event.deltaMode === 2 ? stack.clientHeight : 1;
    const delta = rawDelta * multiplier;
    const maximum = stack.scrollHeight - stack.clientHeight;
    const canScroll = delta > 0 ? stack.scrollTop < maximum : stack.scrollTop > 0;
    if (!canScroll) return;
    event.preventDefault();
    stack.scrollTop += delta;
    dragScrollStack = stack;
    if (event.clientX || event.clientY) {
      dragPointerX = event.clientX;
      dragPointerY = event.clientY;
    }
    reorderDraggedAtPoint(stack, dragPointerY || stack.getBoundingClientRect().top + stack.clientHeight / 2);
  }, { passive: false, capture: true });

  document.addEventListener("wheel", function (event) {
    if (event.defaultPrevented || closest(event.target, ".bp-picker-menu")) return;
    const scroller = closest(event.target, "#module_picker");
    if (!scroller || scroller.scrollWidth <= scroller.clientWidth) return;
    const rawDelta = Math.abs(event.deltaX) > Math.abs(event.deltaY) ? event.deltaX : event.deltaY;
    if (!rawDelta) return;
    const multiplier = event.deltaMode === 1 ? 24 : event.deltaMode === 2 ? scroller.clientWidth : 1;
    const delta = rawDelta * multiplier;
    const maximum = scroller.scrollWidth - scroller.clientWidth;
    const canScroll = delta > 0 ? scroller.scrollLeft < maximum : scroller.scrollLeft > 0;
    if (!canScroll) return;
    event.preventDefault();
    scroller.scrollLeft += delta;
  }, { passive: false });

  document.addEventListener("pointerdown", function (event) {
    const pickerScroller = closest(event.target, "#module_picker");
    if (pickerScroller && !closest(event.target, ".bp-picker-menu") && pickerScroller.scrollWidth > pickerScroller.clientWidth) {
      if (event.pointerType === "touch" || event.button !== 0) return;
      activePickerScroll = {
        scroller: pickerScroller,
        pointerId: event.pointerId,
        startX: event.clientX,
        startScrollLeft: pickerScroller.scrollLeft,
        moved: false
      };
      return;
    }

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
    if (activePickerScroll && event.pointerId === activePickerScroll.pointerId) {
      const delta = event.clientX - activePickerScroll.startX;
      if (!activePickerScroll.moved && Math.abs(delta) > 4) {
        activePickerScroll.moved = true;
        activePickerScroll.scroller.classList.add("is-dragging");
        closeModulePickers();
        if (typeof activePickerScroll.scroller.setPointerCapture === "function") {
          activePickerScroll.scroller.setPointerCapture(event.pointerId);
        }
      }
      if (activePickerScroll.moved) {
        event.preventDefault();
        activePickerScroll.scroller.scrollLeft = activePickerScroll.startScrollLeft - delta;
      }
      return;
    }

    if (!activeResize || event.pointerId !== activeResize.pointerId) return;
    event.preventDefault();
    resizeFromPointer(activeResize.target, event.clientX, event.clientY, activeResize.grabOffset);
  });

  document.addEventListener("pointerup", function (event) {
    if (finishPickerScroll(event.pointerId)) return;
    if (!activeResize || event.pointerId !== activeResize.pointerId) return;
    finishResize();
  });

  document.addEventListener("pointercancel", function (event) {
    finishPickerScroll(event.pointerId);
    finishResize();
  });

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

    const pickerTrigger = closest(event.target, ".bp-picker-trigger");
    if (pickerTrigger && event.key === "ArrowDown") {
      event.preventDefault();
      openModulePicker(pickerTrigger.closest(".bp-picker-group"), true);
      const firstOption = pickerTrigger.closest(".bp-picker-group").querySelector(".bp-picker-list .bp-library-row:not([hidden])");
      if (firstOption) firstOption.focus();
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

    const previewView = closest(event.target, ".bp-preview-view-button[data-preview-view]");
    if (previewView && (event.key === "ArrowLeft" || event.key === "ArrowRight")) {
      event.preventDefault();
      setPreviewView(previewView.dataset.previewView === "plot" ? "data" : "plot", true);
      return;
    }

    const modifier = event.ctrlKey || event.metaKey;
    if (!modifier) return;
    const key = event.key.toLowerCase();
    if (key === "enter") {
      event.preventDefault();
      const run = document.documentElement.classList.contains("bp-interface-visual")
        ? document.getElementById("visual_run_preview")
        : document.getElementById("run_preview");
      if (run) run.click();
      return;
    }
    if (key === "z" && !event.shiftKey) {
      event.preventDefault();
      const undo = document.documentElement.classList.contains("bp-interface-visual")
        ? document.getElementById("visual_undo")
        : document.getElementById("undo");
      if (undo) undo.click();
      return;
    }
    if (key === "y" || (key === "z" && event.shiftKey)) {
      event.preventDefault();
      const redo = document.documentElement.classList.contains("bp-interface-visual")
        ? document.getElementById("visual_redo")
        : document.getElementById("redo");
      if (redo) redo.click();
    }
  });

  window.BioPlotBlocks = {
    copyGeneratedCode: copyGeneratedCode,
    openHelp: openHelp,
    closeHelp: closeHelp,
    setHelpLanguage: setHelpLanguage,
    setInterfaceMode: setInterfaceMode,
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

  document.addEventListener("scroll", function () {
    if (window.innerWidth <= 760 || pickerPositionFrame || !document.querySelector(".bp-picker-group.is-open")) return;
    pickerPositionFrame = window.requestAnimationFrame(function () {
      pickerPositionFrame = null;
      positionOpenModulePickerMenus();
    });
  }, true);

  function initializeInterface() {
    setInterfaceMode(storedInterfaceMode(), false, false);
    refreshResizeHandles();
    setPreviewView("plot", false);
    initializePreviewViewHandler();
    initializeParameterValueHandler();
    initializeVisualChartHandler();
    setHelpLanguage("zh");
    initializeHelpNavigation();
    initializeTextInputContinuity();
    initializeProjectPersistence();
    updateVisualColorSwatch(document.getElementById("visual_point_color"));
    updateVisualColorSwatch(document.getElementById("visual_reference_color"));
  }

  if (readStoredProject()) document.documentElement.classList.add("bp-restoring-project");

  if (document.readyState === "loading") {
    document.addEventListener("DOMContentLoaded", initializeInterface, { once: true });
  } else {
    window.requestAnimationFrame(initializeInterface);
  }
  window.addEventListener("resize", constrainResizeLayout);
  document.addEventListener("shiny:connected", function () {
    setInterfaceMode(storedInterfaceMode(), false, true);
    refreshResizeHandles();
    initializePreviewViewHandler();
    initializeParameterValueHandler();
    initializeVisualChartHandler();
    initializeProjectPersistence();
  });
})();
