(function () {
  "use strict";

  let draggedCard = null;
  const pendingInputTimers = new Map();

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

  document.addEventListener("keydown", function (event) {
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
    sendInput: sendInput
  };
})();
