"use strict";

window.Gleam = function() {
  /* Global Object */
  const self = {};

  /* Public Properties */

  self.hashOffset = undefined;

  /* Public Methods */

  self.getProperty = function(property) {
    let value;
    try {
      value = localStorage.getItem("Gleam." + property);
    } catch (_error) {}
    if (-1 < [null, undefined].indexOf(value)) {
      return gleamConfig[property].value;
    }
    return value;
  };

  self.scrollToHash = function() {
    const locationHash = arguments[0] || window.location.hash;
    const query = locationHash ? locationHash : "body";
    const hashTop = document.querySelector(query).offsetTop;
    window.scrollTo(0, hashTop - self.hashOffset);
    return locationHash;
  };

  self.toggleSidebar = function() {
    const previousState =
      bodyClasses.contains("drawer-open")
      ? "open"
      : "closed";

    let state;
    if (0 < arguments.length) {
      state = false === arguments[0] ? "closed" : "open";
    }
    else {
      state = "open" === previousState ? "closed" : "open";
    }

    bodyClasses.remove("drawer-" + previousState);
    bodyClasses.add("drawer-" + state);

    if ("open" === state) {
      document.addEventListener("click", closeSidebar, false);
    }
  };

  /* Private Properties */

  const html = document.documentElement;
  const body = document.body;
  const bodyClasses = body.classList;
  const sidebar = document.querySelector(".sidebar");
  const sidebarToggles = document.querySelectorAll(".sidebar-toggle");

  /* Private Methods */

  const initProperty = function(property) {
    setProperty(null, property, function() {
      return self.getProperty(property);
    });
  };

  const setProperty = function(_event, property) {
    const previousValue = self.getProperty(property);

    const update =
      2 < arguments.length
      ? arguments[2]
      : gleamConfig[property].update;
    const value = update();

    try {
      localStorage.setItem("Gleam." + property, value);
    }
    catch (_error) {}

    bodyClasses.remove(property + "-" + previousValue);
    bodyClasses.add(property + "-" + value);

    const isDefault = value === gleamConfig[property].value;
    const toggleClasses =
      document.querySelector("#" + property + "-toggle").classList;
    toggleClasses.remove("toggle-" + (isDefault ? "right" : "left"));
    toggleClasses.add("toggle-" + (isDefault ? "left" : "right"));

    try {
      gleamConfig[property].callback(value);
    }
    catch(_error) {}

    return value;
  }

  const setHashOffset = function() {
    const el = document.createElement("div");
    el.style.cssText =
      "height: var(--hash-offset);"
      + "pointer-events: none;"
      + "position: absolute;"
      + "visibility: hidden;"
      + "width: 0;";
    body.appendChild(el);
    self.hashOffset = parseInt(
      getComputedStyle(el).getPropertyValue("height")
      || "0"
    );
    body.removeChild(el);
  };

  const closeSidebar = function(event) {
    if (! event.target.closest(".sidebar-toggle")) {
      document.removeEventListener("click", closeSidebar, false);
      self.toggleSidebar(false);
    }
  };

  const init = function() {
    for (const property in gleamConfig) {
      initProperty(property);
      const toggle = document.querySelector("#" + property + "-toggle");
      toggle.addEventListener("click", function(event) {
        setProperty(event, property);
      });
    }

    sidebarToggles.forEach(function(sidebarToggle) {
      sidebarToggle.addEventListener("click", function(event) {
        event.preventDefault();
        self.toggleSidebar();
      });
    });

    setHashOffset();
    window.addEventListener("load", function(_event) {
      self.scrollToHash();
    });
    window.addEventListener("hashchange", function(_event) {
      self.scrollToHash();
    });
  };

  /* Initialise */

  init();

  return self;
}();
