(() => {
  const bodySidebarClasses = () =>
    document.body.className.split(" ").filter(c => c.startsWith("sidebar-"));
  const bodyNonSidebarClasses = () =>
    document.body.className.split(" ").filter(c => !c.startsWith("sidebar-"));
  const bodyModeClass = () =>
    document.body.className.split(" ").find(c => c.endsWith("-mode"));
  const bodyNonModeClasses = () =>
    document.body.className.split(" ").filter(c => !c.endsWith("-mode"));

  if (window.screen.width <= 768) {
    document.body.className = [
      ...bodyNonSidebarClasses(),
      "sidebar-closed"
    ].join(" ");
  }
  const closeSidebar = () => {
    document.body.className = [
      ...bodyNonSidebarClasses(),
      "sidebar-closing"
    ].join(" ");
    setTimeout(() => {
      document.body.className = [
        ...bodyNonSidebarClasses(),
        "sidebar-closed"
      ].join(" ");
    }, 300);
  };

  const openSidebar = () => {
    document.body.className = [
      ...bodyNonSidebarClasses(),
      "sidebar-opening"
    ].join(" ");
    setTimeout(() => {
      document.body.className = [
        ...bodyNonSidebarClasses(),
        "sidebar-opened"
      ].join(" ");
    }, 150);
  };

  const toggleSidebar = () => {
    let classes = bodySidebarClasses();
    if (
      classes.includes("sidebar-closing") ||
      classes.includes("sidebar-opening")
    ) {
      return;
    }

    if (classes.includes("sidebar-closed")) {
      openSidebar();
    } else {
      closeSidebar();
    }
  };

  const toggleNightMode = () => {
    const modeClass = bodyModeClass();
    if (modeClass === "night-mode") {
      document.body.className = [...bodyNonModeClasses(), "light-mode"].join(
        " "
      );
    } else {
      document.body.className = [...bodyNonModeClasses(), "night-mode"].join(
        " "
      );
    }
  };

  document
    .querySelector("button.sidebar-button")
    .addEventListener("click", e => {
      toggleSidebar();

      e.preventDefault();
    });

  document
    .querySelector("button.night-mode-toggle")
    .addEventListener("click", e => {
      toggleNightMode();

      e.preventDefault();
    });

  document.body.addEventListener("keydown", e => {
    if (e.shiftKey || e.metaKey || e.ctrlKey || e.altKey) {
      return;
    }

    if (e.code === "KeyC") {
      toggleSidebar();
      e.preventDefault();
      return;
    }

    if (e.code === "KeyN") {
      toggleNightMode();
      e.preventDefault();
      return;
    }
  });
})();
