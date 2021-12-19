import("./target-javascript/dist/main.js").then((module) => {
  process.exit(module.main());
});
