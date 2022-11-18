import("./target-javascript/main.mjs").then((module) => {
  process.exit(module.main());
});
