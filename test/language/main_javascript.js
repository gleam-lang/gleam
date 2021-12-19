import("./target-javascript/dist/main.mjs").then((module) => {
  process.exit(module.main());
});
