import("./target-javascript/build/main.mjs").then((module) => {
  process.exit(module.main());
});
