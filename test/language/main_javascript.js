import("./target-javascript/main.mjs").then((module) => {
  if (globalThis.Deno) {
    globalThis.Deno.exit(module.main());
  } else {
    process.exit(module.main());
  }
});
