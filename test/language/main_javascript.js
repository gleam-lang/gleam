import("./target-javascript/main.mjs").then((module) => {
  if (Deno) {
    Deno.exit(module.main());
  } else {
    process.exit(module.main());
  }
});
