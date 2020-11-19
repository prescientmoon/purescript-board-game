import("purescript/Main").then(({ main }) => {
  console.timeEnd("Loaded purescript");

  main();
});
