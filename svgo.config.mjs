// svgo settings for `make images`. Keep shapes as they are: converting a rect
// to a path drops style rules that target rects (svglite's `.svglite rect
// {fill: none}`), so a clipping rectangle became a black box.
export default {
  plugins: [
    {
      name: "preset-default",
      params: { overrides: { convertShapeToPath: false } },
    },
  ],
};
