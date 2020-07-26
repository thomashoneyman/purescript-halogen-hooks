exports.setInterface = function (cb) {
  return function () {
    window.query = cb;
  };
};

// } window.query = (queryFn) => {
//   render(
//     pdfContainer({ pdfType, pdfData, now: instant })(),
//     document.getElementById("Awake")
//   );
// };
