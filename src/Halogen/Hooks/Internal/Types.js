exports._memoValuesImpl = function(eq) {
  return function(memos) {
    return { eq, memos };
  };
};
