export function _memoValuesImpl (eq) {
  return function (memos) {
    return { eq, memos }
  }
}
