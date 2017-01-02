"use strict";

// This is an efficiency workaround for: https://github.com/purescript/purescript/issues/2154
exports._take = function(uncons) {
  return function (n) {
    return function (il) {
      var currentList = il;
      var u;
      var result = [];
      while(n > 0) {
        u = uncons(currentList);
        result.push(u.head);
        currentList = u.tail;
        n--;
      }
      return result;
    };
  };
};

// This is an efficiency workaround for: https://github.com/purescript/purescript/issues/2154
exports._takeWhile = function(uncons) {
  return function (keep) {
    return function (il) {
        var result = [];
        var u = uncons(il);
        while(keep(u.head)) {
          result.push(u.head);
          u = uncons(u.tail);
        }
        return result;
    };
  };
};
