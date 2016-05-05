// module Node.Which

var which = require('which');

exports.whichImpl = function(path) {
  return function (errcb) {
    return function (cb) {
      return function() {
        which(path, { all: true }, function(err, resolved) {
          if (err) {
            errcb(err)();
          }
          cb(resolved)();
        });
      };
    };
  };
};
