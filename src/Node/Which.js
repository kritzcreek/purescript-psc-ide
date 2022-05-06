import which from 'which';

export function whichImpl(options) {
  return function (path) {
    return function (errcb) {
      return function (cb) {
        return function () {
          which(path, { all: true, path: options.path, pathExt: options.pathExt }, function (err, resolved) {
            if (err) {
              errcb(err)();
            }
            else {
              cb(resolved)();
            }
          });
        };
      };
    };
  };
};
