import { createConnection } from 'net';

export function send(cmd) {
  return function (port) {
    return function (cb) {
      return function (err) {
        return function () {
          var sock = createConnection({ host: "127.0.0.1", port: port })
            , result = ""
          sock.on('data', function (data) {
            result += data
          })
            .on('end', function () {
              cb(result)()
            })
            .on('connect', function () {
              sock.setEncoding('utf8');
              sock.write(cmd + "\n")
            })
            .on('error', function (e) {
              err(e)()
            })
        }
      }
    }
  }
}
