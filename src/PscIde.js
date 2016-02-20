// module PscIde

var net = require('net');

exports.send = function(cmd){
  return function(port){
    return function(cb){
      return function(err){
        return function(){
          var sock = net.createConnection({port: port})
          sock.on('data', function (data) {
            cb(data)()
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
