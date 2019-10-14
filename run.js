const EventEmitter = require('events').EventEmitter;
const WebSocketServer = require('websocket').server;
const http = require('http');
const nodeStatic = require('node-static');

const ev = new EventEmitter;
const file = new nodeStatic.Server('./dist', {
    indexFile: "index.html"
});

const monitor = http.createServer((request, response) => {
    request.addListener('end', () => {
        file.serve(request, response);
    }).resume();
}).listen(8888, () => {
    console.log((new Date()) + ' Server is listening on port 8888');
});

