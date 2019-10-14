const EventEmitter = require('events').EventEmitter;
const WebSocketServer = require('websocket').server;
const http = require('http');
const nodeStatic = require('node-static');

const monitorPort = 8888;
const ev = new EventEmitter;
const file = new nodeStatic.Server('./dist', {
    indexFile: "index.html"
});

function log (message) {
    console.log((new Date()) + ' ' + message);
}

const monitor = http.createServer((request, response) => {
    request.addListener('end', () => {
        file.serve(request, response);
    }).resume();
}).listen(monitorPort, () => {
    log(`Server (monitor) is listening on port {monitorPort}`);
});

wsServer = new WebSocketServer({
    httpServer: monitor,
    autoAcceptConnections: false
});

function originIsAllowed(origin) {
    console.log({
        'origin': origin
    });
    return true;
}

function onMessage(connection, message) {
    if (message.type === 'utf8') {
        log('Received message: ' + message.utf8Data);
        connection.sendUTF(message.utf8Data);
    }
}

wsServer.on('request', request => {
    if (!originIsAllowed(request.origin)) {
        request.reject();
        log('Connection from origin ' + request.origin + ' rejected.');
        return;
    }

    const connection = request.accept('echo-protocol', request.origin);
    log('Connection accepted. Peer: ' + connection.remoteAddress);

    connection.on('message', message => onMessage(connection, message));
    connection.on('close', (reasonCode, description) => {
        log('Peer ' + connection.remoteAddress + ' disconnected.');
    });
});
