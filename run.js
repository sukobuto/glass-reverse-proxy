const EventEmitter = require('events').EventEmitter;
const WebSocketServer = require('websocket').server;
const http = require('http');
const httpProxy = require('http-proxy');
const nodeStatic = require('node-static');
const { ulid } = require('ulid');

const monitorPort = 8888;
const proxyPort = 7777;
const proxyTarget = 'http://localhost:8080';
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
    log(`Server (monitor) is listening on port ${monitorPort}`);
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
        // connection.sendUTF(message.utf8Data);
        ev.emit('publish', message.utf8Data);
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

    function onGotRequest (data) {
        data['type'] = 'request';
        connection.sendUTF(JSON.stringify(data));
    }

    function onGotResponse (data) {
        data['type'] = 'response';
        connection.sendUTF(JSON.stringify(data));
    }

    ev.on('got-request', onGotRequest);
    ev.on('got-response', onGotResponse);

    connection.on('close', (reasonCode, description) => {
        log('Peer ' + connection.remoteAddress + ' disconnected.');
        ev.removeListener('got-request', onGotRequest);
        ev.removeListener('got-response', onGotResponse)
    });
});

const proxy = httpProxy.createProxyServer({
    target: proxyTarget
});

ev.on('got-request', data => {
    console.log(data);
});
ev.on('got-response', data => {
    console.log(data);
});

proxy.on('proxyReq', (proxyReq, req, res, options) => {
    const start = new Date();
    let body = null;
    req.id = ulid();    // response との紐付けのために ID をふる
    req.on('data', chunk => {
        body = (body == null ? '' : body) + chunk;
    });
    req.on('end', () => {
        const end = new Date();
        ev.emit('got-request', {
            'id': req.id,
            'headers': Object.entries(req.headers).map(([key, value]) => ({ name: key, value })),
            'url': req.url,
            'httpVersion': req.httpVersion,
            'method': req.method,
            'body': body,
            'start': start.getTime(),
            'end': end.getTime(),
        });
    });
});
proxy.on('proxyRes', (proxyRes, req, res) => {
    const start = new Date();
    let body = null;
    proxyRes.on('data', chunk => {
        body = (body == null ? '' : body) + chunk;
    });
    proxyRes.on('end', () => {
        const end = new Date();
        ev.emit('got-response', {
            'id': req.id,
            'httpVersion': proxyRes.httpVersion,
            'headers': Object.entries(proxyRes.headers).map(([key, value]) => ({ name: key, value })),
            'statusCode': proxyRes.statusCode,
            'statusMessage': proxyRes.statusMessage,
            'body': body,
            'start': start.getTime(),
            'end': end.getTime(),
        });
    });
});
proxy.listen(proxyPort, () => {
    log(`Server (proxy) is listening on port ${proxyPort}`);
});

