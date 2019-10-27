const WebSocketServer = require('websocket').server;
const http = require('http');
const nodeStatic = require('node-static');

module.exports.listen = startMonitor;

function startMonitor (port, eventEmitter, log) {

    const file = new nodeStatic.Server('./dist', {
        indexFile: "index.html"
    });

    const monitor = http.createServer((request, response) => {
        request.addListener('end', () => {
            file.serve(request, response);
        }).resume();
    }).listen(port, () => {
        log(`Server (monitor) is listening on port ${port}`);
    });

    const wsServer = new WebSocketServer({
        httpServer: monitor,
        autoAcceptConnections: false
    });

    function originIsAllowed(origin) {
        console.log({
            'origin': origin
        });
        return true;
    }

    wsServer.on('request', request => {
        if (!originIsAllowed(request.origin)) {
            request.reject();
            log('Connection from origin ' + request.origin + ' rejected.');
            return;
        }

        const connection = request.accept('echo-protocol', request.origin);
        log('Connection accepted. Peer: ' + connection.remoteAddress);

        function onGotRequest (data) {
            data['type'] = 'request';
            connection.sendUTF(JSON.stringify(data));
        }

        function onGotResponse (data) {
            data['type'] = 'response';
            connection.sendUTF(JSON.stringify(data));
        }

        eventEmitter.on('got-request', onGotRequest);
        eventEmitter.on('got-response', onGotResponse);

        connection.on('close', (reasonCode, description) => {
            log('Peer ' + connection.remoteAddress + ' disconnected.');
            eventEmitter.removeListener('got-request', onGotRequest);
            eventEmitter.removeListener('got-response', onGotResponse)
        });
    });
}


