const httpProxy = require('http-proxy');
const { ulid } = require('ulid');

module.exports.listen = startProxy;

function startProxy (port, target, eventEmitter, log) {

    const proxy = httpProxy.createProxyServer({
        target: target,
        timeout: 3600000,
        proxyTimeout: 3600000,
    });

    proxy.on('proxyReq', (proxyReq, req) => {
        const start = new Date();
        const bodyBuffer = [];
        req.id = ulid();    // response との紐付けのために ID をふる
        req.on('data', chunk => {
            bodyBuffer.push(chunk);
        });
        req.on('end', () => {
            const end = new Date();
            eventEmitter.emit('got-request', {
                'id': req.id,
                'headers': Object.entries(req.headers).map(([key, value]) => ({ name: key, value })),
                'url': req.url,
                'httpVersion': req.httpVersion,
                'method': req.method,
                'body': (bodyBuffer ? Buffer.concat(bodyBuffer).toString('base64') : null),
                'start': start.getTime(),
                'end': end.getTime(),
            });
        });
    });

    proxy.on('proxyRes', (proxyRes, req) => {
        const start = new Date();
        const bodyBuffer = [];
        proxyRes.on('data', chunk => {
            bodyBuffer.push(chunk);
        });
        proxyRes.on('end', () => {
            const end = new Date();
            eventEmitter.emit('got-response', {
                'id': req.id,
                'httpVersion': proxyRes.httpVersion,
                'headers': Object.entries(proxyRes.headers).map(([key, value]) => ({ name: key, value })),
                'statusCode': proxyRes.statusCode,
                'statusMessage': proxyRes.statusMessage,
                'body': (bodyBuffer ? Buffer.concat(bodyBuffer).toString('base64') : null),
                'start': start.getTime(),
                'end': end.getTime(),
            });
        });
    });

    proxy.on('error', function (err, req, res) {
        console.log(err);
        res.end();
    });

    proxy.listen(port, () => {
        log(`Server (proxy) is listening on port ${port} for ${target}`);
    });
}
