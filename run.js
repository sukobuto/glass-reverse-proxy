const EventEmitter = require('events').EventEmitter;
const argv = require('argv');
const monitor = require('./server/monitor');
const proxy = require('./server/proxy');

function main (monitorPort, proxyPort, proxyTarget) {

    function log (message) {
        console.log((new Date()) + ' ' + message);
    }

    const ev = new EventEmitter;
    ev.on('got-request', data => {
        log('got request ' + data.id + ' url ' + data.url);
    });
    ev.on('got-response', data => {
        log('got response ' + data.id)
    });

    monitor.listen(monitorPort, ev, log);
    proxy.listen(proxyPort, proxyTarget, ev, log);
}

argv.option({
    name: 'monitor-port',
    short: 'm',
    type: 'number',
    description: 'port number for monitoring console',
});

argv.option({
    name: 'proxy-port',
    short: 'p',
    type: 'number',
    description: 'port number for proxy service',
});

argv.option({
    name: 'proxy-target',
    short: 't',
    type: 'string',
    description: 'target base url of backend server',
});

const args = argv.run();
main(
    args.options['monitor-port'] || 18000,
    args.options['proxy-port'] || 8080,
    args.options['proxy-target'] || 'http://localhost:8000'
);
