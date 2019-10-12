const EventEmitter = require('events').EventEmitter;
const WebSocketServer = require('websocket').server;
const http = require('http');

const ev = new EventEmitter;

const monitor = http.createServer((request, response) => {

})
