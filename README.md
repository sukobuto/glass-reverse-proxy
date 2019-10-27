# Glass Reverse Proxy
HTTP reverse proxy with request and response viewer  
for monitoring Web service, backend Web API, and so on.  
!! use only for development !!

![use image](docs/use-image.png)

## Get Started

```
git clone https://github.com/sukobuto/glass-reverse-proxy.git
cd glass-reverse-proxy
npm i
npm run-script build
node run.js -m 18000 -p 8080 -t http://localhost:8000
```

then access to http://localhost:18000

## Usage

`node run.js [options]`

options

- `-m <port>` `--monitor-port=<port>`
    - designate the port number for monitoring
- `-p <port>` `--proxy-port=<port>`
    - designate the port number for proxy service
- `-t <base url>` `--proxy-target=<base url>`
    - designate the url of the backend server
