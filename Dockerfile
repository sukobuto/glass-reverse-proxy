FROM node:12.13-alpine
MAINTAINER sukobuto

ENV MONITOR_PORT 18000
ENV PROXY_PORT 8080
ENV PROXY_TARGET "http://localhost:8000"

COPY . /app
WORKDIR /app
RUN npm i && npm run-script build

EXPOSE $MONITOR_PORT
EXPOSE $PROXY_PORT
CMD node run.js -m $MONITOR_PORT -p $PROXY_PORT -t $PROXY_TARGET
