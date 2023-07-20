FROM theasp/clojurescript-nodejs:shadow-cljs-alpine

RUN apk add chromium chromium-chromedriver
ENV CHROME_BIN=/usr/bin/chromium

COPY . /app
WORKDIR /app

RUN npm install
RUN npx shadow-cljs classpath
RUN npx shadow-cljs compile test
CMD npm run test
