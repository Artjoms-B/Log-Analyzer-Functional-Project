FROM haskell:9

WORKDIR /app
COPY . /app

RUN cabal update && cabal build

CMD ["cabal", "run", "analyzer", "--", "--file", "log.txt", "--top", "5", "--json"]
