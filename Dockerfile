FROM tanelso2/haskell-stack:latest

WORKDIR /opt/example
COPY . /opt/example

RUN stack build

CMD stack run
