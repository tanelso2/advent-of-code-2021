FROM haskell

RUN stack config set system-ghc --global true
RUN stack setup

WORKDIR /opt/example
COPY . /opt/example

RUN stack build

CMD stack run
