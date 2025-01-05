FROM haskell

RUN stack setup --install-ghc
RUN apt-get update
RUN apt-get install -y ruby
