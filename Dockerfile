FROM ubuntu:precise

RUN apt-get update --fix-missing
RUN apt-get install -y software-properties-common python-software-properties
RUN add-apt-repository -y ppa:hvr/ghc
RUN apt-get update --fix-missing
RUN apt-get install -y cabal-install-1.16 ghc-7.6.3

ENV PATH=/opt/ghc/7.6.3/bin:/opt/cabal/1.16/bin:$PATH

ADD ./ascii-progress.cabal /opt/ascii-progress/ascii-progress.cabal
RUN cd /opt/ascii-progress && cabal update && cabal install --only-dependencies -fexamples --enable-test --enable-benchmarks

ADD ./ /opt/ascii-progress/
RUN cabal sandbox delete && cabal build

RUN cabal test
