FROM prettynatty/ghc-7.6

ADD . /haskell-ascii-progress

WORKDIR /haskell-ascii-progress

RUN cabal sandbox init
RUN cabal install --only-dep -j --enable-test -fexamples

RUN cabal build
RUN cabal test
