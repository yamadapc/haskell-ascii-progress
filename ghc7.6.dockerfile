FROM prettynatty/ghc-7.6

RUN cabal update

ADD . /haskell-ascii-progress

WORKDIR /haskell-ascii-progress

RUN cabal sandbox init
RUN cabal install --only-dep -j -fexamples

RUN cabal build
