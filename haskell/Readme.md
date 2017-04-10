Building and Running
 Requirements: ghc-8.0.1 or later

Within this directory, from the commandline:

 `$cabal install --only-dependencies`
 `$ cabal build`

The cabal file will produce 4 binaries, one for each hardware thread configuration: 1,2,4, and 8 threads specifically.

To run the tests:

`$ dist/build/test-con-q-$X/test-con-q-$X`

Where '$X' is the number of threads.

