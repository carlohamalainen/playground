# Yesod 1.4 cabal sandbox

The current [Yesod quickstart](http://www.yesodweb.com/page/quickstart) recommends
[Stack](https://github.com/commercialhaskell/stack#readme) which is good. You should use Stack.

I wanted to build Yesod 1.4 in an old-fashioned cabal sandbox, so I
first built it the stack environment, found all of the package versions, and produced a
[cabal.config](./cabal.config) and [build script](./build_in_sandbox.sh) that gets it all going.

How to use:

    mkdir my-yesod-project
    cd my-yesod-project

    wget https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/yesod-1.4-in-sandbox-quickstart/build_in_sandbox.sh
    wget https://raw.githubusercontent.com/carlohamalainen/playground/master/haskell/yesod-1.4-in-sandbox-quickstart/cabal.config

    bash build_in_sandbox.sh

This takes about 18 minutes on my quad-core E450 thinkpad.

Now to initialise the scaffold and run the devel server:

    export PATH=`pwd`/.cabal-sandbox/bin:$PATH

    yesod init --bare

    cabal build

    cabal repl # Check that this works

    yesod devel

Then visit http://127.0.0.1:3000/
