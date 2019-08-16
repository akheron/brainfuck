# Brainfuck

[![CircleCI](https://circleci.com/gh/akheron/brainfuck.svg?style=shield)](https://circleci.com/gh/akheron/brainfuck)

Brainfuck interpreter for the browser, written in Elm.

Live demo: http://digip.org/brainfuck


## Development

Install Elm 0.19:

    $ npm install -g elm

See [the guide](https://guide.elm-lang.org/install.html) for more info
on installing Elm.

Install dependencies:

    $ yarn install

Available commands:

    $ yarn build  # build once
    $ yarn watch  # watch sources and build when a change is made
    $ yarn test   # run tests
    $ yarn dist   # build a dist bundle (optimized, minimized)

After building, open `index.html` in the browser.
