[![Build Status](https://travis-ci.org/r-raymond/purple-muon.svg?branch=master)](https://travis-ci.org/r-raymond/purple-muon) [![LICENSE](https://img.shields.io/badge/LICENSE-GPL--3-brightgreen.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)

Purple Muon (Working Title)
==========================

_Purple Muon_ is a networked two player game written in Haskell. So far it is
still in the _very_ early stages.


How to run
----------

Clone the library. Make sure `stack` is installed.

### Dependencies
- SDL 2.0.5
- SDL2-mixer 2.0.x
- SDL2-ttf   2.0.x

### Build with stack
`stack build`

### Run
First start the server

```
stack exec pm-server -- -u 1337
```

Then start the client

```
stack exec pm-client
```


Suggestions, Pullrequest, etc.
-----------------------------
Are all welcome.


Documentation
-------------
can be found at [gh-pages](https://r-raymond.github.io/purple-muon)
