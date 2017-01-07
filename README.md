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

### Build with stack
`stack build`

### Run
First start the server

```
stack exec pm-server
```

Then start the client

```
stack exec pm-client
```


Suggestions, Pullrequest, etc.
-----------------------------
Are all welcome.

TODO
----

* implement clock synchronization
* fixTimeStep should be abe to catch up
* implement handshake protocol
* reduce size of updates send across the net
