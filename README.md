[![Build Status](https://travis-ci.org/r-raymond/purple-muon.svg?branch=master)](https://travis-ci.org/r-raymond/purple-muon) [![LICENSE](https://img.shields.io/badge/LICENSE-GPL--3-brightgreen.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html)


![Purple Muon Logo](https://github.com/r-raymond/purple-muon/blob/master/res/png/logo.png)

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
- SDL2-image 2.0.x

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

Special Thanks
--------------

* To the Haskell community for helping me endless times and providing the
  awesome libraries used in this project
* [Kenney](kenney.nl) for creating and sharing awesome sprites and sounds

Copyright
---------
2016 - 2017 Robin Raymond

Credits
-------

###Purple Muon Logo

Space graphic by <a href="http://www.freepik.com/">Freepik</a> from <a href="http://www.flaticon.com/">Flaticon</a> is licensed under <a href="http://creativecommons.org/licenses/by/3.0/" title="Creative Commons BY 3.0">CC BY 3.0</a>. Made with <a href="http://logomakr.com" title="Logo Maker">Logo Maker</a>
