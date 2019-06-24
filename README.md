# One Hour One Life Family Tree Viewer

Web UI searching characters and displaying family tree graphs from a life data server.

Depends on a server [ohol-data-server](https://github.com/JustinLove/ohol-data-server) which also uses [ohol-family-trees](https://github.com/JustinLove/ohol-family-trees)

## Compiling

Built using [Elm](http://elm-lang.org/)

My build command:

> `elm make src/FamilyTreeViewer.elm --output public/family-tree-viewer.js

`bin/monitor.bat` has a command using the [watch](https://www.npmjs.com/package/watch) CLI

Once built, `public/index.html` can be opened locally from the filesystem, or set up for a local or internet server as you wish.

## Credits

Icons: [IcoMoon - Free](https://icomoon.io/#icons-icomoon) ([CC BY 4.0](http://creativecommons.org/licenses/by/4.0/))
