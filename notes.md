just use cabal, ignore stack

need to make the ghc version and base version match up

cabal exec -v0 -- ghc --print-libdir
/Users/mike/.ghcup/ghc/9.8.2/lib/ghc-9.8.2/lib

now we have lsp working and jump to def for lib fn

try adding a dep

```
cabal update && cabal install brick
Downloading the latest package list from hackage.haskell.org
Package list of hackage.haskell.org is up to date.
The index-state is set to 2024-11-25T21:40:33Z.
Resolving dependencies...
Error: cabal: Cannot build the executables in the package brick bec
ause none                                                          of the components are available to build: the executable
'brick-visibility-demo', the executable 'brick-viewport-scrollbars-
demo', the                                                         executable 'brick-viewport-scroll-demo', the executable 'brick-them
e-demo',                                                           the executable 'brick-text-wrap-demo', the executable 'brick-tail-d
emo', the                                                          executable 'brick-tabular-list-demo', the executable 'brick-table-d
emo', the                                                          executable 'brick-suspend-resume-demo', the executable 'brick-readm
e-demo',                                                           the executable 'brick-progressbar-demo', the executable 'brick-padd
ing-demo',                                                         the executable 'brick-mouse-demo', the executable 'brick-list-vi-de
mo', the                                                           executable 'brick-list-demo', the executable 'brick-layer-demo', th
e                                                                  executable 'brick-hello-world-demo', the executable 'brick-form-dem
o', the                                                            executable 'brick-fill-demo', the executable 'brick-file-browser-de
mo', the                                                           executable 'brick-editor-line-numbers-demo', the executable 'brick-
edit-demo',                                                        the executable 'brick-dynamic-border-demo', the executable
'brick-dialog-demo', the executable 'brick-custom-keybinding-demo',
 the                                                               executable 'brick-custom-event-demo', the executable 'brick-croppin
g-demo',                                                           the executable 'brick-cache-demo', the executable 'brick-border-dem
o' and the                                                         executable 'brick-attr-demo' are all marked as 'buildable: False'
```

try installing it with flag to build the demos

```
$ cabal install brick -f demos
```

requires GHC -threaded option to compile

```
# add flag to ttt.cabal
    ghc-options: -threaded

cabal clean && cabal run
```

## Brick.Main

### `App s e n` type

#### `s` application state

#### `e` custom events

Default `vty` events

- keyboard
- resize
  https://hackage.haskell.org/package/vty-6.2/docs/Graphics-Vty-Input-Events.html

#### `n` resource name

ways to refer to rendering state

### appDraw

Draws a list of `Widget` layers
Fixed size
Responsive layout combinators

Drawing functions requesting the use of attributes consult the attribute map

## Dialog

render takes a dialog state

## BrickEvent

defined in [`src/Brick/Types/Internal.hs`](https://github.com/jtdaugherty/brick/blob/9a300da21333cb8a88fc2f24478f3caed801e6a8/src/Brick/Types/Internal.hs)

- union of vty event, custom event, mouse events

## EventM

`EventM n s a`
`n` resource name
`s` application state
`a` ???
monad in which event handlers run
