## Dev workflow

NOTE: this basic reload mechanism, gotten from the miso's README works fine with
Chrome but seems to malfunction with Firefox.

### With ghcid

```
ghcid -c 'cabal repl counter' -T=:main --reload=./counter.css --warnings
```

### Without ghcid

Reloading manually forces a browser refresh. And the CSS update does trigger
accordingly.

```
cabal repl counter
ghci> :cmd return $ unlines [":reload", ":main"]
```

### Build

For jsaddle over GHC

```sh
cabal build
```

For WASM target, run this

```sh
# Optionally serves the app locally
# CLEAN purges nay prior builds
SERVE=1 CLEAN=1 OPTIMIZE=1 ./manage/build_wasm counter


# WASM on port 8080
rg --files | entr -rc bash -c 'SERVE=1 ./manage/build_wasm counter'

# I can run the same app on GHC, port 8008
ghcid -c 'cabal repl temp-converter' -T=:main --reload=./temp-converter.css --warnings
```
