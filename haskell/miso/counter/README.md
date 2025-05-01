## Dev workflow

NOTE: this basic reload mechanism, gotten from the miso's README works fine with Chrome but seems to malfunction with Firefox.

### With ghcid

```
ghcid -c 'cabal repl app' -T=Main.main --reload=./main.css
```

### Without ghcid

Reloading manually forces a browser refresh. And the CSS update does trigger accordingly.

```
cabal repl app
ghci> :cmd return $ unlines [":reload", ":main"]
```

