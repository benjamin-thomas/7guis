Initialize projects with:

```
mkdir project_name
rsync -av --exclude-from=../counter/.gitignore  ../counter/ .
npm i
spago build
```

Then run with:

```
# terminal 1
pscid --censor-codes UnusedName,ShadowedName,UnusedImport,UnusedExplicitImport,UnusedDeclaration

# terminal 2
vite dev
```
