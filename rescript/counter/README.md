# How to setup a new project

```bash
mkdir project_name && cd $_
degit https://github.com/celsobonutti/vite-template-rescript-tea
yarn install
yarn build
echo >./src/style.css
yarn dev --host=0.0.0.0
```

## Side notes

Don't play with the warning/error defaults. It breaks the tooling in unexpected ways.
