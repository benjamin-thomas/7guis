Implementing [7 GUI tasks](https://eugenkiss.github.io/7guis/tasks) in many languages

**Live demo:** https://benjamin-thomas.github.io/7guis/

- Counter
- Temperature Converter
- Flight Booker
- Timer
- CRUD
- Circle Drawer
- Cells

## Git hooks

A dispatcher script runs each sub-project's `manage/pre-commit.sh` on commit.

To install, run from the repo root:

```sh
ln -sf ../manage/pre-commit.sh .git/hooks/pre-commit
```

To add a hook for a new sub-project, create `<project>/manage/pre-commit.sh` (make it executable). It will be picked up automatically.