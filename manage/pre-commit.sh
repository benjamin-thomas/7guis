#!/bin/sh

# Git pre-commit hook dispatcher.
# Runs each sub-project's manage/pre-commit.sh if it exists.
#
# Install from the repo root with:
#   ln -sf ../manage/pre-commit.sh .git/hooks/pre-commit

for hook in */manage/pre-commit.sh; do
  [ -x "$hook" ] && "$hook" || exit 1
done
