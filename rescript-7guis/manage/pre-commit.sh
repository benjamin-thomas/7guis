#!/bin/sh

# Check formatting of staged ReScript files.
# Called by the top-level .git/hooks/pre-commit dispatcher.

if git diff --cached --name-only | grep -q '\.res$'; then
  cd rescript-7guis && npx rescript format --check || {
    echo "ReScript formatting check failed. Run: cd rescript-7guis && npx rescript format"
    exit 1
  }
fi