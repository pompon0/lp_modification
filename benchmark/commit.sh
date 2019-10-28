if git diff-index --quiet HEAD; then
  git rev-list HEAD --max-count=1
fi
