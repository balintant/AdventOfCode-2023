#!/bin/zsh
if ! command -v hyperfine >/dev/null 2>&1; then
  echo "Please install hyperfine."
  exit 1
fi

execute () {
  sudo purge
  (
    trap time EXIT
    cd $1
    hyperfine \
      --warmup 5 \
      --min-runs 10 \
      --max-runs 300 \
      --command-name $1 \
      --setup 'make clean prepare' \
      --prepare 'sync;' \
      'make execute'
  )
  echo " "
}

if test -z $1; then
  for dir in day_*; do
   execute $dir
  done
else
  execute $1
fi

