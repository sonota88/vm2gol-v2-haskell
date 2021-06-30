Haskellでかんたんな自作言語のコンパイラを書いた  
https://memo88.hatenablog.com/entry/20210628_vm2gol_v2_haskell

```
$ ./docker_run.sh stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 8.10.4
```

```sh
## Build Docker image

docker build \
  --build-arg USER=$USER \
  --build-arg GROUP=$(id -gn) \
  -t vm2gol-v2:haskell .
```
