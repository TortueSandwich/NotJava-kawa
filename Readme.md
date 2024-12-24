build and execute :
```sh
dune build && dune exec ./kawai.exe -- <args>
```
or
```sh
dune build && ./kawai.exe <args>
```

run using :
```sh
./kawai.exe <file1> [--show-source | -s]
```

exemples d'utilisation :
```sh
./kawai.exe ./test/var.kwa
```
```sh
dune build && ./kawai.exe -s
```
```sh
./kawai.exe ./tests/min.kwa -s
```

todo
transtypage
typelevel programming

