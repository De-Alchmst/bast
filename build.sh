#! /bin/sh

dune build
mv _build/default/bin/main.exe ./bast
chmod 755 ./bast
