#! /usr/bin/env bash

pandoc input.custom --from markdown --to html5 --standalone --template=custom.tpl --wrap=none --katex -o output.html
