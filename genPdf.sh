#!/bin/bash

for f in *.md ; do echo "$f"; pandoc "$f" -o "${f%.md}.pdf" ; done
