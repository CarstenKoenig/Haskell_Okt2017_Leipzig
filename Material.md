---
title: "Haskell für Fortgeschrittene - Workshop"
author: Carsten König
date: Oktober 2017
geometry: margin=2cm
output: pdf_document
papersize: a4
---

# Inhaltsverzeichnis

- [Typklassen](Typklassen.md)
- [Monaden Transformer](MonadTrans.md)
- [Lenses](Lenses.md)
- [Persist - DB Zugriff](Persist.md)
- [Aeson - Json Decode/Encode](Aeson.md)
- [Scotty](Scotty.md)


# Tricks

## sqlite ansehen
```bash
sqlite3 "test.db"
> select * from event;
...
> .exit
```

## Bash

- STDERR verschlucken: `befehl 2>/dev/null`
- alle PDFs erzeugen mit `for f in *.md ; do echo "$f"; pandoc "$f" -o "${f%.md}.pdf" ; done`
