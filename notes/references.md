# Reference library

The papers behind this package and the training material, kept outside every
repository so that they are never committed.

## The location

`C:/Rworking/references/` holds one copy of each paper, guidance document and
saved web page, shared by every repository under `C:/Rworking`
(`C:/Rworking/CLAUDE.md`, section 15). It is not inside a git repository, so
nothing can add it to a commit. A symbolic link in this repository,
`ignore/references`, points to it. `ignore/` is listed in `.gitignore`, so the
link itself is invisible to git as well. Read a paper
through the link rather than the absolute path, so that the read stays inside
the working directory: for example `ignore/references/fox2010.pdf`.

These are publisher PDFs of copyrighted articles. They stay local for that
reason as well as for repository size.

## Finding a paper

`references.bib` in the library is the catalogue, with one entry per work.
Each file is named by its key, `<key>.<ext>`, and the entry's `file` field
names it. Where `vignettes/bayesnec.bib` cites a paper under another key, the
library entry lists that key in its `ids` field, so a search for the package's
key finds the file: `Fox2010` is `fox2010.pdf`, and `fisherfox2023` is
`fisher2023.pdf`. `renames.csv` in the library maps the file names used before
2026-11-24 to the current ones.

An entry of `vignettes/bayesnec.bib` that no library entry lists has no local
copy. Most of these are software or books rather than articles.
