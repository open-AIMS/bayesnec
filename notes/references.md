# Reference library

The papers behind this package and the training material, kept outside every
repository so that they are never committed.

## The location

`C:/Rworking/references/` holds 42 PDFs, one copy, shared by every repository
under `C:/Rworking`. It is not inside a git repository, so nothing can add it to
a commit. Two repositories reach it through a symbolic link:

| Path | Points to |
|---|---|
| `bayesnec/ignore/references` | `C:/Rworking/references` |
| `cr_modelling_training/ignore/references` | `C:/Rworking/references` |

`ignore/` is listed in the `.gitignore` of both repositories, so the link itself
is invisible to git as well. Read a paper through the link rather than the
absolute path, so that the read stays inside the working directory: for example
`ignore/references/Fox2010.pdf`.

These are publisher PDFs of copyrighted articles. They stay local for that
reason as well as for repository size.

## Papers the package bibliography cites

Each of these has an entry in `vignettes/bayesnec.bib`, so a citation in a
vignette resolves to a paper in the library.

| File | Key |
|---|---|
| `Blasco‐Moreno2019 - What does a zero mean....pdf` | `blasco2019` |
| `Burkner2017.pdf` | `Burkner2017` |
| `Fisher&Fox2023 -Enviro Toxic and Chemistry....pdf` | `fisherfox2023` |
| `Fisher_etal2023 - Integr Envir Assess   Manag.pdf` | `fisher2023ieam` |
| `Fox2010.pdf` | `Fox2010` |
| `Helsel2006.pdf` | `helsel2006` |
| `Hormesis-defined_2008_Ageing-Research-Reviews.pdf` | `Mattson2008` |
| `Jones and Kerswell2003 - Marine Ecology Progress Series.pdf` | `jones2003` |
| `Martin2005 - Zero tolerance ecology....pdf` | `martin2005` |
| `OECD201_2026.pdf` | `oecd2026tg201` |
| `Pires_et_al-2002-Environmetrics.pdf` | `Pires2002` |
| `Ritz_etal2016.pdf` | `Ritz2016` |
| `Ritz_etal2026.pdf` | `Ritz2026` |
| `Trenfied_etal2016.pdf` | `trenfield2016` |
| `Warton2005 - Many zeros does not mean zero inflation....pdf` | `warton2005` |
| `Weimer_etal2012.pdf` | `Weimer2012` |
| `Yao_etal2018.pdf` | `Yao2018` |
| `vehtari_etal2017.pdf` | `vehtari2017` |
| `vehtari_etal2021.pdf` | `Vehtari2021` |

## Papers with no bibliography entry

The rest of the library is background that no vignette cites: Brinkman et al.
2023 and its supplement, Chipman and McCulloch on data-informed priors, Cox 1987
on threshold models, de Bruyn and Elphick 2013, Depaoli et al. 2020, four Fox
papers from 2008 to 2012, Fox and Landis 2016 in two parts, Gelman 2006, Gelman
et al. 2017, Krull 2020, Kruschke and Liddell 2018, Labelle et al. 2019, Luter et
al. 2025, Mebane 2015 and Mebane et al. 2019, Nordborg et al. 2022, Stephen et
al. 1975 with the USEPA 1985 guidelines, Thomas 2007, and Warne et al. 2025.

## Bibliography entries with no paper

Ten entries in `vignettes/bayesnec.bib` have no PDF here, and all of them are
software or books rather than articles: `Burnham2002`, `Plummer2003`, `Su2015`,
`Wood2016`, `Fisher2020`, `Thorley2018`, `vehtari2020`, `Burkner2018`,
`stan2020` and `fox2020`.
