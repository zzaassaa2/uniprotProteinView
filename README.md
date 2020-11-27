
# uniprotProteinView

<!-- badges: start -->
<!-- badges: end -->

The goal of uniprotProteinView is to provide a means to show a graphical
representation of the UniProt data as well as allow easy cross
comparison of features between different proteins for easy analysis of
protein features.

# Installation

To download package:

``` r
install.packages("devtools")
devtools::install_github("zzaassaa2/uniprotProteinView", build_vignettes = TRUE)
library("uniprotProteinView")
```

# Overview

The uniprotProteinView package provides a single function called
`drawProtein`. To see further documentation, use:

``` r
ls("package:uniprotProteinView")
```

To view an interactive shiny app, you can visit the webpage:

TODO

Otherwise the most simple usage of the function `drawProtein` is to
supply it with a protein key\_code, this can be found from the Uniprot
webpage at [UniProt](https://www.uniprot.org/) Once there, search for
your protein and get the protein key\_code

TODO picture

This code can then be used as shown below, for example purpose, we will
use human transcription factor p65, with code Q04206.

``` r
uniprotProteinView::drawProtein("Q04206")
```

TODO picture

Here the function will search the Uniprot database for the protein, and
then draw the main chain. Further details about the structure can be
added, for further information look through the vignettes:

``` r
browseVignettes("uniprotProteinView")
```

An example of a more advance usage, using two proteins is such:

``` r
uniprotProteinView::drawProtein(
  proteins = list(source = c("Q04206", "Q9D270"), colors = c("green", "green")),
  types = list(type = c("domain", "region of interest"), colors = c("red", "purple")),
  dess = list(type = "phos", colors = "blue"),
  structure = list(type = c("strand", "helix", "turn"), colors = c("green", "orange", "purple")),
  singleOffset = 2
)
```

TODO picture

# Contributions

The author of the package is George Zorn. The xml retrieval functions
make use of the `XML` and `httr` R packages for xml parsing and data
retrieval, respectively. The plot drawing functions make use of the
`plotly` R package in order to draw and label the plot. The package also
TODO-shiny

# References

TODO

Wickham, H. and Bryan, J. (2019). R Packages (2nd edition). Newton,
Massachusetts: O’Reilly Media. <https://r-pkgs.org/>

The UniProt Consortium. UniProt: a worldwide hub of protein knowledge.
Nucleic Acids Res. 47: D506-515 (2019)

# Acknowledgements

This package was developed as part of an assessment for 2020BCB410H:
Applied Bioinformatics, University of Toronto, Toronto,CANADA.
