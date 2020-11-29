
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

The uniprotProteinView package provides six functions, however the user
will normally only need to use two: `drawProtein()`, the main drawing
function, or `runUniprotProteinView()`, a function that opens up an
interactive Shiny app. To see further documentatio

``` r
ls("package:uniprotProteinView")
```

To view an interactive shiny app, you can visit the webpage [Shiny
App](https://zzaassaa2.shinyapps.io/proteinView/), or call the function:

``` r
uniprotProteinView::runUniprotProteinDraw()
```

Otherwise the most simple usage of the function `drawProtein` is to
supply it with a protein key\_code, this can be found from the UniProt
webpage at [UniProt](https://www.uniprot.org/) Once there, search for
your protein and get the protein key\_code

This code can then be used as shown below, for example purpose, we will
use human transcription factor p65, with code Q04206.

``` r
#Note: that your protein will be a different color than the image shown, by default the function chooses a random color
uniprotProteinView::drawProtein("Q04206")
```

![](https://github.com/zzaassaa2/uniprotProteinView/inst/extdata/first.png)

Here the function will search the UniProt database for the protein, and
then draw the main chain. Further details about the structure can be
added, for further information look through the vignettes:

``` r
browseVignettes("uniprotProteinView")
```

An example of a more advance usage, using two proteins is such:

``` r
uniprotProteinView::drawProtein(
  proteins = list(type = c("Q04206", "Q9D270"), colors = c("green", "green")),
  types = list(type = c("domain", "region of interest"), colors = c("red", "purple")),
  descriptionSearch = list(type = "phos", colors = "blue"),
  offSetFeatures = list(type = c("strand", "helix", "turn"), colors = c("green", "orange", "purple")),
  singleOffset = 2
)
```

![](https://github.com/zzaassaa2/uniprotProteinView/inst/extdata/second.png)

# Contributions

The author of the package is George Zorn. The xml retrieval functions
make use of the `XML` and `httr` R packages for xml parsing and data
retrieval, respectively. The plot drawing functions make use of the
`plotly` R package in order to draw and label the plot. The package also
makes use of Shiny to create an interactive app. Secondary inclusions
are the `rgb()` function from `grDevices`, `runif()` from `stats`, and
`download.file()`, `setTxtProgressBar()`, and `txtProgressBar()`
functions from the `utils` package.

# References

Chang, W., Cheng J., Allaire J., Xie Y., McPherson J. (2017). Shiny: web
application framework for R.

Park T. Bootswatch. (2020). GitHub Repository.
<https://github.com/thomaspark/bootswatch>

Plotly R Open Source Graphing Library. Plotly. Website.
<https://plotly.com/r/>

R Core Team (2017). R: A language and environment for statistical
computing. R Foundation for Statistical Computing, Vienna, Austria. URL
<https://www.R-project.org/>.

The UniProt Consortium. UniProt: a worldwide hub of protein knowledge.
*Nucleic Acids Res.* 47: D506-515 (2019)

Wickham, H. and Bryan, J. (2019). R Packages (2nd edition). Newton,
Massachusetts: O’Reilly Media. <https://r-pkgs.org/>

# Acknowledgements

This package was developed as part of an assessment for 2020BCB410H:
Applied Bioinformatics, University of Toronto, Toronto,CANADA.
