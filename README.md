# vetoboxr 

[![Build Status](https://travis-ci.org/erocoar/vetoboxr.svg?branch=master)](https://travis-ci.org/erocoar/vetoboxr)
[![CRAN_Release_Badge](http://www.r-pkg.org/badges/version-ago/vetoboxr)](https://CRAN.R-project.org/package=vetoboxr)
[![CRAN_Download_Badge](http://cranlogs.r-pkg.org/badges/grand-total/vetoboxr)](https://CRAN.R-project.org/package=vetoboxr)

## About
Vetoboxr implements Spatial Voting in R, offering an intuitive way of creating voters and voting games. With Vetoboxr you can

- create Agenda Setters, Veto Players, Normal voters and Random voters
- let them vote once or multiple times
  - let them drift in space after each vote
  - subject them to random vibration after each vote
- obtain detailed results of each vote -- voting outcomes, winset area, distance travelled by status quo, payoffs, coalitions, etc.
- `plot()` and `animate()` voting outcomes in 1-D and 2-D.

## Installation
`vetoboxr` can be installed via GitHub:

```r
if (!require(devtools)) {
    install.packages('devtools')
}
devtools::install_github('erocoar/vetoboxr')
```

## Roadmap
- optimize winset and voting functions
- add convenience functions for generating voters
- add other shapes, i.e. yolk, shepherd set, etc.

If you would like to see a certain feature, please file an issue with a detailed description.

