# diezeit
An R wrapper for the ZEIT ONLINE Content API

[![Build Status](https://api.travis-ci.org/chgrl/diezeit.png)](https://travis-ci.org/chgrl/diezeit)
![downloads](http://cranlogs.r-pkg.org/badges/grand-total/diezeit)

The [ZEIT ONLINE Content API](http://developer.zeit.de) allows for access to articles and corresponding metadata from the [ZEIT](http://www.zeit.de) archive and from ZEIT ONLINE. Access requires a personal [API key](http://developer.zeit.de/quickstart), that is [offered for free](http://developer.zeit.de/quickstart) for non-commercial use (up to 10,000 requests per day). The [terms of use](http://developer.zeit.de/licence) can be found [here](http://developer.zeit.de/licence) (German only at the moment).

## Install from GitHub
```
if(packageVersion("devtools") < 1.6) {
  install.packages("devtools")
}
devtools::install_github("chgrl/diezeit")
```

## Install from CRAN
`install.packages("diezeit")`

## Howto
Take a look at the vignette here: https://github.com/chgrl/diezeit/blob/master/vignettes/diezeit_vignette.Rmd
