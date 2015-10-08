## ----echo=FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  comment = "#>",
  collapse = TRUE,
  warning = FALSE, 
  message = FALSE
)

## ----eval=FALSE----------------------------------------------------------
#  if(packageVersion("devtools") < 1.6) {
#    install.packages("devtools")
#  }
#  devtools::install_github("chgrl/diezeit")

## ------------------------------------------------------------------------
library(diezeit)

## ------------------------------------------------------------------------
zeit_search(endpoint="content", query="bayreuth")

## ------------------------------------------------------------------------
zeit_search("content", c("bayreuth", "festspiele"))

## ------------------------------------------------------------------------
zeit_search("content", "bayreuther festspiele")

## ------------------------------------------------------------------------
zeit_search("content", "title:bayreuth")

## ------------------------------------------------------------------------
zeit_search("content", "bayreuth", fields=c("title", "teaser_text"))

## ------------------------------------------------------------------------
zeit_search("content", "bayreuth", limit=1) # just one match
zeit_search("content", "bayreuth", limit=1, offset=1) # just the second match

## ------------------------------------------------------------------------
zeit_search("content", "bayreuth", 
  sort=c("release_date", "asc")) # sort by date
zeit_search("content", "bayreuth", 
  sort=list(c("release_date", "desc"), c("title", "asc"))) # sort by date and title

## ------------------------------------------------------------------------
bt.matches <- zeit_search("content", "bayreuth", print=FALSE)

## ------------------------------------------------------------------------
cap <- c("Berlin", "Bremen", "Dresden", "Düsseldorf", "Erfurt",
  "Hamburg", "Hannover", "Kiel", "Magdeburg", "Mainz", "München",
  "Potsdam", "Saarbrücken", "Schwerin", "Stuttgart", "Wiesbaden")
lat <- c(52.516667, 53.083333, 51.033333, 51.233333, 50.983333,
  53.565278, 52.366667, 54.333333, 52.133333, 50, 48.133333,
  52.4, 49.233333, 53.633333, 48.783333, 50.08) 
lon <- c(13.383333, 8.8, 13.733333, 6.783333, 11.033333,
  10.001389, 9.716667, 10.133333, 11.616667, 8.266667, 11.566667,
  13.066667, 7, 11.416667, 9.183333, 8.24)

## ------------------------------------------------------------------------
berlin <- zeit_search("content", "Berlin", print=FALSE)

## ------------------------------------------------------------------------
berlin$found

## ------------------------------------------------------------------------
found.l <- lapply(cap, function(x) zeit_search("content", x, print=FALSE)$found)
found <- unlist(found.l)

## ------------------------------------------------------------------------
found.norm <- found/max(found)

## ------------------------------------------------------------------------
cols <- rainbow(16, alpha=.8)
symbols(lon, lat, circles=8*found.norm, fg=cols, lwd=2, 
	main="Occurrences of German State Capitals in DIE ZEIT",
  xlim=c(6, 16), ylim=c(46, 56), xlab="", ylab="", bty="n")
h <- c(1,2,3,4,6,7,8,11,15)	# high occurrence capitals - just for readability
text(lon[h], lat[h], cap[h], col=cols[h])
text(lon[-c(h,16)], lat[-c(h,16)], cap[-c(h,16)], col=cols[-c(h,16)], pos=1)
text(lon[16], lat[16], cap[16], col=cols[16], pos=3)

