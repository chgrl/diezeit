all: move rmd2md

move:
		cp inst/doc/diezeit_vignette.md vignettes
		
rmd2md:
		cd vignettes;\
		mv diezeit_vignette.md diezeit_vignette.Rmd
