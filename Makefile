docs:
	Rscript --vanilla -e "devtools::document()"

test:
	Rscript --vanilla -e "devtools::test()"

README.md: README.Rmd
	Rscript --vanilla -e "rmarkdown::render('README.Rmd')"