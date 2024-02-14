animations:
	Rscript R/ambla.R

figures:
	Rscript R/figures.R

render:
	quarto render

publish:
	quarto publish gh-pages

push:
	git add .
	git commit -m "Update"
	git push

all: figures animations render publish push