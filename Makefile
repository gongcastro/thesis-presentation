restore:
	Rscript -e "renv::restore()"

animations:
	Rscript R/ambla.R

figures:
	Rscript R/figures.R

render:
	quarto render

pdf:
	decktape https://gongcastro.github.io/thesis-presentation ./index.pdf

publish:
	quarto publish gh-pages

push:
	git add .
	git commit -m "Update"
	git push

all: figures animations render publish push
