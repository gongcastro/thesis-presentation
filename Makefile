restore:
	Rscript -e "renv::restore()"

animations:
	Rscript R/ambla.R

render:
	quarto render

pdf:
	decktape https://gongcastro.github.io/thesis-presentation ./index.pdf