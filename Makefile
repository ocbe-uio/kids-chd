test:
	Rscript -e "shiny::runApp(port=8080, launch.browser=TRUE)"

deploy:
	Rscript -e "rsconnect::deployApp()"
