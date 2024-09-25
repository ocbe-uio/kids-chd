test:
	Rscript -e "shiny::shinyAppFile('app.R', options=list(launch.browser=TRUE))"

deploy:
	Rscript -e "rsconnect::deployApp()"
