test:
	Rscript -e "shiny::shinyAppDir('src', options=list(launch.browser=TRUE))"

deploy:
	Rscript -e "rsconnect::deployApp('src', appName='kids-chd')"
