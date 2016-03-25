PKGSRC  := $(shell basename `pwd`)

watch:
	watch -n 20 "touch server.R && touch ui.R"

app:
	Rscript -e "require(shiny); runApp(launch.browser = T)"