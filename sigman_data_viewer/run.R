message("\n********************************",
        "\nLaunching Sigman data viewer ...",
        "\n********************************\n")

# supposed to be run from the linearity folder
folder <- "sigman_data_viewer"
if (basename(getwd()) != folder) {
	cat("\n")
	stop("Please launch this app from within the ", folder, " folder.\nYou are currently in the following folder:\n", getwd(), "\n\n")
}

require(shiny)
require(methods)
runApp(".", host = "0.0.0.0", port = 1234, launch.browser=TRUE)