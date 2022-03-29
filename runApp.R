###########################
# START biomapper 1.0 App #
###########################

if (!require("shiny"))
  install.packages("shiny")
require(shiny)
runGitHub('biomapper_1.0_html_intel','TommasoCanc', ref="main")
