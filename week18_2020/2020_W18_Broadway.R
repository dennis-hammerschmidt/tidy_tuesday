#################
## TidyTuesday ##
## 2020 -- W17 ##
#################

##----------##
## Packages ##
##----------##

# function to install and load packages
install_and_load <- function(pkg) {
  new.pkgs <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkgs))
    install.packages(new.pkgs, repos = "https://cran.rstudio.com/")
  
  for (pkg_name in pkg)
    library(pkg_name, character.only = TRUE, quietly = TRUE)
}

# apply it to the following list of packages
packages <-
  c("tidyverse","extrafont", "showtext", "ggalluvial", "magrittr", "stringr", "RColorBrewer")
install_and_load(packages)

# additionally, load the tidytuesdayR package from github
devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)

# load the Sonsie One font from google for the final graph
font_add_google(name = "Frijole", family = "frijole")
showtext_auto()

##------------------##
## Read in the data ##
##------------------##

tuesdata <- tidytuesdayR::tt_load(2020, week = 18)
grosses <- tuesdata$grosses

test <- grosses %>% 
  group_by(theatre) %>% 
  count() %>% 
  filter(n > 500)
