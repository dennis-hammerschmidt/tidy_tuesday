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

tuesdata <- tidytuesdayR::tt_load(2020, week = 20)
volcano <- tuesdata$volcano
events <- tuesdata$events

##------------------##
## Prepare the data ##
##------------------##

volcano_data <- events %>% 
  # merge the datasets
  left_join(., volcano, by = "volcano_number") %>%
  # keep only the relevant variables
  select(event_type, region) %>%
  # drop all instances where no region was being located
  filter(!is.na(region)) %>%
  group_by(region, event_type) %>%
  # count the number of events by region
  count() %>%
  # select only those occurrences that appear more than 150 times across the entire time period 
  filter(n > 150) %>%
  # rename the count variable
  rename(event_by_region = n) %>%
  ungroup() %>% 
  # shorten the names of particularly long regions
  mutate(region = ifelse(
    region == "Africa and Red Sea",
    "Africa/Red Sea",
    ifelse(region == "Canada and Western USA", "Canada & US", region)
  ))

##--------------------##
## Data Visualization ##
##--------------------##

ggplot(volcano_data,
       aes(
         y = event_by_region,
         axis1 = stringr::str_wrap(event_type, 15), # make sure that long strings fit in the stratum
         axis2 = stringr::str_wrap(region, 15)
       )) +
  # create the sankey diagram using ggalluvium
  geom_alluvium(aes(
    fill = event_type,
    color = event_type,
    alpha = event_by_region
  ),
  width = 1 / 12) +
  # modify the appearance of the stratum
  geom_stratum(width = 0.1, fill = "#cfd3cd") +
  geom_text(stat = "stratum",
            infer.label = TRUE,
            size = 2) +
  # add the most volcano-color-like palette from RColorBrewers
  scale_colour_brewer(palette = "YlOrRd") +
  scale_fill_brewer(palette = "YlOrRd") +
  # add the axis titles as annotations on top
  annotate(
    "text",
    x = 1,
    y = 21500,
    size = 2,
    hjust = 0.5,
    label = "observed\n activities",
    color = "white",
    family = "frijole"
  ) +
  annotate(
    "text",
    x = 2,
    y = 21500,
    size = 2,
    hjust = 0.5,
    label = "geographic \n location",
    color = "white",
    family = "frijole"
  ) +
  # add title, subtitle and footnote
  labs(title = "Volcanos",
       subtitle = "Most common types of activity and where they occur",
       caption = "Visualization: Dennis Hammerschmidt \n \n") +
  # modify the appearance of the graph
  theme_void() +
  theme(
    plot.title = element_text(
      color = "white",
      size = 40,
      hjust = 0.5,
      family = "frijole"
    ),
    plot.subtitle = element_text(
      color = "white",
      size = 10,
      hjust = 0.5,
      family = "frijole"
    ),
    plot.caption = element_text(
      color = "white",
      size = 8,
      hjust = 0.5,
      family = "frijole"
    ),
    plot.background = element_rect(fill = "#474b4e", colour = "#474b4e"),
    legend.position = "none"
  ) 

# save the final graph
ggsave(
  "week20_2020/final_graph.png",
  dpi = 320,
  width = 20,
  height = 30,
  unit = "cm"
)
