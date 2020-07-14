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
  c("tidyverse", "ggbump", "lubridate", "countrycode", "magrittr", "sysfonts")
install_and_load(packages)

# load from github the ggflags package to use flag icons in the graph

devtools::install_github("rensa/ggflags")
library(ggflags)

# additionally, load the tidytuesdayR package from github
devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)

# load the Sonsie One font from google for the final graph
font_add_google(name = "Sonsie One", family = "sonsie-one")
showtext_auto()

##------------------##
## Read in the data ##
##------------------##

tuesdata <- tidytuesdayR::tt_load(2020, week = 15)
tdf_winners <- tuesdata$tdf_winners

##------------------##
## Data Preparation ##
##------------------##

tdf_winners %<>%
  filter(edition >= 37) %>% # restrict to post-WWII period
  mutate(decade = year(lubridate::floor_date(ymd(start_date), years(10)))) %>% # create decades from start_date variable
  group_by(decade, nationality) %>%
  summarise(wins = sum(!is.na(nationality))) %>% # count the total number of wins per decade
  ungroup() %>%
  group_by(decade) %>%
  mutate(rank = rank(-wins, ties.method = "random")) %>% # create a rank of countries for each decade based on total wins
  ungroup()

# prepare a dataset to use two-letter countrycodes for the ggflags package
country_2_letters <-
  countrycode(
    tdf_winners$nationality %>% unique() %>% sort(),
    origin = "country.name",
    destination = "genc2c"
  ) %>%
  tolower() %>%
  set_names(tdf_winners$nationality %>% unique() %>% sort())

tdf_winners %<>%
  mutate(nationality_letters = country_2_letters[nationality])

# define the final decade where a country won at least one TDF title to place the country flags at this point
first_time <- tdf_winners %>%
  group_by(nationality) %>%
  filter(decade == max(decade))

##--------------------##
## Data Visualization ##
##--------------------##

tdf_winners %>%
  ggplot(aes(
    decade,
    rank,
    group = nationality,
    color = nationality,
    fill = nationality
  )) +
  # geom_bump creates the tracing lines for countries over time
  geom_bump(aes(smooth = 1), size = 0.5, lineend = "round") +
  geom_segment(
    data = tdf_winners %>% filter(rank <= 5),
    aes(
      x = decade - .1,
      xend = decade + .1,
      y = rank,
      yend = rank
    ),
    size = 4,
    lineend = "round"
  ) +
  # adds the flags of countries to the final decade of a country's appeareance/win
  geom_flag(
    data = first_time,
    aes(country = nationality_letters),
    size = 12,
    color = "black"
  ) +
  # add the number of total wins per decade
  geom_label(
    aes(
      x = decade + 1.45,
      y = rank,
      label = wins,
      fill = nationality
    ),
    color = "white",
    label.size = 0,
    family = "sonsie-one"
  ) +
  # modify the total wins for the Netherlands and Luxembourg as their country colors are white
  geom_label(
    data = tdf_winners %>% filter(nationality_letters == "nl" |
                                    nationality_letters == "lu"),
    aes(
      x = decade + 1.35,
      y = rank,
      label = wins,
      fill = nationality
    ),
    color = "black",
    label.size = 0,
    family = "sonsie-one"
  ) +
  # reverse the y-scale to have rank no. 1 on top
  scale_y_reverse() +
  # modify the apperance of decades on the x-axis
  scale_x_continuous(breaks = seq(1950, 2010, 10), labels=c("1950s", "'60s", "'70s", "'80s", "'90s", "2000s", "10s")) +
  # define custom colors for the tracing lines
  scale_colour_manual(
    values = c(
      "black",
      "black",
      "#3D4247",
      "black",
      "black",
      "#3E8AC3",
      "black",
      "black",
      "black",
      "#74A93C",
      "white",
      "white",
      "#FFCF02",
      "#EF4A59"
    )
  ) +
  # define custom colors for the labels of total wins
  scale_fill_manual(
    values = c(
      "#646b63",
      "#646b63",
      "#3D4247",
      "#646b63",
      "#646b63",
      "#3E8AC3",
      "#646b63",
      "#646b63",
      "#646b63",
      "#74A93C",
      "white",
      "white",
      "#FFCF02",
      "#EF4A59"
    )
  ) +
  theme_classic() +
  labs(title = "Le Tour de France: Ranking of Performance by Country and Decade",
       subtitle = "Visualization by: Dennis Hammerschmidt") +
  theme(
    legend.position = "none",
    plot.title = element_text(family = "sonsie-one", size = 20, color = "white"),
    plot.subtitle = element_text(family = "sonsie-one", size = 10, color = "white"),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(face = 2, color = "white", size = 17, family = "sonsie-one"),
    axis.text.y = element_blank(),
    panel.background = element_rect(fill = "#646b63"),
    plot.background = element_rect(fill = "#646b63")
  ) +
  # add the label information
  annotate(
    "curve",
    x = 1997,
    y = 4.4,
    xend = 1992,
    yend = 4.95,
    curvature = -0.3,
    arrow = arrow(length = unit(3, "mm")),
    color = "white"
  ) +
  annotate(
    "label",
    x = 1997,
    y = 4.3,
    label = "Wins per decade",
    color = "black",
    family = "sonsie-one"
  ) +
  annotate(
    "curve",
    x = 2002,
    y = 3.8,
    xend = 2000,
    yend = 4.95,
    curvature = -0.2,
    arrow = arrow(length = unit(3, "mm")),
    color = "white"
  ) +
  annotate(
    "label",
    x = 2002,
    y = 3.65,
    label = "Line tracks countries' \n performance over time",
    color = "black",
    family = "sonsie-one"
  ) +
  annotate(
    "curve",
    x = 2001,
    y = 2.8,
    xend = 1991,
    yend = 2.9,
    curvature = 0.3,
    arrow = arrow(length = unit(3, "mm")),
    color = "white"
  ) +
  annotate(
    "label",
    x = 2001,
    y = 3,
    label = "Countries that had wins \n in only one decade",
    color = "black",
    family = "sonsie-one"
  )

# save the final graph
ggsave("week15_2020/final_graph.png",
       dpi = 320,
       width = 38,
       height = 20,
       unit = "cm")
