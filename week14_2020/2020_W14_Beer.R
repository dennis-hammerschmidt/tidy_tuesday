#################
## TidyTuesday ##
## 2020 -- W14 ##
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
  c("tidyverse",
    "maps",
    "extrafont",
    "tidylog",
    "magrittr",
    "showtext",
    "emojifont")
install_and_load(packages)

# load the fontawesome library to display the beer symbol on the map & add custom font
emojifont::load.fontawesome()
font_add("raleway-extralight", "Raleway-ExtraLight.ttf")

# set the output of exponential to "readable" numbers
options(scipen = 999)

# additionally, load the tidytuesdayR package from github
devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)

##------------------##
## Read in the data ##
##------------------##

# load the state-wise data for beer production
beer_states <-
  readr::read_csv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv'
  )

# load US city names data and state names
data(us.cities)
data("state")

# load the US states map data
map <- map_data("state")

##-------------------##
## Data Manipulation ##
##-------------------##

# create a dataset that contains US state names and their abbrevations
state_name <- tolower(state.name)
us_states <- cbind(state.abb, state_name)
us_states <- as.data.frame(us_states)

# select only capital cities from the us.cities dataset (this is where the beer glas icons should be placed in the map)
cap_only <- us.cities %>%
  filter(capital == 2) %>%
  select(-pop, -capital)

# merge the capital dataset with the US state names dataset
us_map_states <-
  left_join(cap_only, us_states, by = c("country.etc" = "state.abb"))

# remove Hawaii and Alaska
us_map_states %<>% filter(state_name != "hawaii" &
                            state_name != "alaska")

# summarize beer production across production type for each year and state (minus Alaska, Hawaii and the total amount)
beer_states %<>%
  filter(state != "AK" & state != "Hi" & state != "total") %>%
  group_by(state, year) %>%
  summarize(barrels_total = sum(barrels)) %>%
  ungroup() %>%
  filter(!is.na(barrels_total))

# merge the beer_states dataset with the us_map_states data
us_map_with_beer <-
  left_join(beer_states, us_map_states, by = c("state" = "country.etc"))

# set NA values to 0 for visualization
us_map_with_beer$barrels_total <-
  ifelse(is.na(us_map_with_beer$barrels_total),
         0,
         us_map_with_beer$barrels_total)

##-------------------##
## Map Visualization ##
##-------------------##

# create the base map for the US using the default map dataset
base_map <- ggplot(map, aes(long, lat, group = group)) +
  geom_polygon() +
  coord_map() +
  theme_void()

# modify the base map to include information on total barrel production and display as beer glas icon
base_map + geom_text(
  data = us_map_with_beer,
  aes(x = long, y = lat, size = barrels_total),
  label = emojifont::fontawesome("fa-beer"),
  color = "#F6C101",
  inherit.aes = FALSE,
  family = "fontawesome-webfont",
  na.rm = TRUE
) +
  # produce maps for each year
  facet_wrap(~ year) +
  # add labels and titles
  labs(
    title = "US Beer Production by State, 2008 - 2019",
    subtitle = "The visualization shows the total beer producation for each state and year across all three production types (On Premises, Kegs and Barrels, Bottles and Cans) \n",
    caption = "Visualization: Dennis Hammerschmidt (@ d_hammers) | Data: TTP Beer Statistics   \n",
    size = "Barrels produced (total)"
  ) +
  scale_size_continuous(breaks=c(2500000, 5000000, 7500000, 10000000, 12500000, 15000000, 17500000, 20000000, 22500000, 25000000)) +
  # modify the theme
  theme(
    plot.title = element_text(
      color = "#F6C101",
      size = 20,
      hjust = 0.02
    ),
    plot.subtitle = element_text(
      color = "#fffff8",
      size = 10,
      hjust = 0.04
    ),
    plot.caption = element_text(
      color = "#fffff8",
      size = 8,
      vjust = 0.2
    ),
    text = element_text(family = "raleway-extralight"),
    plot.background = element_rect(fill = "#757575", colour = "#757575"),
    panel.background = element_rect(fill = "#757575",
                                    colour = "#757575"),
    legend.background = element_rect(fill = "#757575",
                                     colour = "#757575"),
    legend.position = "bottom",
    legend.title = element_text(colour = '#fffff8'),
    legend.text = element_text(colour = '#fffff8'),
    strip.text = element_text(colour = '#fffff8')
  )

# save the final graph
ggsave(
  "week14_2020/final_graph.png",
  dpi = 320,
  width = 40,
  height = 20,
  unit = "cm"
)