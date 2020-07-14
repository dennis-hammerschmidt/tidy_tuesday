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
  c("tidyverse", "ggridges", "sf", "rgeos", "extrafont", "showtext")
install_and_load(packages)

devtools::install_github("ropensci/rnaturalearthhires", force = TRUE)
library(rnaturalearthhires)
library(rnaturalearth)

# additionally, load the tidytuesdayR package from github
devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)

# load the Sonsie One font from google for the final graph
font_add_google(name = "VT323", family = "vt323")
showtext_auto()

##------------------##
## Read in the data ##
##------------------##

# main dataset for this week
gdpr_violations <-
  readr::read_tsv(
    'https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv'
  )

# external data: centroids of each country in the world (from https://developers.google.com/public-data/docs/canonical/countries_csv)
centroids <-
  readr::read_csv("/Users/dennis_hammerschmidt/Downloads/countries.csv")

##------------------##
## Prepare the data ##
##------------------##

# get the unique names of all countries in the dataset
gdpr_countries <- gdpr_violations %>%
  distinct(name) %>%
  pull()

# extract shape files from all countries in the dataset
countries_sf <-
  ne_countries(
    country = c(gdpr_countries, "Czechia"),
    scale = "large",
    returnclass = "sf"
  ) %>%
  select(name, geometry) %>%
  mutate(name = replace(name, name == "Czechia", "Czech Republic"))

# prepare the GDPR dataset
gdpr_violations %<>%
  group_by(name) %>%
  mutate(
    price_total = sum(price),
    price_label = case_when(
      round(price_total / 1e6) > 0 ~ paste0(round(price_total / 1e6), "M"),
      round(price_total / 1e5) > 0 ~ paste0(round(price_total / 1e6, 1), "M"),
      round(price_total / 1e3) > 0 ~ paste0(round(price_total / 1e3), "K"),
      price_total > 0 ~ paste0(round(price_total / 1e3, 1), " K"),
      TRUE ~ "0"
    )
  ) %>%
  # merge with the coutnries shape file
  left_join(countries_sf) %>%
  select(name, price_label, price_total, geometry)

# identify the centroid locations for plotting the bubbles
locations <- gdpr_violations %>%
  select(name, price_total, price_label) %>%
  left_join(centroids)

##--------------------##
## Data Visualization ##
##--------------------##

ggplot() +
  # create the map
  geom_sf(
    data = gdpr_violations,
    aes(geometry = geometry),
    fill = "#6B8B8D",
    colour = "#B6CDB1",
    size = 0.1
  ) +
  # fix the coordinates to only display Europe's main members (e.g., excluding Portugese islands)
  coord_sf(
    xlim = c(-27.5, 32.5),
    ylim = c(32.5, 72.5),
    expand = FALSE
  ) +
  # add points in relation to total fines per country
  geom_point(
    data = locations,
    aes(
      x = longitude,
      y = latitude,
      size = price_total,
      alpha = price_total
    ),
    color = "#F9D268"
  ) +
  # add the rounded amount
  geom_text(
    data = locations %>% filter(price_total > 0),
    aes(x = longitude - 0.5,
        y = latitude,
        label = price_label),
    check_overlap = TRUE,
    hjust = 0,
    vjust = 1,
    size = 3,
    color = "#324448"
  ) +
  # adjust the range of the points
  scale_size(range = c(0, 40)) +
  # add the title
  labs(title = "GDPR Fines in Europe",
       subtitle = "Total amount of fines by country since the introduction of the new GDPR regulation",
       caption = "Visualization: Dennis Hammerschmidt \n ") +
  # aestethics of the plot
  theme_void() +
  theme(
    plot.title = element_text(
      color = "#B6CDB1",
      size = 40,
      hjust = 0.02,
      family = "vt323"
    ),
    plot.subtitle = element_text(
      color = "#B6CDB1",
      size = 12,
      hjust = 0.04,
      family = "vt323"
    ),
    plot.caption = element_text(
      color = "#B6CDB1",
      size = 8,
      vjust = -0.1,
      family = "vt323"
    ),
    plot.background = element_rect(fill = "#324448", colour = "#324448"),
    panel.background = element_rect(fill = "#324448",
                                    colour = "#324448"),
    legend.position = "none"
  )

# save the final graph
ggsave(
  "week17_2020/final_graph.png",
  dpi = 320,
  width = 20,
  height = 15.75,
  unit = "cm"
)
