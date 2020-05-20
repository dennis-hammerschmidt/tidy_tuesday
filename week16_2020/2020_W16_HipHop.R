#################
## TidyTuesday ##
## 2020 -- W16 ##
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
  c("tidyverse", "extrafont", "showtext", "stringr", "magrittr")
install_and_load(packages)

# additionally, load the tidytuesdayR package from github
devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)

remotes::install_github("wilkelab/ggtext")
library(ggtext)

# load the Sonsie One font from google for the final graph
font_add_google(name = "Monoton", family = "monoton")
font_add_google(name = "Montserrat Alternates", family = "rum-raisin")
showtext_auto()

##------------------##
## Read in the data ##
##------------------##

# get the data for this week's tidytuesday
tuesdata <- tidytuesdayR::tt_load(2020, week = 16)
rankings <- tuesdata$rankings

# create a vector of artists that I consider as Old School HipHop (controversial, I know, but that's what it is ;))
old_school <-
  c(
    "2Pac",
    "The Notorious B.I.G.",
    "NWA",
    "Dr Dre",
    "Snoop Dogg",
    "Ice Cube",
    "Wu-Tang Clan",
    "Nas",
    "Run DMC",
    "Public Enemy",
    "A Tribe Called Quest",
    "DMX",
    "Rakim",
    "Eric B",
    "Outkast",
    "Method Man",
    "Gang Starr",
    "The Lox",
    "112",
    "Slick Rick",
    "Killah Priest",
    "Mobb Deep"
  )

##------------------##
## Prepare the data ##
##------------------##

# There are several artists that occur as features or collaborations with each other. I want to find out the impact of
# an artist itself, so including their appearances on featured tracks. To separate and extend the dataset with all 
# collaborations split up, I use separate_rows()
rankings %<>%
  separate_rows(artist, sep = " ft. | & | and ")

# This allows me know to count the total number of occurrences by artist. Before, there needs to be some house cleaning
# for misspelled names or artist that changed their name over the past years.
occurrences <- rankings %>%
  mutate(artist = str_replace(artist, " Doggy", "")) %>%
  mutate(artist = ifelse(artist == "OutKast", "Outkast", artist)) %>%
  group_by(artist) %>%
  count()

# It's now possible to create the final dataset where rankings and occurrences are merged, artists with less than or
# equal to 10 points in total are filtered out (as well as those with only 1 occurrence), and the hand-coded old_school
# variable is added.
hiphop_df <- rankings %>%
  group_by(artist) %>%
  summarize(total_points = sum(points)) %>%
  left_join(., occurrences, by = "artist") %>%
  filter(total_points >= 10) %>%
  mutate(old_school = as.factor(ifelse(artist %in% old_school, 1, 0))) %>% 
  # to ensure that the lollipop graph is ordered by total points, artists needs to be re-oreded
  mutate(artist = fct_reorder(artist, total_points)) %>%
  filter(n > 1)

# Quick and dirty: create an ordered color vector in the same order that the artists appear in the graph in order to
# also color their axis ticks labels in black and gold 
colors <- hiphop_df %>%
  mutate(color = ifelse(old_school == "1", "#f1af09", "black"))
colors <- colors$color[order(colors$total_points)]

##--------------------##
## Data Visualization ##
##--------------------##

hiphop_df %>%
  ggplot(aes(x = artist, y = total_points)) +
  # create the sticks for the lollipop
  geom_segment(aes(
    x = artist,
    xend = artist,
    y = 0,
    yend = total_points,
    color = old_school
  ), size = 0.75) +
  # create the bubbles at the end
  geom_point(aes(size = n, color = old_school)) +
  # display the total number of occurrences only for those artists that have more than 4 songs in the BBC list
  geom_text(data = hiphop_df %>% filter(n > 4), aes(label = n), family = "rum-raisin", nudge_x = 0.1, nudge_y = 6, color = "white", size = 2) +
  # create a horizontal lollipop chart
  coord_flip() +
  # black and gold
  scale_colour_manual(values = c("black", "#f1af09")) +
  # aesthetics and apperance
  labs(title = "Old School HipHop still has it",
       subtitle = "<br/> The graph shows total points allocated for HipHop artists by BBC Music's <br/> annual critics choice. <span style='color:#f1af09;'>Gold</span> indicates artists that can be classified as Old <br/> School HipHop. <span style='color:white;'>White numbers</span> show the total number of songs per artist <br/> in the ranking. <br/> <br/> **Lines** indicate total number of points, **dots** total number of songs by artist. <br/>",
       caption = "Visualization: Dennis Hammerschmidt",
       y = "Total Points by BBC Music Critics") +
  theme_bw()+
  theme(
    legend.position = "none",
    plot.title = element_text(
      color = "#f1af09",
      size = 25,
      hjust = 0.02,
      family = "monoton"
    ),
    plot.subtitle = element_markdown(
      color = "black",
      size = 12,
      #hjust = 0.04,
      family = "rum-raisin"
    ),
    plot.caption = element_markdown(
      color = "#f1af09",
      size = 8,
      #hjust = 0.04,
      family = "rum-raisin"
    ),
    axis.title.x = element_markdown(family = "rum-raisin", color = "black"),
    axis.title.y = element_blank(),
    axis.text.x = element_text(family = "rum-raisin", color = "black"),
    axis.text.y = element_text(family = "rum-raisin", margin = margin(r = -23), color = colors),
    panel.grid = element_blank(),
    axis.line.x = element_blank(),
    axis.line.y = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    plot.background = element_rect(fill = "#374447", colour = "#374447"),
    panel.background = element_rect(fill = "#374447",
                                    colour = "#374447")
  )+
  annotate(
    "text",
    x = 35,
    y = 150,
    label = "Modern artists need way \n more songs to get to the top.",
    color = "white",
    family = "rum-raisin"
  ) +
  annotate(
    "curve",
    x = 33.5,
    y = 120,
    xend = 35,
    yend = 93,
    curvature = -0.5,
    arrow = arrow(length = unit(3, "mm")),
    color = "white"
  ) +
  annotate(
    "curve",
    x = 36.5,
    y = 150,
    xend = 41,
    yend = 141,
    curvature = 0.3,
    arrow = arrow(length = unit(3, "mm")),
    color = "white"
  )

# save the final graph
ggsave(
  "week16_2020/final_graph.png",
  dpi = 320,
  width = 20,
  height = 25,
  unit = "cm"
)
