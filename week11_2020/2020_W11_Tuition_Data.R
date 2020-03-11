#################
## TidyTuesday ##
## 2020 -- W11 ##
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
  c("tidyverse", "openintro", "viridis", "scales", "patchwork")
install_and_load(packages)

# additionally, load the tidytuesdayR package from github
devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)

##------------------##
## Read in the data ##
##------------------##

# load the tidytuesday data for W11 and use the tuition_income data
tuesdata <- tidytuesdayR::tt_load(2020, week = 11)
tuition_income <- tuesdata$tuition_income

# to get the information on the regions for each US state, I use the baseR dataset "state"
data("state")
# and extract and combine state abbrevations and regions from the data
us_regions <- cbind.data.frame(state.abb, state.region)
# I rename variables and ensure that it is sorted alphabetically (by state name)
us_regions <- us_regions %>%
  rename(state = state.abb, region = state.region) %>%
  arrange(state)

##-----------------##
## Preparethe data ##
##-----------------##

# the following loop creates a dataset for each tuition_income year as well as one that contains labels for the 
# visualization 
for (i in 2010:2018) {
  # tuition_income datasets
  tuition_year_i <- tuition_income %>%
    filter(year == i, state != "DC") %>% # exclude DC from the list of states
    group_by(state) %>%
    summarize(net_cost_avg = mean(net_cost)) # calculate the average net costs to study for each state and year
  tuition_year_i <-
    dplyr::left_join(tuition_year_i, us_regions, by = "state") # merge with the us_regions data
  assign(paste("tuition_income", i, sep = "_"), tuition_year_i) # save the output for each year as a separate dataframe
  
  # label_data datasets
  label_data_i <- tuition_year_i %>%
    arrange(-net_cost_avg) %>%
    mutate(net_cost_simple = sprintf("$%5.0f", net_cost_avg)) # I create a simpler description of the average net costs
  # these lines of code ensure that the labels for both the state names and the simplified average net costs are 
  # smoothly aligned with the direction of the bars
  number_of_bar <- nrow(label_data_i)
  label_data_i$row_num <- seq.int(nrow(label_data_i))
  angle <-
    90 - 360 * (label_data_i$row_num - 0.5) / number_of_bar # get the angle
  label_data_i$angle <-
    ifelse(angle < -90, angle + 180, angle) # modify it to fit the ciruclar bar plot structure
  label_data_i$angle <- label_data_i$angle * (-1)
  assign(paste("label_data", i, sep = "_"), label_data_i) # save the output for each year as a separate dataframe
}

##---------------##
## Visualization ##
##---------------##

# circular bar plot function that takes as an input the tuition_income dataset, the corresponding label_data 
# and the respective year
circular_bar <- function(data, label, year) {
    ggplot(data) +
      geom_bar(
        aes(
          x = fct_reorder(as.factor(state), net_cost_avg),
          y = net_cost_avg,
          fill = region
        ),
        stat = "identity",
        alpha = 0.5
      ) +
      # appearance of the graph
      scale_fill_viridis(discrete = TRUE) +
      theme_minimal() +
      theme(
        legend.position = c(0.8, 0.8),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.margin = unit(rep(-1, 4), "cm")
      ) +
      # set the coordinations of the labels according to the respective angles
      coord_polar() +
      geom_text(
        data = label,
        aes(
          x = as.factor(state),
          y = net_cost_avg + 1700,
          # labels for the states should be a bit above the bars
          label = state
        ),
        size = 3,
        angle = label$angle
      ) +
      geom_text(
        data = label,
        aes(
          x = as.factor(state),
          y = net_cost_avg - 2700,
          # the simplified net costs should be within the bars
          label = net_cost_simple
        ),
        size = 2,
        angle = label$angle
      ) +
      # add the year in the center of each plot
      annotate("text",
               x = 1,
               y = 0,
               label = year)
}

# apply to any combination of data and label sets
# for the final output, I use all 9 plots but, in principle, one could also look at each plot separately or use 
# any other combination of plots that one is interested (e.g., every two years)
plot1 <- circular_bar(tuition_income_2010, label_data_2010, 2010)
plot2 <- circular_bar(tuition_income_2011, label_data_2011, 2011)
plot3 <- circular_bar(tuition_income_2012, label_data_2012, 2012)
plot4 <- circular_bar(tuition_income_2013, label_data_2013, 2013)
plot5 <- circular_bar(tuition_income_2014, label_data_2014, 2014)
plot6 <- circular_bar(tuition_income_2015, label_data_2015, 2015)
plot7 <- circular_bar(tuition_income_2016, label_data_2016, 2016)
plot8 <- circular_bar(tuition_income_2017, label_data_2017, 2017)
plot9 <- circular_bar(tuition_income_2018, label_data_2018, 2018)


# I use the patchwork package (finally, for the first time!) to create two subplots with 5 figures each
# final_plot1 contains the plots for the years 2018 to 2014
final_plot1 <-
  (plot9 |
     plot8 |
     plot7 |
     plot6 |
     plot5) + plot_layout(guides = "collect") &
  theme(legend.position = "none") # while I collect the legends from each plot, I do not want them to appear in the 
# first row of my circular bar plots

# final_plot2 the remaining ones for 2013 to 2010 plus the legend
final_plot2 <-
  (plot4 |
     plot3 |
     plot2 | plot1 | guide_area()) + plot_layout(guides = "collect") # using guide_area() places the collected legend
# in the fifth spot of the second orw

# combine the two final_plots below each other and add a title and a caption
final_plot <- final_plot1 / final_plot2 &
  theme(plot.title = element_text(hjust = 0.5)) &
  plot_annotation(title = 'Average Net Costs for Studying by State and Year',
                  caption = '#tidytuesday Week 11: Tuitiontracker.org \n Viz: @d_hammers')

# save the plot as a png with sufficently high resolution and a useful height and width
ggsave(
  "final_plot.png",
  width = 40,
  height = 20,
  units = "cm",
  dpi = 250
)
