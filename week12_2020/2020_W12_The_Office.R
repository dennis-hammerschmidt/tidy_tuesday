#################
## TidyTuesday ##
## 2020 -- W12 ##
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
  c("tidyverse", "schrute", "caret", "paletteer", "patchwork")
install_and_load(packages)

# additionally, load the tidytuesdayR package from github
devtools::install_github("thebioengineer/tidytuesdayR")
library(tidytuesdayR)

##------------------##
## Read in the data ##
##------------------##

# load the IMBD The offcie ratings dataset
tuesdata <- tidytuesdayR::tt_load(2020, week = 12)
office_ratings <- tuesdata$office_ratings

# use the transcripts of all The Office episodes from the schrute package
office_transcripts <- schrute::theoffice

##-----------------##
## Preparethe data ##
##-----------------##

# transform season and episode to numeric
office_transcripts$season <- as.numeric(office_transcripts$season)
office_transcripts$episode <- as.numeric(office_transcripts$episode)

# order transcripts (make them tidy!)
tidy_office <- office_transcripts %>% 
  group_by(season, episode, episode_name, character) %>% 
  count() %>% 
  arrange(desc(n))

# ge the top 10 characters on the office (those who have the most lines across all seasons)
top_10_chars <- tidy_office %>%
  group_by(character) %>%
  summarize(unique_char = sum(n)) %>%
  ungroup() %>%
  group_by(character, unique_char) %>%
  arrange(desc(unique_char)) 

top_10_chars <- top_10_chars[1:10,]
character_names <- as.character(top_10_chars$character)

# produce final dataframe
tidy_office <- tidy_office %>% filter(character %in% character_names)

tidy_office <- tidy_office %>% spread(key=character, value=n) %>% 
  replace(is.na(.),0) %>% 
  rename(title = episode_name)

# and merge ratings and transcripts datasets
the_office <- dplyr::left_join(tidy_office, office_ratings, by = c("season", "episode"))

# final data cleaning
the_office <- the_office %>%
  select(imdb_rating, everything()) %>% 
  select(-c(total_votes,air_date,title.y)) %>% 
  drop_na()

##--------------##
## Descriptives ##
##--------------##

# function to detect outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}

# Boxplot
office_boxplot <- the_office %>%
  group_by(season) %>%
  mutate(outlier = ifelse(is_outlier(imdb_rating), imdb_rating, as.numeric(NA))) %>% # specify outliers
  mutate(to_label = ifelse(!is.na(outlier), title.x, NA)) %>%
  ggplot(aes(as.factor(season), imdb_rating, fill = as.factor(season))) +
  geom_boxplot(
    outlier.colour = "red",
    outlier.shape = 8,
    outlier.size = 2
  ) +
  # add dots to the boxplots
  geom_dotplot(
    binaxis = 'y',
    stackdir = 'center',
    dotsize = .2,
    binwidth = 0.15
  ) +
  # add the titles to the outliers
  geom_text(
    aes(label = to_label),
    na.rm = TRUE,
    size = 2,
    position = position_nudge(y = -0.1)
  ) +
  # appearance of the graph
  theme_minimal() +
  theme(legend.position = "none") +
  xlab("Seasons") +
  ylab("IMDB Rating") +
  ggtitle("Average Ratings per Season") +
  paletteer::scale_fill_paletteer_d("nord::afternoon_prarie")

ggsave(
  "week12_2020/boxplot.png",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 250
)

##---------##
## XGBoost ##
##---------##

set.seed(68163)

# Remove episode and season columns
the_office_pred <- the_office[,-(2:4)]

# General trainControl params
tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 5, # with n folds 
  verboseIter = FALSE,
  allowParallel = TRUE 
)

#### XGBoost Tree ####

tune_grid <- expand.grid(
  nrounds = seq(from = 10, to = 200, by = 10), # number of boosting rounds
  eta = c(0.1, 0.2, 0.5, 0.6), # learning rate
  max_depth = c(2, 3, 4), # max. tree depth
  gamma = c(0, 0.1, 0.5, 0.7, 0.9, 1.0), # gamma values
  # keep constant
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# train the model
xgb_tune <- caret::train(
  imdb_rating ~ .,
  data = the_office_pred,
  trControl = tune_control, # use pre-defined controls
  tuneGrid = tune_grid, # use pre-defined tune grid
  method = "xgbTree",
  verbose = TRUE
)

# sneak peak the results and identify the minimum RMSE and the best model specification
head(xgb_tune$results)
min(xgb_tune$results$RMSE)
xgb_tune$bestTune

#### Model Evaluation Plot ####

# evaluate the model performance across all tuning parameters
tune_crosval <- ggplot(xgb_tune) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  ylim(0.44, 0.5) + 
  ggtitle("Model evaluation")

#### Feature Importance Plot ####

# define a function to extract the feature importance
feat_imp <- function(x){
  xgb_imp <- xgboost::xgb.importance(feature_names = x$finalModel$feature_names,
                                     model = x$finalModel)
  xgboost::xgb.ggplot.importance(xgb_imp) + 
    theme_minimal() +
    theme(legend.position = "none") +
    paletteer::scale_fill_paletteer_d("nord::afternoon_prarie")
}

xgb_tuneplot <- feat_imp(xgb_tune) + ggtitle("Feature Importance XGBoost Tree")

ggsave(
  "week12_2020/xgb_tune_plot.png",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 250
)

#### ICE and PDP Plot ####

# provide further information on Michael (as it appears to be by far the most important feature)
part_michael <-
  partial(
    xgb_tune,
    pred.var = "Michael",
    plot = TRUE,
    ice = TRUE,
    plot.engine = "ggplot2",
    alpha = 0.1
  ) + theme_minimal() +
  ggtitle("ICE Plot: Michael")

# also compare it to Dwight (second most important feature and life-long buddy!)
part_compare <-
  partial(
    xgb_tune,
    pred.var = c("Michael", "Dwight"),
    plot = TRUE,
    plot.engine = "ggplot2",
    chull = TRUE
  ) + theme_minimal()+
  theme(legend.position = "bottom") +
  ggtitle("PDP Plot: Michael & Dwight")

##-------------##
## Final Graph ##
##-------------##

final_graph <- (office_boxplot + xgb_tuneplot) / (tune_crosval + part_michael + part_compare)  &  
  plot_annotation(title = 'The Office: The legacy of Michael Scott',
                  caption = '#tidytuesday Week 12: The Office \n Viz: @d_hammers')

ggsave(
  "week12_2020/final-graph.png",
  width = 30,
  height = 20,
  units = "cm",
  dpi = 250
)

##------------------------------------------------------------------------------------------------------------#

##----------------------------------##
## Bonus: Compare to XGBoost Linear ##
##----------------------------------##

# Just to see how well the XGBoost Tree compares to an XGBoost Linear model

#### XGBoost Linear ####

# linear grid
linear_grid <- expand.grid(
  nrounds = seq(from = 10, to = 500, by = 10), # number of boosting iterations
  eta = c(0.2,0.5,0.1, 0.15),  # learning rate, low value means model is more robust to overfitting
  lambda = c(0.1, 0.5, 1), # L2 Regularization (Ridge Regression)
  alpha =  c(0.1, 0.5, 1) # L1 Regularization (Lasso Regression)
) 

xgb_fit <- train(
  imdb_rating ~ .,
  data = the_office_pred,
  method = "xgbLinear",
  trControl = tune_control, # use the same tune controls as above
  tuneGrid = linear_grid # but with a different tuning grid, specified for an xgboost linear model
)

# sneak peak the results
head(xgb_fit$results)
min(xgb_fit$results$RMSE)

# model evaluation
ggplot(xgb_fit) +
  theme_bw() +
  theme(legend.position = "bottom") +
  ylim(0.49,0.55)

# feature importance
xgb_linear_plot <- feat_imp(xgb_fit) + ggtitle("Feature Importance XGBoost Linear")

# Compare the two models
resamples <-
  resamples(list(XGBoost_Linear = xgb_fit, XGBoost_Tree = xgb_tune)) 
dotplot(resamples, metric = "RMSE") 
