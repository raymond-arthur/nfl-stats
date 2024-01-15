###Housekeeping
#clear all vars
rm(list=ls())

##Libraries and packages
#list of packages to install
packages_to_install <- c("tidyverse", "nflfastR", "nflplotR")

#check if packages already installed in library
for (package in packages_to_install) {                    
  if (!requireNamespace(package, quietly = TRUE)) {
    install.packages(package)
  }
}

#load packages from packages_to_install
for (package in packages_to_install) {                    
  library(package, character.only = TRUE)
}

##Documentation and information

#Documentation for nflfastR can be found here: https://www.nflfastr.com/
#or with the R function
# ?nflfastR


###D


##Multivariate logistic regression
win_loss_totals_allteams_allyears <- read.csv("data/win_loss_playoff_outcomes.csv")

#Loop over the following years
years <- c(2002:2020)

#Create an empty data frame to store the output of our loop
result_df <- data.frame()

# Loop over the years vector
for (year in years) {
  pbp <- nflfastR::load_pbp(year) %>%
    dplyr::filter(season_type == "REG" & !is.na(posteam) & (rush == 1 | pass == 1))

  # Define offensive EPA by the mean of the EPA of the team on offense
  offense <- pbp %>%
    dplyr::group_by(team = posteam) %>%
    dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))

  defense <- pbp %>%
    dplyr::group_by(team = defteam) %>%
    dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))

  # Add a column with the value from the year
  offense$season <- year
  defense$season <- year

  # Append the results to the main data frame
  result_df <- bind_rows(result_df, offense, defense)
}

#combine such that off_epa and def_epa appear in the same row for each team
result_df_combined <- result_df %>%
  dplyr::group_by(season, team) %>%
  dplyr::summarise(across(everything(), ~na.omit(.)))

merged_df <- merge(win_loss_totals_allteams_allyears, result_df_combined, by = c("season", "team"), all.x = TRUE)
# merged_df$total_epa <- rowSums(merged_df[, c("off_epa", "def_epa")], na.rm = TRUE)
merged_df$total_epa <- merged_df$off_epa - merged_df$def_epa

log_model <- glm(playoffs ~ wins + total_epa, data = merged_df, family = binomial)
lim_model <- lm(wins ~ total_epa, data = merged_df)

## Print the summary of the model
#For playoffs ~ wins + total_epa, wins is a predictor (as expected) whereas total_epa is not statistically significant
summary(log_model)
#For wins ~ total_epa however, we have a nice linear regression line
summary(lim_model)

log_plot <- ggplot2::ggplot(merged_df, aes(x = total_epa, y = wins)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Total EPA", 
       y = "Wins",
       title = "Linear Regression Plot",
       caption = "Data from nflfastR") +
  theme_gray() +
  theme(plot.title = element_text(size = 10,
                                  hjust = 0.5,
                                  face = "bold")) +
  scale_y_continuous(breaks = seq(0, 16, by = 2))



#We can extract even more data from our model summary, such as the deviance, residuals, AIC, etc
summary(lim_model)

#Saving our linear regression model plot
ggplot2::ggsave("Playoff_and_wins_plots/Wins_to_make_playoffs.png", plot)

write.csv(capture.output(summary(lim_model)), file = "data/lim_model_wins~total_epa.csv", row.names = FALSE)

