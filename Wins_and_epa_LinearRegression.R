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

merged_df <- read.csv("data/win_loss_playoff_outcomes_epa.csv")


lim_model <- lm(wins ~ total_epa, data = merged_df)

## Print the summary of the model
#For wins ~ total_epa however, we have a nice linear regression line
summary(lim_model)

plot <- ggplot2::ggplot(merged_df, aes(x = total_epa, y = wins)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(x = "Total EPA", 
       y = "Wins",
       title = "Number of wins by total EPA",
       caption = "Data from nflfastR") +
  theme_gray() +
  theme(plot.title = element_text(size = 10,
                                  hjust = 0.5,
                                  face = "bold")) +
  scale_y_continuous(breaks = seq(0, 16, by = 2))

plot

#We can extract even more data from our model summary, such as the deviance, residuals, AIC, etc
summary(lim_model)

#Saving our linear regression model plot
ggplot2::ggsave("Playoff_and_wins_plots/Wins_to_make_playoffs.png", plot)

write.csv(capture.output(summary(lim_model)), file = "data/lim_model_wins~total_epa.csv", row.names = FALSE)

