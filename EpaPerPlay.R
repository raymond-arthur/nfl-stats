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


###Data visualization of offensive and defensive EPA per play in a season
##We aim to look at the expected points added (EPA) for offense and defense of each of the 32 teams
##for the 2022 season and plot the results.
#Note: posteam is the team with possesion of the ball at the start of the play 
#in standard 3 character team name abbreviation for teams (eg: BAL for Baltimore Ravens)

##Creating the data
#Grab the play by play from the year 2022, filter by regular reason, filter by team with possession (eg offense)
#load the play-by-play data for 2022 season
year <- c(2023)
pbp <- nflfastR::load_pbp(year) %>%                         
  #filter for only regular season games
  dplyr::filter(season_type == "REG") %>%
  #filter out only rushes and passes (no sp teams)
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))  

#Define offensive EPA by the mean of the EPA of the team on offense
offense <- pbp %>%                                          
  dplyr::group_by(team = posteam) %>%
  #strip NA values, take mean of EPA for offensive plays
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))       

defense <- pbp %>%
  dplyr::group_by(team = defteam) %>%
  #strip NA values, take mean of EPA for defensive plays
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))       


##Creating the plot(s)
#Create a plot of offenseEPA/play (x-axis) and defenseEPA/play (y-axis).
#Note: Negative defenseEPA/play is better while positive offenseEPA/play is better
plotEPA <- offense %>%
  #inner_join() strips away any x that doesn't have a matching y
  dplyr::inner_join(defense, by = "team") %>%  
  #graph offense EPA on x, defense EPA on y
  ggplot2::ggplot(aes(x = off_epa, y = def_epa)) +
    #add horizontal/vertical lines for mean values of off and def EPA
    nflplotR::geom_mean_lines(aes(y0 = off_epa, x0 = def_epa)) +
    #add team logos for data points
    nflplotR::geom_nfl_logos(aes(team_abbr = team),
                             width = 0.03, alpha = 0.7) +       
    labs(x = "Offense EPA/play", 
         y = "Defense EPA/play", 
         title = paste(year, "NFL Offensive and Defensive EPA per play"), caption = "Data from nflfastR") +                      
  
    #Since a negative defensive EPA per play is better, reverse the y-axis orientation so top right is best of both
    scale_y_reverse() +                                          

    theme_gray() +                                              
    theme(plot.title = element_text(size = 10,
                                    hjust = 0.5,
                                    face = "bold")              
          ) +
    
    annotate("text", 
             x = 0.1, y = -0.1,  vjust = 0.5, hjust = 0.5,
             label = "Good offense\n Good defense"
            ) + 
    annotate("text", 
             x = -0.1, y = 0.1,  vjust = 0.5, hjust = 0.5,
             label = "Bad offense\n Bad defense"
    ) +
    annotate("text", 
             x = -0.1, y = -0.1,  vjust = 0.5, hjust = 0.5,
             label = "Bad offense\n Good defense"
    ) + 
    annotate("text", 
             x = 0.1, y = 0.1,  vjust = 0.5, hjust = 0.5,
             label = "Good offense\n Bad defense"
    )


#generate the plot to visualize in RStudio and save plot locally in subfolder "EpaPerPlay_plots
plotEPA
ggplot2::ggsave(file.path("EpaPerPlay_plots", paste("EpaPerPlay_", year, ".png")), plot = plotEPA)


##For looping this for a year range:
#Creating the function generate_plot as
generate_EPAplot <- function(year) {
  #exact same code as above...
  pbp <- nflfastR::load_pbp(year) %>%                         
    dplyr::filter(season_type == "REG") %>%
    dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))  
  
  offense <- pbp %>%                                          
    dplyr::group_by(team = posteam) %>%
    dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))       
  
  defense <- pbp %>%
    dplyr::group_by(team = defteam) %>%
    dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))       
  
  plotEPA <- offense %>%
    dplyr::inner_join(defense, by = "team") %>%  
    ggplot2::ggplot(aes(x = off_epa, y = def_epa)) +
    nflplotR::geom_mean_lines(aes(y0 = off_epa, x0 = def_epa)) +
    nflplotR::geom_nfl_logos(aes(team_abbr = team),
                             width = 0.03, alpha = 0.7) +       
    labs(x = "Offense EPA/play", 
         y = "Defense EPA/play", 
         title = paste(year, "NFL Offensive and Defensive EPA per play"), caption = "Data from nflfastR") +                      
    scale_y_reverse() +                                          
    theme_gray() +                                              
    theme(plot.title = element_text(size = 10,
                                    hjust = 0.5,
                                    face = "bold")              
    ) +
    annotate("text", 
             x = 0.1, y = -0.1,  vjust = 0.5, hjust = 0.5,
             label = "Good offense\n Good defense"
    ) + 
    annotate("text", 
             x = -0.1, y = 0.1,  vjust = 0.5, hjust = 0.5,
             label = "Bad offense\n Bad defense"
    ) +
    annotate("text", 
             x = -0.1, y = -0.1,  vjust = 0.5, hjust = 0.5,
             label = "Bad offense\n Good defense"
    ) + 
    annotate("text", 
             x = 0.1, y = 0.1,  vjust = 0.5, hjust = 0.5,
             label = "Good offense\n Bad defense"
    )
  
  ggplot2::ggsave(file.path("EpaPerPlay_plots", paste("EpaPerPlay_", year, ".png")), plot = plotEPA)
}

#Basic for loop, with the range being having a min value of 1999 (when data started)
for (year in 1999:2022) {
  generate_EPAplot(year)
}


