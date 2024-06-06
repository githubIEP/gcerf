# load data, ukraine only, ppi 2024

ukriane_data_ppi <- rio::import("~/GitHub/gcerf/02_data/ukriane_data_ppi.xlsx")
ukriane_data_ppi <- ukriane_data_ppi%>%
  select(year, Attitudes, Structures, Institutions)

attitudes_2009 <- ukriane_data_ppi$Attitudes[ukriane_data_ppi$year == 2009]
institutions_2009 <- ukriane_data_ppi$Institutions[ukriane_data_ppi$year == 2009]
structures_2009 <- ukriane_data_ppi$Structures[ukriane_data_ppi$year == 2009]

# Divide all values in each column by the respective value in the year 2009
ukriane_data_ppi <- ukriane_data_ppi %>%
  mutate(
    iAttitudes = Attitudes / attitudes_2009,
    iInstitutions = Institutions / institutions_2009,
    iStructures = Structures / structures_2009
  )

#------------------------------------------------------------------------------------------------
# Plot the trendline graph (attitudes, institutions, structures)
ggplot(ukriane_data_ppi, aes(x = year)) +
  geom_line(aes(y = Attitudes, color = "Attitudes"), size = 1.5) +
  geom_line(aes(y = Structures, color = "Structures"), size = 1.5) +
  geom_line(aes(y = Institutions, color = "Institutions"), size = 1.5) +
  scale_color_manual(values = c("Attitudes" = "#CC3300", "Structures" = "#6699CC", "Institutions" = "#009966"),
                     labels = c("Attitudes", "Structures", "Institutions"),
                     name = NULL) + # Remove legend title
  labs(x = "", y = "PPI Theme Score
  More peaceful                       Less peaceful", title = "",
       caption = "Source:IEP") +
  theme_minimal() +
  theme(legend.position = "top") # Place legend on top

#-------------------------------------------------------------------------------------------------------------------
####---- indexed trend, ppi themes ---------------------------------------------------------------------------------------------------
CHART_PpiThemes_Indexed<- ggplot(ukriane_data_ppi, aes(x = year)) +
  geom_line(aes(y = iAttitudes, color = "Attitudes"), size = 1.5) +
  geom_line(aes(y = iStructures, color = "Structures"), size = 1.5) +
  geom_line(aes(y = iInstitutions, color = "Institutions"), size = 1.5) +
  scale_color_manual(values = c("Attitudes" = "#CC3300", "Structures" = "#6699CC", "Institutions" = "#009966"),
                     labels = c("Attitudes", "Structures", "Institutions"),
                     name = "PPI Theme", # Add legend title
                     guide = guide_legend(override.aes = list(size = 2))) + # Adjust legend size
  labs(x = "Year", y = "PPI Theme Score(indexed, 1= value in 2009)
  More peaceful                       Less peaceful", title = "") +
  theme_minimal() +
  theme(legend.position = c(0.9, 0.9))+ # Adjust legend position
  ylim(0.8, 1.1)+
  xlim(2009,2022)

print(CHART_PpiThemes_Indexed)

#--------------------------------------------------------------------------------------------------------------
####---- trendline, all pillars, Ukriane graph-------------------------------------------------------------------------------------
ukriane_data_ppi <- rio::import("~/GitHub/gcerf/02_data/ukriane_data_ppi.xlsx")

# linegraphs_1 acceptance of the rigths of others
plot1<- ggplot(ukriane_data_ppi, aes(x = year, y = `Acceptance of the Rights of Others`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "Acceptance of the Rigths of Others",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

# linegraphs_1 low levels of corruption
plot2<- ggplot(ukriane_data_ppi, aes(x = year, y = `Low Levels of Corruption`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "Low Levels of Corruption",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

# linegraphs_1 sound business environment
plot3<- ggplot(ukriane_data_ppi, aes(x = year, y = `Sound Business Environment`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "Sound Business Environment",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

# linegraphs_1 Well-functioning governmnet
plot4<- ggplot(ukriane_data_ppi, aes(x = year, y = `Well-Functioning Government`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "Well-Functioning Government",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

# linegraphs_1 Equitable distribution of resources
#df <- data.frame(
#  year = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
#  `Equitable_Distribution_of_Resources` = c(1.966, 1.966, 1.959, 1.916, 1.906, 1.898, 1.887, 1.876, 1.872, 1.866, 1.866, 1.865, 1.867, 1.867)
#) data from dashboard
plot5<- ggplot(ukriane_data_ppi, aes(x = year, y = `Equitable Distribution of Resources`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "Equitable Distribution of Resources",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

# linegraphs_1 free flow of information
plot6<- ggplot(ukriane_data_ppi, aes(x = year, y = `Free Flow of Information`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "Free Flow of Information",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

# linegraphs_1 free flow of information
#dfdf <- data.frame(
#  year = c(2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022),
#  `Good_Relations_with_Neighbours` = c(3.262, 3.284, 3.282, 3.255, 3.240, 3.371, 3.300, 3.292, 3.243, 3.191, 3.185, 3.299, 3.299, 3.299)
#) data from dashboard
plot7<- ggplot(ukriane_data_ppi, aes(x = year, y = `Good Relations with Neighbours`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "Good Relations with Neighbours",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

# linegraphs_1 high levels of human capital
plot8<- ggplot(ukriane_data_ppi, aes(x = year, y = `High Levels of Human Capital`)) +
  geom_line(color = "#0072B2", size = 1.5) +  # Add line with custom color and thickness
  labs(x = "Year", y = "Score (1= High, 5= Low)", title = "High Levels of Human Capital",
       caption = "Source:IEP") +  # Add axis labels and title
  theme_minimal() +  # Apply minimal theme
  theme(  # Customize theme
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),  # Title settings
    axis.title.x = element_text(size = 12),  # X-axis label settings
    axis.title.y = element_text(size = 12),  # Y-axis label settings
    axis.text = element_text(size = 10)  # Axis text settings
  )

library(gridExtra)
CHART_PpiPillarsUkraineInOne<- grid.arrange(plot1, plot2, plot3, plot4, plot5, plot6, plot7, plot8, ncol = 3)

print(CHART_PpiPillarsUkraineInOne)
