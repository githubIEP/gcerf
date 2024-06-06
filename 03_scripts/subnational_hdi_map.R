# hdi
hdi<- iepg_search()

# Get Shapefile
ukr.map <- iepg_get_gadm("level1") %>%
  filter(str_detect (geocode, "UKR")) # 

# subnational hdi for females
sdhi_f<- iepg_get(19846)
sdhi_f <- sdhi_f %>%
  dplyr:: filter(grepl("UKR", geocode))
sdhi_f<- sdhi_f%>% filter (year == 2021)

# subnational hdi for males
sdhi_m<- iepg_get(19849)
sdhi_m<- sdhi_m %>%
  dplyr:: filter(grepl ("UKR", geocode))
sdhi_m<- sdhi_m%>% filter (year== 2021)

merged_data <- sdhi_f %>%
  inner_join(sdhi_m, by = c("geocode", "geoname", "admin_level", "year", "income", "population"))

# calculate average (female and male) to arrive with one subnational hdi value
merged_data <- merged_data %>%
  mutate(value = (value.x + value.y) / 2)

f_ScoringBands <- function(value, bands) {
  cut(value, breaks = bands, include.lowest = TRUE)
}
# Example HDI_BANDS, you should replace this with your actual bands
HDI_BANDS <- c(-Inf, 0.55, 0.7, 0.8, 0.9, Inf)


# Tidy Data
UKR_HDI.df <- merged_data%>%# Tidy Data as appropriate
  mutate(map_bands = f_ScoringBands(merged_data$value, HDI_BANDS)) # make sure the function arguements are named correctly


# Map Bands for latest year
ukr.map <- ukr.map %>%
  left_join(UKR_HDI.df, by = ("geoname"))

# Base Plot
MAP_subnat_hdi_ukr <- ggplot(data = ukr.map) +
  geom_sf(aes(fill = value), color = "white") +  # Specify fill based on the 'value' column
  theme_void() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    legend.position = c(0.2, 0.1),  # Specify legend position here
    legend.direction = "horizontal",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(color = "black")
  ) +
  scale_fill_continuous(low = "lightblue", high = "darkblue") +
  labs(fill = "Score 
(0=Low, 1=High)", caption = "Source: UNDP, IEP Calculations")

# To display the plot in your R environment
print(MAP_subnat_hdi_ukr)

###########################################################################################################################
##########################################################################################################################
####----components of hdi -----------------------------------------------------------------------------------------------
# Get Shapefile
ukr.map <- iepg_get_gadm("level1") %>%
  dplyr:: filter(str_detect (geocode, "UKR")) # 

tmp<- iepg_get(19870)
tmp <- tmp %>%
  dplyr::filter(grepl("UKR", geocode))
tmp<- tmp%>% filter (year == 2021)

ukr.map <- ukr.map %>%
  left_join(tmp, by = ("geoname"))

ukr_plot <- ggplot(data = ukr.map) +
  geom_sf(aes(fill = value), color = "white") +  # Specify fill based on the 'value' column
  theme_void() +
  theme(
    panel.background = element_rect(fill = 'white', colour = 'black'),
    legend.position = c(0.2, 0.1),  # Specify legend position here
    legend.direction = "horizontal",
    legend.key.height = unit(0.4, 'cm'),
    legend.key.width = unit(1, 'cm'),
    legend.text = element_text(color = "black")
  ) +
  scale_fill_continuous(low = "lightblue", high = "darkblue") +
  labs(fill = "Score", 
       title = "Income Index",
       caption = "Source: HDI, IEP Calculations")

print(ukr_plot)
