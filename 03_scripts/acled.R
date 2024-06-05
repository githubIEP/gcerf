
acled<- iepg_acled()
filtered <- filter(acled, geocode == "UKR")

# Group by year and disorder type, then count the number of incidents
incident_count <- filtered %>%
  group_by(year, event_type) %>%
  summarise(incident_count = n())

# Print the result
print(incident_count)
