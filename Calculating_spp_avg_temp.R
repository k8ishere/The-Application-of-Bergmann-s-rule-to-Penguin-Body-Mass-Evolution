if (!require(dplyr)) install.packages('dplyr')
if (!require(tidyr)) install.packages('tidyr')

# Create a function to calculate average temperature over a range of ages
calc_avg_temp <- function(min_age, max_age, temp_data) {
  # Filter temperature data for the relevant age range
  relevant_temps <- temp_data %>%
    filter(age >= min_age & age <= max_age)
  
  # Calculate and return the average temperature
  avg_temp <- mean(relevant_temps$temperature, na.rm = TRUE)
  return(avg_temp)
}

# Calculate the average temperature for each species
spp_avg_temp <- spp_age %>%
  rowwise() %>%
  mutate(avg_temp = calc_avg_temp(mya_min, mya_max, temp)) %>%
  ungroup()

# Write the results to a new CSV file
write.csv(spp_avg_temp, 'spp_avg_temp.csv', row.names = FALSE)

# Print the result
print(spp_avg_temp)

# Install and load writexl
install.packages("writexl")
library(writexl)
write_xlsx(spp_avg_temp, "spp_avg_temp.xlsx")

