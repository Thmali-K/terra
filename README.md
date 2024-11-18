# terra
# Load necessary libraries
library(terra)
library(ggplot2)
library(ncdf4)

# Function to load and process the NetCDF data
process_precipitation_data <- function(file_path, start_year = 1950, end_year = 2023, exclude_years = c(1950, 2023)) {
  # Open NetCDF file
  f <- nc_open(file_path)
  
  # Extract latitude, longitude, and time information
  lon <- ncvar_get(f, "longitude")
  lat <- ncvar_get(f, "latitude")
  t <- ncvar_get(f, "time")
  time_from <- substr(ncatt_get(f, "time")$units, 13, 33)
  time <- as.POSIXct(t, origin = time_from, tz = "UTC")
  
  # Load data as SpatRaster object
  rSL <- terra::rast(file_path)
  
  # Calculate the mean precipitation for each layer across the entire region
  mean_precipitation <- as.vector(global(rSL, fun = "mean", na.rm = TRUE)[, 1])
  
  # Prepare the data for plotting
  years <- seq(start_year, end_year, by = 1)
  mean_precipitation_df <- data.frame(Year = years, Precipitation = mean_precipitation)
  
  # Exclude specific years if needed
  mean_precipitation_df <- mean_precipitation_df[!mean_precipitation_df$Year %in% exclude_years, ]
  
  # Calculate the 10th percentile
  percentile_10 <- quantile(mean_precipitation_df$Precipitation, 0.10)
  
  # Create a line plot with ggplot2
  plot <- ggplot(mean_precipitation_df, aes(x = Year, y = Precipitation)) +
    geom_line(color = "black") +
    geom_point(color = "green") +
    geom_text(aes(label = round(Precipitation, 2)), vjust = -0.5, size = 3) +
    labs(title = "Yearly Precipitation",
         x = "Year", y = "Precipitation (mm)") +
    theme_minimal() +
    geom_hline(yintercept = percentile_10, linetype = "dashed", color = "blue") +
    annotate("text", x = end_year, y = percentile_10 + 50, label = paste("10th Percentile:", round(percentile_10, 2), "mm"), color = "blue") +
    geom_point(data = mean_precipitation_df[mean_precipitation_df$Precipitation <= percentile_10, ],
               aes(x = Year, y = Precipitation), color = "red", size = 2)
  
  # Print driest years
  driest_years <- mean_precipitation_df[mean_precipitation_df$Precipitation <= percentile_10, ]
  print(driest_years)
  
  # Return plot and data for further use
  return(list(plot = plot, driest_years = driest_years))
}

# Example of running the function with a sample NetCDF file
file_path <- "path_to_your_netCDF_file.nc"  # Update this path
result <- process_precipitation_data(file_path)
print(result$plot)
