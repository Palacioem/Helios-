library(dplyr) #install.packages("dplyr")
library(ggplot2) #install.packages("ggplot2")


#thios is code if excel sheets are .xlx 
solar_2004 <- Solar_flare_RHESSI_2004_05
solar_2015 <- Solar_flare_RHESSI_2015_16


#import both excel sheets located in teams
solar_2004 <- read.csv("Solar_flare_RHESSI_2004_05.csv")
solar_2015 <- read.csv("Solar_flare_RHESSI_2015_16.csv")

# subdivides set into smaller batches of 4 months and 2 month overlap between consecutive batches 
subgroup_data <- function(data, start_year){
  subgroups <- list()
  
  months_list <- list(c(1,2,3,4), c(3,4,5,6), c(5,6,7,8), c(7,8,9,10), 
                      c(9,10,11,12), c(11,12,1,2), c(1,2,3,4), c(3,4,5,6), 
                      c(5,6,7,8), c(7,8,9,10), c(9,10,11,12))
  
  years_list <- list(c(start_year), c(start_year), c(start_year), c(start_year), c(start_year), 
                     c(start_year, start_year+1), c(start_year+1), c(start_year+1), 
                     c(start_year+1), c(start_year+1), c(start_year+1))
  
  for(i in 1:length(months_list)){
    subgroups[[i]] <- data[data$month %in% months_list[[i]] & data$year %in% years_list[[i]], ]
  }
  
  return(subgroups)
}

# Replace "45089" and "45285" with '800-700' in your_column
# this step is only necessary if data was corrupted on import 
#45089 changes to 6-12
#45285 12-25


solar_2004 <- solar_2004 %>%
mutate(energy.kev = ifelse(energy.kev == 45089, '6-12',
                    ifelse(energy.kev == 45285, '12-25', energy.kev)))
solar_2015 <- solar_2015 %>%
  mutate(energy.kev = ifelse(energy.kev == 45089, '6-12',
                             ifelse(energy.kev == 45285, '12-25', energy.kev)))

unique(solar_2004["energy.kev"])

unique(solar_2015["energy.kev"])

#create list of 11 subgroups
solar_2004_subgroups <- subgroup_data(solar_2004, 2004)
solar_2015_subgroups <- subgroup_data(solar_2015, 2015)


remove_outliers <- function(data, column_name) {
  # Extract the specified data column
  data_column <- data[[column_name]]
  
  # Calculate the interquartile range (IQR)
  q1 <- quantile(data_column, 0.25)
  q3 <- quantile(data_column, 0.75)
  iqr <- q3 - q1
  
  # Define the lower and upper bounds to identify outliers
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  
  # Remove outliers from the specified column in the data frame
  data_filtered <- data[data_column >= lower_bound & data_column <= upper_bound, ]
  
  return(data_filtered)
}

#useful for part 3 or determining thresholds 
create_density_plot <- function(data_column) {
  ggplot(data.frame(column = data_column), aes(x = column)) +
    geom_density(fill = "skyblue", color = "white") +
    labs(
      title = paste("Density Plot of", deparse(substitute(data_column))),
      x = deparse(substitute(data_column)),
      y = "Density"
    )
}

#used to categorize total.counts
categorize_data <- function(data_column) {
  low_threshold <- quantile(data_column, 0.66)  # 1st quantile
  high_threshold <- quantile(data_column, 0.99)  # 80th quantile
  
  categories <- cut(data_column, breaks = c(-Inf, low_threshold, high_threshold, Inf),
                    labels = c("Low", "Medium", "High"), include.lowest = TRUE, right = FALSE)
  return(categories)
}

# Method 1 with intensity based on total count category
method_1 <- function(data, year,k) {
  # Categorize total counts into "Low," "Medium," and "High"
  data$intensity_category <- categorize_data(data$total.counts)
  
  ggplot(data, aes(x = x.pos.asec, y = y.pos.asec, color = intensity_category, size = total.counts)) +
    geom_point(alpha = 0.9) +
    scale_color_manual(values = c("Low" = "blue", "Medium" = "lightblue", "High" = "red")) +
    scale_size_continuous(range = c(1, 6), breaks = seq(min(data$total.counts), max(data$total.counts), length.out = 10)) +
    scale_fill_gradient(low = "blue", high = "red") +
    scale_x_continuous(limits = c(-1000, 1000)) +
    scale_y_continuous(limits = c(-1000, 1000)) +
    labs(
      title = paste("Flare Intensity based on Total Counts -", year,"-",year+1, " batch: ",k),
      x = "X position [asec]",
      y = "Y position [asec]",
      color = "Intensity Category"
    ) +
    theme_minimal() +
    theme(legend.key.width = unit(1.5, "cm"))
}


 #whole year 
method_1(solar_2004_subgroups[[1]],2004,1) # 2004 month 1, 2, 3, 4
method_1(solar_2004_subgroups[[11]],2004,11)




calculate_intensity <- function(energy_data, duration, weight_factor) {
  seq_indices <- seq_along(energy_data)
  
  # Adjust upper limit based on the minimum of duration and length of energy_data
  upper_limit <- min(duration, length(energy_data))
  
  intensity <- integrate(Vectorize(function(t) weight_factor * energy_data[t]), lower = 1, upper = upper_limit)$value
  return(intensity)
}

method_2 <- function(data, year,k) {
  # Specify the order of levels for energy.kev
  energy_levels <- c("6-12", "12-25", "25-50", "50-100", "100-300", "300-800")
  
  # Assuming you have a column named energy_data in your data
  data <- data %>%
    group_by(energy.kev) %>%
    mutate(intensity = calculate_intensity(energy.kev.f, duration.s,3))
  
  p <-ggplot(data, aes(x = x.pos.asec, y = y.pos.asec, color = factor(energy.kev, levels = energy_levels), size = intensity)) +
    geom_point(alpha = 0.9) +
    scale_color_manual(values = c(
      "6-12" = 'yellow',
      "12-25" =  'gold',
      "25-50" = 'orange',
      "50-100" = "darkorange",
      "100-300" = "red",
      "300-800" = "darkred"
    )) +
    scale_size_continuous(range = c(1, 5), breaks = seq(min(data$intensity), max(data$intensity), length.out = 10)) +
    scale_x_continuous(limits = c(-1300, 1300)) +
    scale_y_continuous(limits = c(-1300, 1300)) +
    labs(
      title = paste("Flare Intensity based on Flux Integration", year,"-",year+1, " batch: ",k),
      x = "X position [asec]",
      y = "Y position [asec]",
      color = "Energy.kev"
    ) +
    theme_dark() +
    theme(legend.key.width = unit(1.5, "cm"))
    return(p)
}





# Replace "overlay_image.jpg" with your actual image file name
library(ggplot2)
library(grid)
library(jpeg)

image_file <- file.path("C:", "Users", "palac", "Desktop", "Gamer", "UH", "Fall 2023", "COSC 3337", "HELIOS", "task2.temp.jpg")

method_2 <- function(data, year, k, image_file) {
  energy_levels <- c("6-12", "12-25", "25-50", "50-100", "100-300", "300-800")
  
  data <- data %>%
    group_by(energy.kev) %>%
    mutate(intensity = calculate_intensity(energy.kev.f, duration.s, 3))
  
  # Read the image using readJPEG (install the jpeg package if not installed)
  img <- readJPEG(image_file)  # Use readJPEG for JPG
  
  p <- ggplot(data, aes(x = x.pos.asec, y = y.pos.asec, color = factor(energy.kev, levels = energy_levels), size = intensity)) +
    annotation_custom(rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")), 
                      xmin = -1300, xmax = 1300, ymin = -1300, ymax = 1300) +
    geom_point(alpha = 0.9) +  # This line was moved after annotation_custom
    scale_color_manual(values = c(
      "6-12" = 'yellow',
      "12-25" = 'gold',
      "25-50" = 'orange',
      "50-100" = "darkorange",
      "100-300" = "red",
      "300-800" = "darkred"
    )) +
    scale_size_continuous(range = c(1, 5), breaks = seq(min(data$intensity), max(data$intensity), length.out = 10)) +
    scale_x_continuous(limits = c(-1300, 1300)) +
    scale_y_continuous(limits = c(-1300, 1300)) +
    labs(
      title = paste("Flare Intensity based on Flux Integration", year, "-", year + 1, " batch: ", k),
      x = "X position [asec]",
      y = "Y position [asec]",
      color = "Energy.kev"
    ) +
    theme_dark() +
    theme(legend.key.width = unit(1.5, "cm"))
  
  return(p)
}


method_2(solar_2015,2004,1 ,image_file)
method_2(solar_2004_subgroups[[1]],2004,1) # 2004 month 1, 2, 3, 4
method_2(solar_2004_subgroups[[11]],2015,3,image_file)
method_2(solar_2004_subgroups[[11]],2015,4,image_file)# 2005 month 9, 10, 11, 12




# Task 2 ------------------------------------------------------
library(animation)

visualize_hotspots <- function(data, d, subgroup,img){
  img <- readJPEG(image_file)
  
  
   p <- ggplot(data, aes(x = x.pos.asec, y = y.pos.asec)) +
    ylim(-1500,1500) + xlim(-1500,1500) +
    annotation_custom(rasterGrob(img, width = unit(1, "npc"), height = unit(1, "npc")), 
                       xmin = -1300, xmax = 1300, ymin = -1300, ymax = 1300)+
    guides(alpha=F) +
    stat_density_2d(aes(fill = ..level..,alpha = ..level..),geom = "polygon",bins = 30) +
    scale_fill_continuous(low = "yellow", high = "red") +
    labs(title = paste("Subgroup", i, "Hotspots Above Threshold", d),
         x = "X position [asec]",
         y = "Y position [asec]",
         color = "Intensity") +
    theme(plot.title = element_text(hjust = 0.5))+
    theme(legend.key.width = unit(1.5, "cm"))
  return(p)
}


# Data below each threshold will be removed before visualizing hotspots
# d1 is 95th percentile and d2 is 75th percentile
percentiles <- quantile(solar_2004$total.counts, probs = c(.75, .95))
d1 <- percentiles["95%"][[1]]
d2 <- percentiles["75%"][[1]]

d1_time_series <- list()
d2_time_series <- list()

# Need to implement d1 and d2
for (i in 1:11){
  data_d1 <- solar_2004_subgroups[[i]][solar_2004_subgroups[[i]]$total.counts >= d1, ]
  data_d2 <- solar_2004_subgroups[[i]][solar_2004_subgroups[[i]]$total.counts >= d2, ]
  d1_time_series[[i]] = visualize_hotspots(data_d1, "d1", i,image_file)
  d2_time_series[[i]] = visualize_hotspots(data_d2, "d2", i,image_file)
}

# Save time series gifs
saveGIF({
  for (i in 1:11) {
    print(d1_time_series[[i]])
  }
}, movie.name = "d1_time_series.gif", interval = 0.5)

saveGIF({
  for (i in 1:11) {
    print(d2_time_series[[i]])
  }
}, movie.name = "d2_time_series.gif", interval = 0.5)



# Task 3 ------------------------------------------------------


#BASIC STATISTICS

k.max <- 11
wss <- numeric(k.max)
for(i in 1:11){
  wss[i] <- mean(solar_2004_subgroups[[i]]$total.counts)
}
plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "2004 Changes in average total counts by subgroup",
     xlab = "subgroup",
     ylab = "Total.counts")

k.max <- 11
wss <- numeric(k.max)
for(i in 1:11){
  wss[i] <- mean(solar_2015_subgroups[[i]]$total.counts)
}
plot(wss,
     type="b",
     pch=19,
     col=2,
     main = "2015 Changes in average total counts by subgroup",
     xlab = "subgroup",
     ylab = "Total.counts")


# CODE TO COMPARE YEARS USING METHOD 1 
d1_time_series <- list()
d2_time_series <- list()


for (i in 1:11){
  d1_time_series[[i]] = method_1((solar_2004_subgroups[[i]]),2004,i)
  d2_time_series[[i]] = method_1((solar_2015_subgroups[[i]]),2015,i)
}

# Save time series gifs
saveGIF({
  for (i in 1:11) {
    print(d1_time_series[[i]])
  }
}, movie.name = "method1_2004.gif", interval = 0.5)

saveGIF({
  for (i in 1:11) {
    print(d2_time_series[[i]])
  }
}, movie.name = "method1_2015.gif", interval = 0.5)

#entire dataset

method_1(solar_2004,2004,0)

method_1(solar_2015,2015,0)


#METHOD 2

d1_time_series <- list()
d2_time_series <- list()


for (i in 1:11){
  d1_time_series[[i]] = method_2((solar_2004_subgroups[[i]]),2004,i)
  d2_time_series[[i]] = method_2((solar_2015_subgroups[[i]]),2015,i)
}

# Save time series gifs
saveGIF({
  for (i in 1:11) {
    print(d1_time_series[[i]])
  }
}, movie.name = "method2_2004.gif", interval = 0.5)

saveGIF({
  for (i in 1:11) {
    print(d2_time_series[[i]])
  }
}, movie.name = "method2_2015.gif", interval = 0.5)

method_2(solar_2004,2004,0)

method_2(solar_2015,2015,0)

density(solar_2004$total.counts)
density(solar_2015$total.counts)
create_density_plot(remove_outliers(solar_2004,"total.counts")$total.counts)
create_density_plot(remove_outliers(solar_2015,"total.counts")$total.counts)

# energy band distribution 
par(mfrow = c(1, 2))
custom_order <- c("6-12", "12-25", "25-50", "50-100", "100-300", "300-800")

# Define colors for each category
category_colors <- c("6-12" = "blue", "12-25" = "royalblue", "25-50" = "green", 
                     "50-100" = "yellow", "100-300" = "orange", "300-800" = "red")


# Group by the 'energy.kev' column and calculate the mean duration for each group
result <- solar_2004 %>%
  group_by(energy.kev) %>%
  summarize(mean_duration = mean(duration.s, na.rm = TRUE))

# View the result
print(result)
result <- solar_2015 %>%
  group_by(energy.kev) %>%
  summarize(mean_duration = mean(duration.s, na.rm = TRUE))

# View the result
print(result)

# Plot for solar_2004
barplot(table(solar_2004$energy.kev), main = "Solar 2004", 
        xlab = "Energy Category", ylab = "Frequency", 
        col = category_colors[as.character(levels(solar_2004$energy.kev))])

text(x = barplot(table(solar_2004$energy.kev), plot = FALSE), 
     y = table(solar_2004$energy.kev) + 1, 
     labels = table(solar_2004$energy.kev), 
     pos = 3, col = "black")

legend("topright", legend = levels(solar_2004$energy.kev), fill = category_colors, title = "Energy Category")

# Plot for solar_2015

# Plot for solar_2015
barplot(table(solar_2015$energy.kev), main = "Solar 2015", 
        xlab = "Energy Category", ylab = "Frequency", 
        col = category_colors[as.character(levels(solar_2015$energy.kev))])

text(x = barplot(table(solar_2015$energy.kev), plot = FALSE), 
     y = table(solar_2015$energy.kev) + 1, 
     labels = table(solar_2015$energy.kev), 
     pos = 3, col = "black")

# Reset the layout to default (1 plot)
par(mfrow = c(1, 1))



k.max <- 11
wss_2004 <- numeric(k.max)
wss_2015 <- numeric(k.max)

# Calculate mean duration for 2004
for (i in 1:11) {
  wss_2004[i] <- mean(solar_2004_subgroups[[i]]$duration.s)
}

# Calculate mean duration for 2015
for (i in 1:11) {
  wss_2015[i] <- mean(solar_2015_subgroups[[i]]$duration.s)
}

# Plot both years on the same graph with different colors
plot(wss_2015,
     type = "b",
     pch = 19,
     col = "blue",
     main = "Changes in duration by subgroup",
     xlab = "Subgroup",
     ylab = "Duration.s")

points(wss_2004,
       type = "b",
       pch = 19,
       col = "red")

# Add a legend to differentiate between the years
legend("topright", legend = c("2004", "2015"), col = c("red", "blue"), pch = 19, title = "Year")
cor(solar_2004$duration.s,solar_2004$energy.kev.f)




