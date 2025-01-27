library(dplyr) #install.packages("dplyr")
library(ggplot2) #install.packages("ggplot2")

#import both excel sheets located in teams
solar_2004 <- Solar_flare_RHESSI_2004_05
solar_2015 <- Solar_flare_RHESSI_2015_16


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

solar_2004_subgroups <- subgroup_data(solar_2004, 2004)
solar_2015_subgroups <- subgroup_data(solar_2015, 2015)

# method 1 measures intensity based on count
method_1 <- function(data) {
  ggplot(data, aes(x = x.pos.asec, y = y.pos.asec, color = total.counts, size = total.counts)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = "blue", high = "red") +
    scale_size_continuous(range = c(1, 5)) +
    labs(title = "Flare Intensity based on Total Counts",
         x = "X position [asec]",
         y = "Y position [asec]",
         color = "Intensity") +
    theme_minimal() +
    theme(legend.key.width = unit(1.5, "cm")) 
}

method_1(solar_2004)
method_1(solar_2004_subgroups[[1]]) # 2004 month 1, 2, 3, 4
method_1(solar_2004_subgroups[[11]])# 2005 month 9, 10, 11, 12
method_1(solar_2015_subgroups[[1]]) # 2015 month 1, 2, 3, 4
method_1(solar_2015_subgroups[[11]])# 2016 month 9, 10, 11, 12

# method 2 calculates flare intensity by multiplying its duration with its starting energy value.
method_2 <- function(data) {
  
  data <- data %>%
    mutate(intensity = duration.s * as.numeric(gsub("[^0-9|\\-]", "", energy.kev)))
  
  ggplot(data, aes(x = x.pos.asec, y = y.pos.asec, color = intensity, size = intensity)) +
    geom_point(alpha = 0.7) +
    scale_color_gradient(low = "blue", high = "red") +
    scale_size_continuous(range = c(1, 5)) +
    labs(title = "Flare Intensity based on Duration and Energy",
         x = "X position [asec]",
         y = "Y position [asec]",
         color = "Intensity") +
    theme_minimal() +
    theme(legend.key.width = unit(1.5, "cm"))
}

method_2(solar_2004)
method_2(solar_2004_subgroups[[1]]) # 2004 month 1, 2, 3, 4
method_2(solar_2004_subgroups[[11]])# 2005 month 9, 10, 11, 12
method_2(solar_2015_subgroups[[1]]) # 2015 month 1, 2, 3, 4
method_2(solar_2015_subgroups[[11]])# 2016 month 9, 10, 11, 12