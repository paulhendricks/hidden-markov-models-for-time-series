# Load libraries
library(ggplot2)

# Load data
earthquakes <- read.csv("../data/earthquakes.csv", 
                        stringsAsFactors = FALSE)

# Figure 1.1
ggplot(earthquakes, aes(x = year, y = count)) + 
  geom_point() + 
  geom_line() + 
  scale_x_continuous("", breaks = seq(1900, 2010, 20)) + 
  scale_y_continuous(limits = c(0, 50)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Figure 1.2
ggplot(earthquakes) + 
  geom_histogram(aes(x = count, y = ..density..), 
                 binwidth = 1, fill = "grey", origin = -0.5) + 
  geom_point(data = data.frame(x = 0:45, 
                               fit = dpois(0:45, mean(earthquakes$count))), 
             aes(x = x, y = fit)) + 
  scale_x_continuous("", limits = c(0, 45)) + 
  scale_y_continuous("", limits = c(0, 0.10)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Figure 1.3

# Figure 1.4

# Figure 1.5