library(ggplot2)

# Figure 1.1
earthquakes$Date <- as.Date(paste0(earthquakes$year, "-01-01"))
ggplot(earthquakes, aes(x = Date, y = count)) + 
  geom_point() + 
  geom_line() + 
  scale_x_date("") + 
  scale_y_continuous(limits = c(0, 50)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Figure 1.2
ggplot(earthquakes) + 
  geom_histogram(aes(x = count, y = ..density..), 
                 binwidth = 1, fill = "grey", origin = -0.5) + 
  geom_point(data = data.frame(x = 1:45, 
                               fit = dpois(1:45, mean(earthquakes$count))), 
             aes(x = x, y = fit)) + 
  scale_x_continuous("", limits = c(0, 45)) + 
  scale_y_continuous("", limits = c(0, 0.10)) + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())

# Figure 1.3

# Figure 1.4

# Figure 1.5