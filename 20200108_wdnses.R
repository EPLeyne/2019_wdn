library(tidyverse)
library(readxl)
library(janitor)
library(viridis)

wdnses <- read_excel('TimeStation_Report_20191221_1051.xlsx', sheet = 'TimeStation_Report_20191221_105') %>% 
  clean_names() %>% 
  select(-(employee_id)) %>% 
  spread(department, total_hours) %>% 
  gather('Meeting - W', 'Community Engagement - W', 'Operations - W', 'Other - W', 'Training - W', key = 'department', value = 'total_hours')
  arrange(department)
wdnses$team <- factor(wdnses$team)

empty_bar <- 2
nObsType <- nlevels(as.factor(wdnses$department))
to_add <- data.frame( matrix(NA, empty_bar*nlevels(wdnses$team)*nObsType, ncol(wdnses)) )
colnames(to_add) <- colnames(wdnses)
to_add$team <- rep(levels(wdnses$team), each = empty_bar)
wdnses <- rbind(wdnses, to_add)
wdnses <- wdnses %>% arrange(team)
wdnses$id <- rep(seq(1, nrow(wdnses)/nObsType), each = nObsType)

label_data <- wdnses %>% group_by(id, employee_name) %>% summarize(tot=sum(total_hours))
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse( angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- wdnses %>% 
  group_by(team) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

grid_data <- base_data
grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]

p <- ggplot(wdnses) +      
  
  # Add the stacked bar
  geom_bar(aes(x=as.factor(id), y=total_hours, fill=department), stat="identity", alpha=0.5) +
  scale_fill_viridis(discrete=TRUE) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 0, xend = start, yend = 0), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 50, xend = start, yend = 50), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 100, xend = start, yend = 100), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 150, xend = start, yend = 150), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  ggplot2::annotate("text", x = rep(max(wdnses$id),5), y = c(0, 50, 100, 150, 200), label = c("0", "50", "100", "150", "200") , color="grey", size=6 , angle=0, fontface="bold", hjust=1) +
  
  ylim(-150,max(label_data$tot, na.rm=T)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() +
  
  # Add labels on top of each bar
  geom_text(data=label_data, aes(x=id, y=tot+10, label=employee_name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=team), hjust=c(1,1,0,0,1), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p
