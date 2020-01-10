library(tidyverse)
library(readxl)
library(janitor)
library(viridis)

wdnses <- read_excel('TimeStation_Report_20191221_1051.xlsx', sheet = 'TimeStation_Report_20191221_105') %>% 
  clean_names() %>% 
  select(-(employee_id)) %>% 
  arrange(department)
wdnses$team <- factor(wdnses$team)






data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value1=sample( seq(10,100), 60, replace=T),
  value2=sample( seq(10,100), 60, replace=T),
  value3=sample( seq(10,100), 60, replace=T)
)

data_long <- data %>% gather(key = "observation", value="value", -c(1,2)) 





















#OPS Only
ops <- wdnses %>% filter(department == 'Operations - W') %>% 
  arrange(total_hours)
ops$team <- factor(ops$team)
  

empty_bar <- 3
to_add <- data.frame( matrix(NA, empty_bar*nlevels(ops$team), ncol(ops)))
colnames(to_add) <- colnames(ops)
to_add$team <- rep(levels(ops$team), each = empty_bar)
ops <- rbind(ops, to_add)
ops <- ops %>% arrange(team)
ops$id <- seq(1, nrow(ops))

label_data <- ops
number_of_bar <- nrow(label_data)
angle <- 90 - 360 * (label_data$id-0.5) / number_of_bar
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

base_data <- ops %>% 
  group_by(team) %>% 
  summarise(start = min(id), end = max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title = mean(c(start, end)))

grid_data <- base_data
grid_data$end <- grid_data$end[ c(nrow(grid_data), 1:nrow(grid_data)-1)] +1
grid_data$start <- grid_data$start - 1
grid_data <- grid_data[-1,]


p <- ggplot(ops, aes(x=as.factor(id), y=total_hours, fill=team)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  geom_bar(aes(x=as.factor(id), y=total_hours, fill=team), stat="identity", alpha=0.5) +
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(ops$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=total_hours, fill=team), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=total_hours, label=employee_name, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=team), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p <- ggplot(ops, aes(x=as.factor(id), y=total_hours, fill = team)) +
  geom_bar(stat='identity', alpha = 0.5) +
  ylim(-100, 120) +
  theme_minimal() +
  theme(
    legend.position = 'none',
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm")
  ) +
  coord_polar() +
  geom_text(data = label_data, aes(x = id, y = total_hours, label = employee_name, hjust = hjust), colour = "black", fontface = 'bold', alpha = 0.6, size = 2.5, angle = label_data$angle, inherit.aes = FALSE)





data <- data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

empty_bardt <- 4
to_adddt <- data.frame( matrix(NA, empty_bardt*nlevels(data$group), ncol(data)) )
colnames(to_adddt) <- colnames(data)
to_adddt$group <- rep(levels(data$group), each=empty_bardt)
data <- rbind(data, to_adddt)
data <- data %>% arrange(group)
data$id <- seq(1, nrow(data))