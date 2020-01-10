library(tidyverse)
library(readxl)
library(janitor)

wdnses <- read_excel('TimeStation_Report_20191221_1051.xlsx', sheet = 'TimeStation_Report_20191221_105') %>% 
  clean_names() %>% 
  select(-(employee_id)) %>% 
  spread(department, total_hours) %>% 
  gather('Meeting - W', 'Community Engagement - W', 'Operations - W', 'Other - W', 'Training - W', key = 'department', value = 'total_hours') 

ggplot(wdnses, aes(fill = department, x = employee_name, y=total_hours)) + 
  geom_bar(position = "stack", stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90))

wdnses_filt <- wdnses %>% 
  filter(team != 'Support')

ggplot(wdnses_filt, aes(fill = department, x = employee_name, y=total_hours)) + 
  geom_bar(position = "stack", stat = 'identity') +
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(~ team, nrow = 3, scales = 'free_x')
