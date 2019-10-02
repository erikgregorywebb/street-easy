# Data Source: https://streeteasy.com/blog/data-dashboard/

library(tidyverse)
library(scales)

# download files
setwd("~/Files/Projects/street-easy/data")
base_url = 'https://streeteasy-market-data-download.s3.amazonaws.com/rentals/'

urls_rent = c(
  paste(base_url, 'Studio/medianAskingRent_Studio.zip', sep = ''),
  paste(base_url, 'OneBd/medianAskingRent_OneBd.zip', sep = ''),
  paste(base_url, 'TwoBd/medianAskingRent_TwoBd.zip', sep = ''),
  paste(base_url, 'ThreePlusBd/medianAskingRent_ThreePlusBd.zip', sep = '')
)

# define "unzipping" function
unzip_file = function(url) {
  temp = tempfile()
  download.file(url, temp, mode = 'wb')
  unzip(temp, exdir = ".")
  unlink(temp)
}

# loop and apply function
for (i in 1:length(urls_rent)) {
  Sys.sleep(1)
  unzip_file(urls_rent[i])
}

# import files
metrics = c('medianAskingRent')
bdrms = c('Studio', 'OneBd', 'TwoBd', 'ThreePlusBd')
for (i in 1:length(metrics)) {
  for (j in 1:length(bdrms)) {
    file = paste(metrics[i], '_', bdrms[j], '.csv', sep = '')
    temp = read.csv(file, header = T, stringsAsFactors = F)
    assign(paste(metrics[i], '_', bdrms[j], sep = ''), temp)
  }
}

# combine files
se = bind_rows(
  medianAskingRent_Studio %>%
    gather('year-month', 'value', -Borough, -areaName, -areaType) %>% mutate(bdrs = 0),
  medianAskingRent_OneBd %>%
    gather('year-month', 'value', -Borough, -areaName, -areaType) %>% mutate(bdrs = 1),
  medianAskingRent_TwoBd %>%
    gather('year-month', 'value', -Borough, -areaName, -areaType) %>% mutate(bdrs = 2),
  medianAskingRent_ThreePlusBd %>%
    gather('year-month', 'value', -Borough, -areaName, -areaType) %>% mutate(bdrs = 3)
)

# clean environment
rm(medianAskingRent_Studio, medianAskingRent_OneBd, medianAskingRent_TwoBd, medianAskingRent_ThreePlusBd, temp)

# clean dataframe
se = se %>%
  mutate(date = gsub('X', '', `year-month`)) %>%
  select(-`year-month`) %>%
  mutate(date = gsub('\\.', '-', date)) %>%
  mutate(date = as.Date(paste(date, '-01', sep = ''), format = '%Y-%m-%d')) %>%
  mutate(bdrs = factor(bdrs)) %>%
  mutate(Borough = factor(Borough, levels = c('Manhattan', 'Brooklyn', 'Queens', 'Bronx')))

# visualize
se %>%
  filter(areaType == 'borough') %>%
  filter(Borough != 'Staten Island') %>%
  filter(bdrs != '3') %>%
  ggplot(., aes(x = date, y = value, col = bdrs)) +
  geom_line(size = 1) +
  geom_smooth(size = 2, se = F) + 
  scale_y_continuous(labels = dollar) + 
  scale_x_date(date_breaks = '2 years', date_labels = "%Y") + 
  facet_wrap(~Borough, scales = 'fixed', nrow = 1) +
  #theme(legend.position = 'top') +
  theme(legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal') + 
  theme(text = element_text(size = 12, family = 'Helvetica')) + 
  labs(x = '', y = 'Median Asking Price', title = 'NYC Median Asking Rent Price by Borough', 
       subtitle = 'January 2010 - August 2019', caption = 'Data by @streeteasy, Visual by @erikgregorywebb', color = 'Number of Bedrooms') 

# export
setwd("~/Files/Projects/street-easy/output")
file_name = paste('street-easy-median-asking-rent-', Sys.Date(), '.csv', sep = '')
write.csv(se, file_name, row.names = F, na = '')
write_csv(se, file_name, na = '')
