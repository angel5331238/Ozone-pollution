#Package Installation:
if(!require(pacman)) install.packages('pacman')
pacman::p_load(tidyverse, data.table,BBmisc,scales)

#Read in data:
Ozone <- fread('hourly_44201_2017.csv') %>% 
  select("State Code", "County Code", "Date Local","Time Local", "Sample Measurement") %>%
  rename(state = "State Code", county = "County Code",date = "Date Local",hour="Time Local",value="Sample Measurement") %>%
  filter(state=="6")%>%  #CA
  select(-state)%>%
  mutate(value=value*1000)  #ppm to ppb (same as VOC)

VOC <- fread('hourly_VOCS_2017.csv') %>% 
  select("State Code", "County Code", "Date Local","Time Local", "Sample Measurement") %>%
  rename(state = "State Code", county = "County Code",date = "Date Local",hour="Time Local",value="Sample Measurement") %>%
  filter(state=="6")%>%  #CA
  select(-state)

#Standardize data (each county has different scale):
#O3
normO3 <- Ozone%>%
  group_by(county)%>%
  mutate(norm= rescale(value,to=c(0, 1)))%>%
  ungroup(county)

#VOC
normVOC <- VOC%>%
  group_by(county)%>%
  mutate(norm= rescale(value,to=c(0, 1)))%>%
  ungroup(county)

#O3
ggplot(normO3, aes(x = hour, y = norm)) + 
  geom_boxplot(outlier.alpha = 0.1,outlier.shape = 1)+
  ylim(0,1)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("Ozone concentration in a day") +
  ylab("standardized ozone concentration")

#VOC
ggplot(normVOC, aes(x = hour, y = norm)) + 
  geom_boxplot(outlier.alpha = 0.1,outlier.shape = 1)+
  ylim(0,1)+
  theme_bw()+
  theme_light()+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("VOC concentration in a day") +
  ylab("standardized VOC concentration")
