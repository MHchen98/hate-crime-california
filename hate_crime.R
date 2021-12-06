setwd("D:/Fileslala/having fun")
library(openxlsx)

# data part

baseline <- read.xlsx("County list.xlsx", colNames = FALSE)
colnames(baseline) = "county"
temp <- read.xlsx("County Hate crime.xlsx")
colnames(temp)[1] = "county"

# select the rows where variable county contains the string "County"
county_hc <- temp[grep("County", temp$county), ][c(1:58), ]

'''
I delete the last two rows because they are under a row named
"Multiple bias total", and I do not know what that means
'''
rownames(county_hc) <- 1:nrow(county_hc)  # reset row index

# first merging
hc <- merge(baseline, county_hc, by="county")


library(maps)
library(ggplot2)
library(dplyr)
library(magrittr)
library(urbnmapr)
library(urbnthemes)
library(tidyverse)
library(RColorBrewer)

### example using data from urbanmap_r
countydata %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="California") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = horate)) +
  geom_polygon(color = "#ffffff", size = .25) + #county borderline
  scale_fill_gradientn(labels = scales::percent, # transform data to percentage
                       guide = guide_colourbar(title.position = "top"), 
                       colours = colorRampPalette(brewer.pal(5, 'GnBu'))(24)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Homeownership rate") 


# preparing data
hc_cases <- hc[, c(1:2)]
colnames(hc_cases) = c("county_name", "hate_crime")



# mapping for hate crime cases

hc_cases %>% 
  left_join(counties, by = "county_name") %>% 
  filter(state_name =="California") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = hate_crime)) +  # fill = variable name of filling data
  geom_polygon(color = "#000000", size = .25) + #county borderline
  ggtitle("Reported Hate Crime in California, by county") +
  scale_fill_gradientn(guide = guide_colourbar(title.position = "top"), 
                       colours = colorRampPalette(brewer.pal(9, 'GnBu'))(24)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "Hate crime cases")

# mapping for county debt

# mapping for housing price
library(ggplot2)

housing <- read.csv("housing.csv")
housing$median_house_value <- log(housing$median_house_value)

ggplot(data = housing,
       aes(x=longitude, y=latitude, color=median_house_value)) +  # fill = variable name of filling data
geom_point() + #county borderline
ggtitle("Housing Price") +
scale_color_gradient(low="white",high="Blue") +
coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
theme(plot.title = element_text(hjust=0.5),  # Centering the title
      legend.title = element_text(hjust=0.5),
      legend.key.width = unit(.5, "in"))




# mapping for API
api_df <- aggregate(x=dsin[c('API13')], by=list(dsin$CNAME), 
                    FUN=mean, na.rm=TRUE)[c(2:59), ] # na.rm means whether delete the NA/missing values
rownames(api_df) <- 1:nrow(api_df)  # reset row index
colnames(api_df) = c("county_name", "API13")
api_df$county_name <- paste(api_df$county_name, "County")

api_df %>% 
  left_join(counties, by = "county_name") %>% 
  filter(state_name =="California") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = API13)) +  # fill = variable name of filling data
  geom_polygon(color = "#000000", size = .25) + #county borderline
  ggtitle("Academic Performance 2013, by county") +
  scale_fill_gradientn(guide = guide_colourbar(title.position = "top"), 
                       colours = colorRampPalette(brewer.pal(9, 'GnBu'))(24)) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")) +
  labs(fill = "API score")


