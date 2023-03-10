#_______________________Data Preparation for ML_________

library(tidyverse)
library(janitor)
library(plotly)

tbl <-  rio::import(here::here("data/Crop_Yield_data.csv")) |>
         janitor::clean_names() |>
         mutate(crop_type = case_when(
           crop_type %in% c("Banana for beer","Bananas","Cooking Banana","Dessert banana")~ "banana",
           crop_type %in% c("Vegetables","Legumes and Pulses","Vegetables and Beans")~ "vegetables",
           crop_type %in% c("Bush bean","Climbing bean","Small red bean","Beans")~ "beans",
           crop_type %in% c("Yams & Taro")~ "yams",
           crop_type %in% c("Irish potatoes")~ "irish_potatoes",
           crop_type %in% c("Sweet potatoes")~ "sweet_potatoes",
           crop_type %in% c("Soya beans")~ "soya",
           TRUE ~ crop_type
         )) |>
  filter(crop_type %in% c("banana","sweet_potatoes", "Cassava", "irish_potatoes","vegetables",
                         "beans", "Maize", "yams" ,"Sorghum","soya")) |>
  group_by(across(where(is.character) | where(is.integer))) |>
  summarise(production = if_else(is.na(production), mean(production,na.rm=TRUE),production),
        arable_size = if_else(is.na(arable_size), mean(arable_size,na.rm=TRUE),arable_size)
         ) |>
  mutate(across(where(is.numeric), ~replace_na(., mean(., na.rm=TRUE)))) |> drop_na()

tbl2 <- rio::import(here::here("data/meteo_data.csv")) |> clean_names()

tbl_ML <- list(tbl, tbl2) |>
          reduce(inner_join, by = c ("year","seasons", "districts")) |>
          relocate(year,.before = "seasons") |>
          mutate(across(where(is.numeric), ~ round(., digits = 2)))

write.csv(tbl_ML, here::here("data/tbl_ML.csv"),row.names = FALSE)

#______________________________________________________________________

#Check the distribution of numeric data

#Overall Distribution For All Continuous Features

df <- rio::import("data/tbl_ML.csv")
HistCont<-df %>%select(-year) %>%
  keep(is.numeric) %>%
  gather() %>%
  ggplot() +
  geom_histogram(mapping = aes(x=value,fill=key), color="black") +theme_bw()+
  facet_wrap(~ key, scales = "free") +
  theme_minimal() + labs(x="Corresponding Value",y= "Total Numbers",
                         caption =  "Source: mgisa ")+
  ggtitle("Distribution of Continuous Variables")+
  theme(legend.position = 'none',
        #axis.text.x = element_text(angle = 45, face = "bold", colour = "black",size = 15),
        axis.title.x= element_text(size = 18,face = "bold",color="red"),
        axis.title.y = element_text(size=18, angle = 90,vjust = 0.3,face = "bold",color = "red"),
        plot.title = element_text(size=16, face="bold", color="forest green"))
print(HistCont)
plotly_build(HistCont)
#___________________

#Exploratory data variables
table1::table1(~area_worst + radius_worst + perimeter_worst + concave.points_worst+
                 + concave.points_mean + area_se + texture_worst +concavity_worst +
                 + texture_mean, data = bcancer)
