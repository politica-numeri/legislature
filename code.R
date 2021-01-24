setwd("C:/Users/Roberto/Desktop/rstudio_default/pol_fuoricontesto/legislature")

library(tidyverse)
library(data.table)
library(openxlsx)
library(lubridate)
library(RColorBrewer)
library(extrafont)
library(tidytext)
font_import()
loadfonts(device = "win")


legislature <- read.xlsx("legislature_20210124.xlsx")

legislature <- legislature %>% 
  mutate(data_inizio_legislatura = convertToDate(data_inizio_legislatura),
         data_fine_legislatura = convertToDate(data_fine_legislatura),
         durata_legislatura = as.numeric(data_fine_legislatura - data_inizio_legislatura),
         durata_legislatura_anni = time_length(data_fine_legislatura - data_inizio_legislatura, "years"))


legislature_dur <- legislature %>% 
  group_by(paese) %>% 
  mutate(perc_legislatura = (durata_legislatura_anni / anni_previsti_legislatura)*100) %>% 
  ungroup()

View(legislature_dur)

legislature_dur_pivot <- legislature_dur %>% 
  group_by(paese) %>% 
  summarise(perc_media_legislatura = mean(perc_legislatura)) %>% 
  ungroup() %>% 
  arrange(desc(perc_media_legislatura))
legislature_dur_pivot

legislature_dur %>% 
  filter(data_inizio_legislatura > "1994-01-01") %>% 
  group_by(paese) %>% 
  summarise(perc_media_legislatura = mean(perc_legislatura)) %>% 
  arrange(desc(perc_media_legislatura))

legislature_dur_1994 <- legislature %>% 
  mutate(post1994 = case_when(data_inizio_legislatura > "1994-01-01" ~ "dal 1994",
                              TRUE ~ "1945-1993")) %>% 
  group_by(paese) %>% 
  mutate(perc_legislatura = (durata_legislatura_anni / anni_previsti_legislatura)*100) %>% 
  ungroup()
  
legislature_dur_pivot94 <- legislature_dur_1994 %>% 
    group_by(paese, post1994) %>% 
    summarise(perc_media_legislatura = mean(perc_legislatura)) %>% 
    ungroup() %>% 
    arrange(desc(perc_media_legislatura))
legislature_dur_pivot94

#grafico

legislature_dur_pivot %>% 
  ggplot(aes(x = reorder(paese, perc_media_legislatura), y = perc_media_legislatura)) +
  geom_col(aes(fill = ifelse(paese == "Italia", "Normal", "Highlighted"))) +
  geom_label(aes(label= round(perc_media_legislatura, digits = 1))) +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  ylim(0, 100) +
  xlab("paese") +
  ylab("percentuale media legislatura") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text=element_text(size=12, family="Trebuchet MS"))

ggsave("legislature_full.svg")

legislature_dur_pivot94 %>% 
  filter(post1994 != "dal 1994") %>% 
  ggplot(aes(x = reorder(paese, perc_media_legislatura), y = perc_media_legislatura)) +
  geom_col(aes(fill = ifelse(paese == "Italia", "Normal", "Highlighted"))) +
  geom_label(aes(label= round(perc_media_legislatura, digits = 1))) +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  ylim(0, 105) +
  xlab("paese") +
  ylab("percentuale media legislatura") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text=element_text(size=12, family="Trebuchet MS")) 

ggsave("legislature_pre94.svg")


legislature_dur_pivot94 %>% 
  filter(post1994 == "dal 1994") %>% 
  ggplot(aes(x = reorder(paese, perc_media_legislatura), y = perc_media_legislatura)) +
  geom_col(aes(fill = ifelse(paese == "Italia", "Normal", "Highlighted"))) +
  geom_label(aes(label= round(perc_media_legislatura, digits = 1))) +
  coord_flip() +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  ylim(0, 105) +
  xlab("paese") +
  ylab("percentuale media legislatura") +
  theme_minimal() +
  guides(fill = "none") +
  theme(text=element_text(size=12, family="Trebuchet MS")) 

ggsave("legislature_post94.svg")
