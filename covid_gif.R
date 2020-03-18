library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(magick)
library(gifski)

###Dados extraídos de Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE https://systems.jhu.edu/research/publ…

covid_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
head(covid_confirmed)
colnames(covid_confirmed)

dim(covid_confirmed)


paises <- c("Brazil", "Italy")


covid_confirmed_subset<-covid_confirmed %>% filter(`Country/Region` %in% paises)
head(covid_confirmed_subset)

covid_confirmed_subset_df<-as.data.frame(t(covid_confirmed_subset[,5:60]))
head(covid_confirmed_subset_df)

colnames(covid_confirmed_subset_df)<-c("Italia", "Brasil")
tempo<-c(1:56)
covid_confirmed_subset_df$tempo<-tempo
covid_df_long<-covid_confirmed_subset_df %>% gather(pais, casos, Italia:Brasil)



covid_df_long_brasil<-covid_df_long %>% filter(pais=="Brasil")
covid_df_long_italia<-covid_df_long %>% filter(pais=="Italia")


p_brasil<-ggplot(covid_df_long_brasil, aes(tempo, casos, group = pais)) + 
  geom_line(size = 2, color='green') +transition_reveal(tempo)+
  geom_point(size = 5, color='green')+
  view_follow(fixed_y = c(0.0001,NA))+
  labs(title = 'Brasil - 17/03/2020', y = 'Casos Covid-19')+
  theme_minimal(base_size = 22) + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
gif_brasil<-animate(p_brasil, fps=4, width = 400, height = 400,  renderer = gifski_renderer(loop = F))
anim_save("brasil.gif", gif_brasil)



p_italia<-ggplot(covid_df_long_italia, aes(tempo, casos, group = pais)) + 
  geom_line(size = 2, color='red') +transition_reveal(tempo)+
  geom_point(size = 5, color='red')+
  view_follow(fixed_y = c(0.0001,NA))+
  labs(title = 'Itália - 17/03/2020', y = 'Casos Covid-19')+
  theme_minimal(base_size = 22) + 
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))
gif_italia<-animate(p_italia, fps=4, width = 400, height = 400,  renderer = gifski_renderer(loop = F))
anim_save("italia.gif", gif_italia)

a_mgif <- image_read('brasil.gif')
b_mgif <- image_read('italia.gif')

new_gif <- image_append(c(a_mgif[1], b_mgif[1]))
for(i in 2:100){
  combined <- image_append(c(a_mgif[i], b_mgif[i]))
  new_gif <- c(new_gif, combined)
}

image_write(new_gif, path="covid_19.gif")


