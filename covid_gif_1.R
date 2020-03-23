
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(magick)
library(gifski)

###Dados extraídos de Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE https://systems.jhu.edu/research/public-health/ncov/

covid_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
head(covid_confirmed)
colnames(covid_confirmed)


covid_confirmed_brasil <-covid_confirmed %>% filter(`Country/Region` == 'Brazil')
covid_confirmed_brasil <-covid_confirmed_brasil[,5:dim(covid_confirmed_brasil)[2]]
dim(covid_confirmed_brasil)

covid_confirmed_italia <-covid_confirmed %>% filter(`Country/Region` == 'Italy')
covid_confirmed_italia <-covid_confirmed_italia[,5:dim(covid_confirmed_italia)[2]]
dim(covid_confirmed_italia)


covid_confirmed_japao <-covid_confirmed %>% filter(`Country/Region` == 'Japan')
covid_confirmed_japao <-covid_confirmed_japao[,5:dim(covid_confirmed_japao)[2]]
dim(covid_confirmed_japao)


covid_confirmed_korea_s <-covid_confirmed %>% filter(`Country/Region` == 'Korea, South')
covid_confirmed_korea_s <-covid_confirmed_korea_s[,5:dim(covid_confirmed_korea_s)[2]]
dim(covid_confirmed_korea_s)

covid_confirmed_paises <- rbind(covid_confirmed_korea_s, covid_confirmed_brasil, covid_confirmed_italia, covid_confirmed_japao)
covid_confirmed_paises<-t(covid_confirmed_paises)
colnames(covid_confirmed_paises)<-c('Coreia S.','Brasil','Italia', 'Japao')


covid_confirmed_paises_long<-as.data.frame(covid_confirmed_paises) %>% gather(pais, casos)


covid_long_1st <- covid_confirmed_paises_long %>% filter(casos>0)

tempo_1_coreia<-1:dim(covid_long_1st %>% filter(pais=='Coreia S.'))[1]
tempo_1_brasil<-1:dim(covid_long_1st %>% filter(pais=='Brasil'))[1]
tempo_1_italia<-1:dim(covid_long_1st %>% filter(pais=='Italia'))[1]
tempo_1_japao<-1:dim(covid_long_1st %>% filter(pais=='Japao'))[1]


covid_long_1st$tempo_st<-c(tempo_1_coreia,
                           tempo_1_brasil,
                           tempo_1_italia,
                           tempo_1_japao)

p_gif <- ggplot(covid_long_1st, aes(x=tempo_st, y=casos, group=pais)) +
  scale_y_log10()+
  ggtitle("COVID-19", subtitle = "Brasil, Coreia do Sul, Itália e Japão após o primeiro caso")+
  labs(y="Casos confirmados (log10)", x = "Tempo (dias)", caption = "Fonte dos dados: Johns Hopkins CSSE")+
  geom_line(size=2,aes(linetype=pais, color=pais)) +
  geom_segment(aes(xend=max(tempo_st), yend = casos), linetype=2, colour='black') +
  geom_point(size = 3) + 
  geom_text(size=5,aes(x = max(tempo_st), label = pais), hjust = 0) +
  transition_reveal(tempo_st) + 
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') + 
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_light(base_size = 14) + 
  theme(plot.subtitle=element_text(size=12,  color="black"))+
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5),legend.position = "none")

gif_paises<-animate(p_gif, fps=4, duration = 20, renderer = gifski_renderer(loop = F))
anim_save("paises_covid_1.gif", gif_paises)
