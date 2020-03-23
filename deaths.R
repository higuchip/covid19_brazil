
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gganimate)
library(magick)
library(gifski)

###Dados extraídos de Novel Coronavirus (COVID-19) Cases, provided by JHU CSSE https://systems.jhu.edu/research/public-health/ncov/

deaths_confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
head(deaths_confirmed)
colnames(deaths_confirmed)


deaths_confirmed_brasil <-deaths_confirmed %>% filter(`Country/Region` == 'Brazil')
deaths_confirmed_brasil <-deaths_confirmed_brasil[,5:dim(deaths_confirmed_brasil)[2]]
dim(deaths_confirmed_brasil)

deaths_confirmed_italia <-deaths_confirmed %>% filter(`Country/Region` == 'Italy')
deaths_confirmed_italia <-deaths_confirmed_italia[,5:dim(deaths_confirmed_italia)[2]]
dim(deaths_confirmed_italia)


deaths_confirmed_japao <-deaths_confirmed %>% filter(`Country/Region` == 'Japan')
deaths_confirmed_japao <-deaths_confirmed_japao[,5:dim(deaths_confirmed_japao)[2]]
dim(deaths_confirmed_japao)

deaths_confirmed_korea_s <-deaths_confirmed %>% filter(`Country/Region` == 'Korea, South')
deaths_confirmed_korea_s <-deaths_confirmed_korea_s[,5:dim(deaths_confirmed_korea_s)[2]]
dim(deaths_confirmed_korea_s)



deaths_confirmed_paises <- rbind(deaths_confirmed_korea_s, deaths_confirmed_brasil, deaths_confirmed_italia, deaths_confirmed_japao)
deaths_confirmed_paises<-t(deaths_confirmed_paises)
colnames(deaths_confirmed_paises)<-c('Coreia S.','Brasil','Italia', 'Japao')


deaths_confirmed_paises_long<-as.data.frame(deaths_confirmed_paises) %>% gather(pais, casos)


deaths_long_1 <- deaths_confirmed_paises_long %>% filter(casos>10)

tempo_deaths_coreia<-1:dim(deaths_long_1 %>% filter(pais=='Coreia S.'))[1]
tempo_deaths_brasil<-1:dim(deaths_long_1 %>% filter(pais=='Brasil'))[1]
tempo_deaths_italia<-1:dim(deaths_long_1 %>% filter(pais=='Italia'))[1]
tempo_deaths_japao<-1:dim(deaths_long_1 %>% filter(pais=='Japao'))[1]

deaths_long_1$tempo_1<-c(tempo_deaths_coreia,tempo_deaths_brasil, tempo_deaths_italia,tempo_deaths_japao)

death_gif_1 <- ggplot(deaths_long_1, aes(x=tempo_1, y=casos, group=pais)) +
  scale_y_log10()+
  ggtitle("COVID-19", subtitle = "Brasil, Coreia do Sul, Itália e Japão após décima mortalidade")+
  labs(y="Número de Mortos (log10)", x = "Tempo (dias)", caption = "Fonte dos dados: Johns Hopkins CSSE")+
  geom_line(size=2,aes(linetype=pais, color=pais)) +
  geom_segment(aes(xend=max(tempo_1), yend = casos), linetype=2, colour='black') +
  geom_point(size = 3) + 
  geom_text(size=5,aes(x = max(tempo_1), label = pais), hjust = 0) +
  transition_reveal(tempo_1) + 
  view_follow(fixed_y = TRUE)+
  coord_cartesian(clip = 'off') + 
  enter_drift(x_mod = -1) + exit_drift(x_mod = 1) +
  theme_light(base_size = 14) + 
  theme(plot.subtitle=element_text(size=12,  color="black"))+
  theme(plot.margin = margin(1.0, 40, 1.0, 1.0),legend.position = "none")

gif_deaths<-animate(death_gif_1, fps=4, duration = 20, renderer = gifski_renderer(loop = F))
anim_save("paises_deaths_caso_100.gif", gif_deaths)
