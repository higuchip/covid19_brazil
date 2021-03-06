library(ggplot2)
library(readr)
library(polynom)


covid19 <- read_delim("covid19_brasil.csv",
                      ";", escape_double = FALSE, col_types = cols(datas = col_datetime(format = "%d/%m/%Y")),
                      trim_ws = TRUE)
covid19_1<-covid19
colnames(covid19_1)<-c('datas', "y", "x")
my.formula<-y~I(x^2)+I(x^3)
m <- lm(my.formula, covid19_1)
summary(m)
m
my.eq<-"9.8462 - 0.6667*x^2 + 0.0601*x^3"
p<-ggplot(data=covid19, aes(t,casos)) + ylim(0,350)+
  geom_point(alpha=0.7, shape=21, fill="red", colour="black", size=5) +
  geom_smooth(method="lm", se=TRUE, formula=y~I(x^2)+I(x^3), fullrange=TRUE)
label.text <- paste(gsub("x", "~italic(x)", my.eq, fixed = TRUE),
                    paste("italic(R)^2",
                          format(summary(m)$r.squared, digits = 2),
                          sep = "~`=`~"),
                    sep = "~~~~")
p<-p+labs(y="Casos confirmados de COVID-19", x = "Dias desde o primeiro caso registrado de COVID-19", caption = "Fonte: Ministério da Saúde (Dados atualizados em 16/03/2020 às 15:15)") +
  ggtitle("COVID-19 no Brasil", subtitle = "Atenção: este é apenas um exercício didático, sem fins científicos")
p<-p + theme_light(base_size = 14)
p<-p + annotate(geom = "text", x = 0, y = 350, label = label.text,
                family = "serif", hjust = 0, parse = TRUE, size =6)

jpeg(filename = "covid19.jpg",width = 2500, height = 2500,
     units = "px",
     quality = 100,
     bg = "white",
     res = 300, family="serif")

p+annotate(geom = "curve", x = 15, y = 280, xend = 20, yend = 234, 
  curvature = 0, arrow = arrow(length = unit(2, "mm"))) +
  annotate(geom = "text", x = 10, y = 285, label = "16/03/20", hjust = "left", cex=6)

dev.off()
