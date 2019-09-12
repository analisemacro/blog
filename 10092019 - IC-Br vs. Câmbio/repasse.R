


library(tidyverse)
library(scales)
library(png)
library(grid)



data = read_csv2('data.csv', col_types = list(col_date(format='%d/%m/%Y'), 
                                              col_double(), 
                                              col_double()))


img <- readPNG('logo.png')
g <- rasterGrob(img, interpolate=TRUE)

ggplot(data, aes(x=date))+
  geom_line(aes(y=ic_br, colour='IC-Br'), size=.8)+
  geom_line(aes(y=cambio*50, colour='Taxa de Câmbio'), size=.8)+
  scale_y_continuous(sec.axis = sec_axis(~./50, name='Taxa de Câmbio'))+
  scale_colour_manual('Séries Utilizadas',
                      values=c('#244747', 'red'))+
  theme(legend.position = c(.8,.2))+
  xlab('')+ylab('IC-Br')+
  scale_x_date(breaks = date_breaks("1 year"),
               labels = date_format("%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title='IC-Br vs. Taxa de Câmbio',
       caption='Fonte: analisemacro.com.br')


ggplot(tail(data,24), aes(x=tail(date,24)))+
  geom_line(aes(y=tail(ic_br,24)), colour='#244747', size=.8)+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x='', y='Índice',
       title='Índice de Commodities Brasil',
       caption='Fonte: analisemacro.com.br')
