##########################################################
################# Confiança e Incerteza ##################


library(readr)
library(ggplot2)
library(gridExtra)
library(scales)
library(vars)

data = read_csv2('confianca.csv',
                 col_types = 
                   list(col_date(format='%d/%m/%Y'),
                        col_double(),
                        col_double(),
                        col_double(),
                        col_double(),
                        col_double(),
                        col_double()))

data = data[complete.cases(data),]

g1 = ggplot(data, aes(Data, icc_exp_sa))+
  geom_line()+
  scale_x_date(breaks = date_breaks("2 year"),
               labels = date_format("%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x='', y='Índice',
       title='Índice de Confiança do Consumidor')

g2 = ggplot(data, aes(Data, iie))+
  geom_line()+
  scale_x_date(breaks = date_breaks("2 year"),
               labels = date_format("%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(x='', y='Índice',
       title='Incerteza')

g3 = ggplot(data, aes(iie, icc_exp_sa))+
  geom_point(colour='black', size=2)+
  geom_smooth(method='lm', se=FALSE, colour='red',
              linetype='dashed')+
  labs(x='Incerteza', y='Confiança Consumidor',
       title='Incerteza vs. Confiança do Consumidor',
       caption='Fonte: analisemacro.com.br')

grid.arrange(g1, g2, g3, ncol=2, nrow=2,
             layout_matrix= rbind(c(1,2), c(3,3)))


