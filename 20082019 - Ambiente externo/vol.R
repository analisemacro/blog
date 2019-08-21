#################################################
###### Volatilidade e Taxa de Câmbio ############



library(quantmod)
library(ggplot2)
library(scales)
library(Quandl)
library(xts)
library(vars)
library(gridExtra)

## Juro 10 anos EUA
juro = Quandl('FRED/DGS10', start_date='2018-01-01')
ggplot(juro, 
       aes(Date, Value))+
  geom_line(size=.8)+
  scale_x_date(breaks = date_breaks("1 months"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  labs(title='Juro de 10 anos dos EUA')+
  xlab('')+ylab('% a.a.')

## Índice de Volatilidade VIX
getSymbols('VIXCLS', src='FRED')
vix = window(VIXCLS[complete.cases(VIXCLS),], start='2018-01-01')
autoplot(vix)+
  geom_line(size=.8, colour='darkblue')+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab('')+ylab('Índice')+
  labs(title='Índice de Volatilidade (VIX-CBOE)',
       caption='Fonte: analisemacro.com.br com dados do FRED')

## Taxa de Câmbio R$/US$
getSymbols("BRL=X",src="yahoo", from=as.Date('2018-01-01'))
cambio = `BRL=X`[,4]
cambio = cambio[complete.cases(cambio)]
autoplot(cambio)+
  geom_line(size=.8, colour='darkblue')+
  geom_hline(yintercept=4, colour='red', linetype='dashed')+
  scale_x_date(breaks = date_breaks("1 month"),
               labels = date_format("%b/%Y"))+
  theme(axis.text.x=element_text(angle=45, hjust=1))+
  xlab('')+ylab('R$/US$')+
  labs(title='Taxa de Câmbio diária R$/US$',
       caption='Fonte: analisemacro.com.br com dados do Yahoo Finance.')

## Câmbio vs. Volatilidade


data = cbind(cambio, vix)
data = data[complete.cases(data)]
colnames(data) = c('Taxa de Câmbio', 'Índice de Volatilidade')

autoplot(data)+facet_free()+
  xlab('')+
  geom_line(size=.8, colour='darkblue')+
  scale_x_date(breaks = date_breaks("1 year"),
               labels = date_format("%Y"))
  

df = data.frame(cambio=data[,1], volatilidade=data[,2])
colnames(df) = c('cambio', 'volatilidade')
colnames(data) = c('cambio', 'volatilidade')

ggplot(df, aes(volatilidade, cambio))+
  geom_point(stat='identity', colour='black', size=3,
             shape=21, fill='orange')+
  geom_smooth(method='lm', colour='black')+
  xlab('Índice de Volatilidade (VIX)')+ylab('Taxa de Câmbio R$/US$')+
  labs(title='Volatilidade vs. Taxa de Câmbio')+
  theme_bw()


## Estimando um var

def = VARselect(data)
def$selection

var <- VAR(data, p=min(def$selection), type='const')

irf = irf(var, impulse='volatilidade', response='cambio', 
          n.ahead = 12, boot=T, ortho=T, cumulative=F)
lags = 1:13
df.irf <- data.frame(irf=irf$irf, lower=irf$Lower, upper=irf$Upper,
                     lags=lags)
colnames(df.irf) <- c('irf', 'lower', 'upper', 'lags')
number_ticks <- function(n) {function(limits) pretty(limits, n)}
ggplot(data = df.irf,aes(x=lags,y=irf)) +
  geom_line(aes(y = upper), colour = 'lightblue2') +
  geom_line(aes(y = lower), colour = 'lightblue')+
  geom_line(aes(y = irf), size=.8)+
  geom_ribbon(aes(x=lags, ymax=upper, 
                  ymin=lower), 
              fill="blue", alpha=.1) +
  xlab("") + ylab("Taxa de Câmbio") + 
  ggtitle("Resposta ao Impulso em Volatilidade") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),                    
        axis.ticks.x=element_blank(),
        plot.margin = unit(c(2,10,2,10), "mm"))+
  geom_line(colour = 'black')+
  scale_x_continuous(breaks=number_ticks(13))+
  theme_bw()


fevd(var, 12)
