#Pacote
library("dlm")
library(ggplot2)
library(hrbrthemes)

setwd("C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia")

#Importa��o dos dados

dados = read.csv("chuvas_2014_a_2021Acumuladav2.csv", dec='.')

#Separando os �ltimos meses para valida��o
u_6_m = criacao_dados_validacao(dados)

#Separando os dados para treinamento
dados_treino = criacao_dados_treino(dados)

#Cria��o de uma s�rie temporal
y = tratamento_y(dados_treino)


data = tratamento_dados(dados_treino)



#Visualiza��o com ggplot2
# S�rie Temporal
# save_plot = ggplot( data, aes(x=xValue, y=yValue)) +
#   geom_line( color="black", size=0.5, alpha=0.9, linetype=1) +
#   labs(x="M�s", y = "Acumulado mensal")+
#   scale_x_date(date_breaks = "3 month" , date_labels = "%Y \ %b")+
#   theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size =14),axis.title.x = element_text(size=20),axis.title.y = element_text(size = 20))
# 
# ggsave(
#   "TS_Acumulada.jpeg",
#   path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
#   width = 30, 
#   height = 20, 
#   units = "cm"
# )
# Histograma
# save_plot = ggplot(data, aes(x = yValue)) + 
#   geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 10)+
#   geom_density(alpha=.2, fill="blue")+ 
#   labs(x="Acumulado de chuva", y = "Densidade")+
# theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
#       axis.title.y = element_text(size = 20))
#   
# ggsave(
#   "Hist_Acumulada.jpeg",
#   path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
#   width = 30, 
#   height = 20, 
#   units = "cm"
# )

#M�tricas
#summary(y)

##Estimando os par�metros pela m�xima verossimilhan�a.

dlmy = estimacao_parametros_primeira_ordem(10,10,y)

##Filtro de Kalman


filtro_d = kalman_primeira_ordem(y, dlmy, data)


# save_plot = ggplot( filtro_d, aes(x=Data, y=Dados)) +
#   labs(x="M�s", y = "Volume de chuva")+
#   theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
#         axis.title.y = element_text(size = 20)) +
#   geom_line(aes(y=Dados),col='darkgrey', size = 1)+       
#   geom_line(aes(y=yFilt_f),col='grey', size = 1, linetype = "dashed")+
#   geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade
# 
# ggsave(
#   "Kalman_Acumulada.jpg",
#   path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
#   width = 30, 
#   height = 20, 
#   units = "cm"
# )

##Smooth 

smooth_d = smooth_primeira_ordem(y, data)

# save_plot = ggplot( smooth_d, aes(x=Data, y=Dados)) +
#   labs(x="M�s", y = "Volume de chuva")+
#   theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
#         axis.title.y = element_text(size = 20)) +
#   geom_line(aes(y=Dados),col='darkgrey', size = 1)+       
#   geom_line(aes(y=s),col='gray', size = 1, linetype = "dashed")+
#   geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade
# 
# ggsave(
#   "Smooth_Acumulada.jpg",
#   path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
#   width = 30, 
#   height = 20, 
#   units = "cm"
# )

# Previs�o, com apenas n�vel

prev = previsao_primeira_ordem(y,dlmy, meses=6, dados_treino, "Acumulada")

# save_plot = ggplot( prev, aes(x=Data, y=window.y..start...c.2014..1..)) +
#   labs(x="M�s", y = "Volume")+
#   theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
#         axis.title.y = element_text(size = 20))+
#   geom_line(aes(y=window.ySmooth.s..start...c.2014..1..),col='grey')+       # Suavizado
#   geom_line(aes(y=Y_real),col='darkgrey', size = 0.5, linetype = "dashed")+ # O que ocorreu
#   geom_line(aes(y=),col='darkgrey', size = 1)+                              # Dados reais
#   geom_line(aes(y=yFore.a...1.),col='blue', size = 1)+                      # Previs�o
#   geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade
# 
# ggsave(
#   "Prev_Acumulada.jpg",
#   path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
#   width = 30, 
#   height = 20, 
#   units = "cm"
# )


#### Erro

erro_primeira_ordem(prev)

rm(dlmy);rm(filtro_d);rm(prev);rm(smooth_d)

# Previs�o com n�vel e tend�ncia

#Estimando os par�metros pela m�xima verossimilhan�a.

dlmy = estimacao_parametros_segunda_ordem(0,6,y)

#Filtro de Kalman

filtro_d = kalman_segunda_ordem(y, dlmy, data)

save_plot = ggplot( filtro_d[-1,], aes(x=Data, y=yFilt_m)) +
  labs(x="M�s", y = "Tend�ncia")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20)) +
  geom_line(aes(y=yFilt_m),col='darkgrey', size = 1)+
  geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')

ggsave(
  "Kalman_Acumuladat.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
  width = 30, 
  height = 20, 
  units = "cm"
)


save_plot = ggplot( filtro_d, aes(x=Data, y=yFilt_m)) +
  labs(x="M�s", y = "N�vel")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20)) +
  geom_line(aes(y=Dados),col='darkgrey', size = 1)+
  geom_line(aes(y=yFilt_m1),col='grey', size = 1, linetype = "dashed")+
  geom_ribbon(aes(ymin=pl1,ymax=pu1),alpha=.25, fill = 'blue')

ggsave(
  "Kalman_AcumuladaNivel.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
  width = 30, 
  height = 20, 
  units = "cm"
)


##Smooth 

ySmooth = dlmSmooth(dlmFilter(y, dlmy))


#Previs�o com tend�ncia, 6 meses a frente

prev = previsao_segunda_ordem(y,dlmy, meses = 6, dados_treino, "Acumulada")

# save_plot = ggplot( prev, aes(x=Data, y=window.y..start...c.2014..1..)) +
#   labs(x="M�s", y = "Volume")+
#   theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
#         axis.title.y = element_text(size = 20))+
#   geom_line(aes(y=window.ySmooth.s...1...start...c.2014..1..),col='grey')+       # Suavizado
#   geom_line(aes(y=Y_real),col='darkgrey', size = 0.5, linetype = "dashed")+ # O que ocorreu
#   geom_line(aes(y=),col='darkgrey', size = 1)+                              # Dados reais
#   geom_line(aes(y=yFore.f...1.),col='blue', size = 1)+                      # Previs�o
#   geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade
# 
# 
# ggsave(
#   "Prev_Acumuladat.jpg",
#   path ="C:/Users/debor/OneDrive/Propria - Estudo/P�s-Gradua��o Ciencia de Dados/Monografia/Gr�ficos",
#   width = 30, 
#   height = 20, 
#   units = "cm"
# )


erro_segunda_ordem(prev,6)

rm(dados);rm(dados_treino);rm(data);rm(dlmy);rm(filtro_d);rm(prev);rm(u_6_m);rm(ySmooth);rm(save_plot);rm(y)

