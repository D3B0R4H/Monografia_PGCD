setwd("C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia")
#Pacote
library("dlm")
library(ggplot2)
library(hrbrthemes)

#Importação dos dados

dados = read.csv("chuvas_2014_a_2021Maximo_dv2.csv", dec='.')

#Últimos 6 meses - Validação
total = nrow(dados)
meses_f = seq(from = total-5,to = total,1)
u_6_m = dados[meses_f,]

#Dados sem os últimos 6 meses - Treinamento
dados_treino = dados[-meses_f,]

rm(total);rm(meses_f)

#Selecionando apenas os valores da série, sem os últimos 6 meses
y = dados_treino[,3]

#Transformando em uma série temporal
y = ts(y,frequency=12,start=c(2014,1))

#Visualização da série
ts.plot(y)

#Criando os dados
data_am = unlist(1+(100*dados_treino[,2])+(10000*dados_treino[,1]), use.names = F)

xValue = as.Date(as.character(data_am), format="%Y%m%d")
yValue = unlist(dados_treino[3], use.names = F)
data = data.frame(xValue,yValue)

#Visualização com ggplot2
# Série Temporal
save_plot = ggplot( data, aes(x=xValue, y=yValue)) +
  geom_line( color="black", size=0.5, alpha=0.9, linetype=1) +
  labs(x="Mês", y = "Máximo mensal")+
  scale_x_date(date_breaks = "3 month" , date_labels = "%Y \ %b")+
  theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5, size =14),axis.title.x = element_text(size=20),axis.title.y = element_text(size = 20))
ggsave(
  "TS_Maximo.jpeg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)



# Histograma
save_plot = ggplot(data, aes(x = yValue)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white", bins = 10)+
  geom_density(alpha=.2, fill="blue")+ 
  labs(x="Máximo", y = "Densidade")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20))

ggsave(
  "Hist_Máximo.jpeg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)



#Métricas
summary(y)


##Estimando os parâmetros pela máxima verossimilhança.

buildFun = function(x) {
  
  dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2]))
}

fit = dlmMLE(y, parm = c(10,10), build = buildFun)

# Estimação V W
coef = fit$par ; coef


dlmy = buildFun(fit$par)

V(dlmy); W(dlmy)


##Filtro de Kalman

yFilt = dlmFilter(y, dlmy)

plot(y, type = 'o', col = "seagreen")
lines(dropFirst(yFilt$m), type = 'o',pch = 20, col = "black")

filtro_d = data.frame("Data" = data[,1]
                      ,"Dados" = as.numeric(data[,2])
                      ,"yFilt_f" = yFilt$f )

save_plot = ggplot( filtro_d, aes(x=Data, y=Dados)) +
  labs(x="Mês", y = "Volume de chuva")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20)) +
  geom_line(aes(y=Dados),col='darkgrey', size = 1)+       
  geom_line(aes(y=yFilt_f),col='grey', size = 1, linetype = "dashed")

ggsave(
  "Kalman_Maximo.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)


##Smooth 

ySmooth = dlmSmooth(yFilt)
plot(y, type = 'o', col = "black", xlab = "Mês", ylab = "Média dos dias de chuva")
attach(ySmooth)
lines(dropFirst(s), type = 'o', pch = 20, col = "gray")
v = unlist(dlmSvd2var(U.S, D.S))
pl = dropFirst(s) + qnorm(0.05, sd = sqrt(v[-1]))
pu = dropFirst(s) + qnorm(0.95, sd = sqrt(v[-1]))
detach()
lines(pl, lty = 2, col = "brown")
lines(pu, lty = 2, col = "brown")

smooth_d = data.frame("Data" = data[,1]
                      ,"Dados" = as.numeric(data[,2])
                      ,"s" = dropFirst(ySmooth$s)
                      ,"pl"=pl
                      ,"pu"=pu)

save_plot = ggplot( smooth_d, aes(x=Data, y=Dados)) +
  labs(x="Mês", y = "Volume de chuva")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20)) +
  geom_line(aes(y=Dados),col='darkgrey', size = 1)+       
  geom_line(aes(y=s),col='gray', size = 1, linetype = "dashed")+
  geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade

ggsave(
  "Smooth_Maximo.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)


# Previsão, com apenas nível


yFore <- dlmForecast(yFilt, nAhead = 6)


sqrtR <- sapply(yFore$R, function(x) sqrt(x[1,1]))
pl <- yFore$a[,1] + qnorm(0.05, sd = sqrtR)
pu <- yFore$a[,1] + qnorm(0.95, sd = sqrtR)
x <- ts.union(window(y, start = c(2014, 1)),
              window(ySmooth$s, start = c(2014, 1)),
              yFore$a[,1], pl, pu)

prev=data.frame(x)

data_am_prev=append(data_am,unlist(1+(100*u_6_m[,2])+(10000*u_6_m[,1]), use.names = F))
data_am_prev = as.Date(as.character(data_am_prev), format="%Y%m%d")

prev["Data"]=data_am_prev
prev["Y_real"]=dados["Maximo"]

save_plot = ggplot( prev, aes(x=Data, y=window.y..start...c.2014..1..)) +
  labs(x="Mês", y = "Volume")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20))+
  geom_line(aes(y=window.ySmooth.s..start...c.2014..1..),col='grey')+       # Suavizado
  geom_line(aes(y=Y_real),col='darkgrey', size = 0.5, linetype = "dashed")+ # O que ocorreu
  geom_line(aes(y=),col='darkgrey', size = 1)+                              # Dados reais
  geom_line(aes(y=yFore.a...1.),col='blue', size = 1)+                      # Previsão
  geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade

ggsave(
  "Prev_Maximo.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)


rm(dlmy);rm(fit);rm(x);rm(yFilt);rm(yFore);rm(ySmooth);rm(coef);rm(pl);rm(pu);rm(sqrtR);rm(v);rm(xValue);rm(yValue);rm(buildFun)


#### Erro


erro_abs = c()
erro_quadratico =c()
p = c()
meses = 6-1
tam = length(prev$Y_real)

for(i in 0:meses){
  
  real = prev$Y_real[tam-i]
  
  previsto =  prev$yFore.a...1.[tam-i]
  
  erro = real-previsto
  
  erro_abs[i+1] = abs(erro)
  erro_quadratico[i+1] =c(erro^2)  
  p[i+1]=abs(100*erro/real)
  
}

#Erro médio absoluto (MAE)
MAE = mean(erro_abs)

#Raiz do erro quadrático médio (RMSE)

RMSE = sqrt(mean(erro_quadratico))

# Erro médio absoluto percentual médio

MAPE = mean(p)

rm(prev);rm(erro_abs);rm(erro_quadratico);rm(erro);rm(real);rm(previsto);rm(tam);rm(meses);rm(MAE);rm(RMSE);rm(MAPE);rm(i);rm(p)

# Previsão com nível e tendência

#Estimando os parâmetros pela máxima verossimilhança.

dlmy = dlmModPoly(order = 2, dW=c(1,1))

buildFun <- function(x) {
  diag(W(dlmy)) <- exp(x[1:2])
  V(dlmy) <- exp(x[3])
  return(dlmy)
}

fit = dlmMLE(y, parm =  rep(0,6), build = buildFun)


# Estimação V W
coef = fit$par ; coef


dlmy = buildFun(fit$par)

V(dlmy); W(dlmy)


#Filtro de Kalman

yFilt = dlmFilter(y, dlmy)

plot(y, type = 'o', col = "seagreen")
lines(dropFirst(yFilt$m)[,1], type = 'o',pch = 20, col = "black")


plot(yFilt$m[,2]) # Colocar IC


filtro_d = data.frame("Data" = data[,1]
                      ,"yFilt_m" = dropFirst(yFilt$m[,2] ))

save_plot = ggplot( filtro_d, aes(x=Data, y=yFilt_m)) +
  labs(x="Mês", y = "Tendência")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20)) +
  geom_line(aes(y=yFilt_m),col='darkgrey', size = 1)

ggsave(
  "Kalman_Maximot.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)

filtro_d = data.frame("Data" = data[,1]
                      ,"yFilt_m" = dropFirst(yFilt$m[,1] ))

save_plot = ggplot( filtro_d, aes(x=Data, y=yFilt_m)) +
  labs(x="Mês", y = "Nível")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20)) +
  geom_line(aes(y=yFilt_m),col='darkgrey', size = 1)

ggsave(
  "Kalman_MaximoNivel.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)


##Smooth 

ySmooth = dlmSmooth(yFilt)
x <- cbind(y, dropFirst(ySmooth$s[,2]))
colnames(x) <- c("Volume", "Tendência")
plot(x, type = 'l')


#Previsão com tendência, 6 meses a frente

yFore <- dlmForecast(yFilt, nAhead = 6)


sqrtR <- sapply(yFore$R, function(x) sqrt(x[1,1]))
pl <- yFore$f + qnorm(0.05, sd = sqrtR)
pu <- yFore$f + qnorm(0.95, sd = sqrtR)
x <- ts.union(window(y, start = c(2014, 1)),
              window(ySmooth$s[,1], start = c(2014, 1)),
              yFore$f[,1], pl, pu)

prev=data.frame(x)

data_am_prev=append(data_am,unlist(1+(100*u_6_m[,2])+(10000*u_6_m[,1]), use.names = F))
data_am_prev = as.Date(as.character(data_am_prev), format="%Y%m%d")

prev["Data"]=data_am_prev
prev["Y_real"]=dados["Maximo"]

save_plot = ggplot( prev, aes(x=Data, y=window.y..start...c.2014..1..)) +
  labs(x="Mês", y = "Volume")+
  theme(axis.title.x = element_text(size=20), axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 20))+
  geom_line(aes(y=window.ySmooth.s...1...start...c.2014..1..),col='grey')+       # Suavizado
  geom_line(aes(y=Y_real),col='darkgrey', size = 0.5, linetype = "dashed")+ # O que ocorreu
  geom_line(aes(y=),col='darkgrey', size = 1)+                              # Dados reais
  geom_line(aes(y=yFore.f...1.),col='blue', size = 1)+                      # Previsão
  geom_ribbon(aes(ymin=pl,ymax=pu),alpha=.25, fill = 'blue')                # Intervalo de credibilidade

ggsave(
  "Prev_Maximot.jpg",
  path ="C:/Users/debor/OneDrive/Propria - Estudo/Pós-Graduação Ciencia de Dados/Monografia/Gráficos",
  width = 30, 
  height = 20, 
  units = "cm"
)


rm(dlmy);rm(fit);rm(x);rm(yFilt);rm(yFore);rm(ySmooth);rm(coef);rm(pl);rm(pu);rm(sqrtR);rm(v);rm(xValue);rm(yValue);rm(buildFun)


#### Erro

erro_abs = c()
erro_quadratico =c()
p = c()
meses = 6-1
tam = length(prev$Y_real)

for(i in 0:meses){
  
  real = prev$Y_real[tam-i]
  
  previsto =  prev$yFore.f...1.[tam-i]
  
  erro = real-previsto
  
  erro_abs[i+1] = abs(erro)
  erro_quadratico[i+1] =c(erro^2)  
  p[i+1]=abs(100*erro/real)
  
}

#Erro médio absoluto (MAE)
MAE = mean(erro_abs)

#Raiz do erro quadrático médio (RMSE)

RMSE = sqrt(mean(erro_quadratico))

# Erro médio absoluto percentual médio

MAPE = mean(p)

rm(prev);rm(erro_abs);rm(erro_quadratico);rm(erro);rm(real);rm(previsto);rm(tam);rm(meses)
