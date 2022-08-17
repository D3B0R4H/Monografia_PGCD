
previsao_primeira_ordem = function(y,dlmy, meses, dados_treino, nome_coluna){
  
  yFilt = dlmFilter(y, dlmy)
  
  ySmooth = dlmSmooth(yFilt)
  
  yFore = dlmForecast(yFilt, nAhead = meses)
  
  
  sqrtR = sapply(yFore$R, function(x) sqrt(x[1,1]))
  pl = yFore$a[,1] + qnorm(0.05, sd = sqrtR)
  pu = yFore$a[,1] + qnorm(0.95, sd = sqrtR)
  x = ts.union(window(y, start = c(2014, 1)),
                window(ySmooth$s, start = c(2014, 1)),
                yFore$a[,1], pl, pu)
  
  prev=data.frame(x)
  
  data_am = unlist(1+(100*dados_treino[,2])+(10000*dados_treino[,1]), use.names = F)
  
  data_am_prev=append(data_am,unlist(1+(100*u_6_m[,2])+(10000*u_6_m[,1]), use.names = F))
  data_am_prev = as.Date(as.character(data_am_prev), format="%Y%m%d")
  
  prev["Data"]=data_am_prev
  prev["Y_real"]=dados[nome_coluna]
  #"Media_dias_chuva"
  
  return(prev)

}

previsao_segunda_ordem = function(y,dlmy, meses, dados_treino, nome_coluna){
  
  yFilt = dlmFilter(y, dlmy)
  
  ySmooth = dlmSmooth(yFilt)
  
  yFore = dlmForecast(yFilt, nAhead = meses)
  
  
  sqrtR <- sapply(yFore$R, function(x) sqrt(x[1,1]))
  pl <- yFore$f + qnorm(0.05, sd = sqrtR)
  pu <- yFore$f + qnorm(0.95, sd = sqrtR)
  x <- ts.union(window(y, start = c(2014, 1)),
                window(ySmooth$s[,1], start = c(2014, 1)),
                yFore$f[,1], pl, pu)
  
  prev=data.frame(x)
  
  data_am = unlist(1+(100*dados_treino[,2])+(10000*dados_treino[,1]), use.names = F)
  
  data_am_prev=append(data_am,unlist(1+(100*u_6_m[,2])+(10000*u_6_m[,1]), use.names = F))
  data_am_prev = as.Date(as.character(data_am_prev), format="%Y%m%d")
  
  prev["Data"]=data_am_prev
  prev["Y_real"]=dados[nome_coluna]
  
  return(prev)
  
}
