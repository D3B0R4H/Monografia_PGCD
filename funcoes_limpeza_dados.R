criacao_dados_validacao = function(dados,meses=6){
  #Últimos 6 meses - Validação
  meses=meses-1
  total = nrow(dados)
  meses_f = seq(from = total-meses,to = total,1)
  u_6_m = dados[meses_f,]
  
  return(u_6_m)
}

criacao_dados_treino = function(dados,meses=6){
  meses=meses-1
  #Últimos 6 meses - Validação
  total = nrow(dados)
  meses_f = seq(from = total-meses,to = total,1)
  
  #Dados sem os últimos 6 meses - Treinamento
  dados_treino = dados[-meses_f,]
  
  return(dados_treino)
}

tratamento_y = function(dados_treino){
  
  #Selecionando apenas os valores da série, sem os últimos 6 meses
  y = dados_treino[,3]
  
  #Transformando em uma série temporal
  y = ts(y,frequency=12,start=c(2014,1))
  
  return(y)
}

tratamento_dados=function(dados_treino){  
  #Criando os dados
  data_am = unlist(1+(100*dados_treino[,2])+(10000*dados_treino[,1]), use.names = F)
  
  xValue = as.Date(as.character(data_am), format="%Y%m%d")
  yValue = unlist(dados_treino[3], use.names = F)
  data = data.frame(xValue,yValue)
  
  return(data)
}
