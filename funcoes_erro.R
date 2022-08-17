#### Erro

erro_primeira_ordem = function(prev, meses_entrada = 6){

  erro_abs = c()
  erro_quadratico =c()
  p = c()
  meses = meses_entrada-1
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
  print("MAE") ;print(MAE)
  
  #Raiz do erro quadrático médio (RMSE)
  
  RMSE = sqrt(mean(erro_quadratico))
  print("RMSE") ;print(RMSE)
  
  # Erro médio absoluto percentual médio
  
  MAPE = mean(p)
  print("MAPE") ;print(MAPE)
}

erro_segunda_ordem = function(prev, meses_entrada = 6){
  
  erro_abs = c()
  erro_quadratico =c()
  p = c()
  meses = meses_entrada-1
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
  print("MAE") ;print(MAE)
  
  #Raiz do erro quadrático médio (RMSE)
  
  RMSE = sqrt(mean(erro_quadratico))
  print("RMSE") ;print(RMSE)
  
  # Erro médio absoluto percentual médio
  
  MAPE = mean(p)
  print("MAPE") ;print(MAPE)
}

