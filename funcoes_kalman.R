## Filtro de Kalman

# Primeira ordem

kalman_primeira_ordem = function(y, dlmy, data){

  yFilt = dlmFilter(y, dlmy)
  
  erro = c()
  for(t in 2:95)
  {
    
    erro[t-1] = 1.96*sqrt(yFilt$U.C[[t]] %*% diag(yFilt$D.C[t,]^2,1,1) %*% t(yFilt$U.C[[t]]))
  }
  
  filtro_d = data.frame("Data" = data[,1]
                        ,"Dados" = as.numeric(data[,2])
                        ,"yFilt_f" = yFilt$f 
                        ,"pl"=dropFirst(yFilt$m)+erro
                        ,"pu"=dropFirst(yFilt$m)-erro)
  return(filtro_d)
  
}

# Segunda ordem

kalman_segunda_ordem = function(y, dlmy, data){

  yFilt = dlmFilter(y, dlmy)

    erro = array(NA,dim=c(2,2,94))
  for(t in 2:95)
  {
    
    erro[,,t-1] = 1.96*sqrt(yFilt$U.C[[t]] %*% diag(yFilt$D.C[t,]^2) %*% t(yFilt$U.C[[t]]))
  }
  
  
  filtro_d = data.frame("Data" = data[,1]
                        ,"Dados" = as.numeric(data[,2])
                        ,"yFilt_m" = dropFirst(yFilt$m[,2] )
                        ,"yFilt_m1" = dropFirst(yFilt$m[,1] )
                        ,"pl"=dropFirst(yFilt$m[,2])-erro[2,2,]
                        ,"pu"=dropFirst(yFilt$m[,2])+erro[2,2,]
                        ,"pl1"=dropFirst(yFilt$m[,1])-erro[1,1,]
                        ,"pu1"=dropFirst(yFilt$m[,1])+erro[1,1,])
  return(filtro_d)
}
