
estimacao_parametros_primeira_ordem = function(parm1, parm2, y){

  ##Estimando os parâmetros pela máxima verossimilhança.
  
  buildFun = function(x) {
    
    dlmModPoly(1, dV = exp(x[1]), dW = exp(x[2]))
  }
  
  fit = dlmMLE(y, parm = c(parm1, parm2), build = buildFun)
  
  
  dlmy = buildFun(fit$par)
  
  return(dlmy)
  
}


estimacao_parametros_segunda_ordem = function(parm1, parm2, y){

  dlmy = dlmModPoly(order = 2, dW=c(1,1))
  
  buildFun <- function(x) {
    diag(W(dlmy)) <- exp(x[1:2])
    V(dlmy) <- exp(x[3])
    return(dlmy)
  }
  
  fit = dlmMLE(y, parm =  rep(parm1, parm2), build = buildFun)
  
  
  # Estimação V W
  #coef = fit$par ; coef
  
  
  dlmy = buildFun(fit$par)
  
  #V(dlmy); W(dlmy)

  return(dlmy)
  
}
