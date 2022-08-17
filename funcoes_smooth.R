# Primeira ordem

smooth_primeira_ordem = function(y, dlmy_ , data){
  
  yFilt = dlmFilter(y, dlmy_)
  
  ySmooth = dlmSmooth(yFilt)
  
  attach(ySmooth)
  v = unlist(dlmSvd2var(U.S, D.S))
  pl = dropFirst(s) + qnorm(0.05, sd = sqrt(v[-1]))
  pu = dropFirst(s) + qnorm(0.95, sd = sqrt(v[-1]))
  detach()
  
  
  smooth_d = data.frame("Data" = data[,1]
                        ,"Dados" = as.numeric(data[,2])
                        ,"s" = dropFirst(ySmooth$s)
                        ,"pl"=pl
                        ,"pu"=pu)
  return(smooth_d)
}