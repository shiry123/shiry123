####################################################
opt.relax=function(data, x.col, obj_model, k=0, n=1){
  x0=data %>% summarise(across(all_of(x.col),  #选择x变量
                               list(x.mean=mean, x.lb=min, x.ub=max),
                               .names = "{.col}_{.fn}")) %>% 
    pivot_longer(everything(), names_to = c("x.var",".value"),names_sep = "_")
  pars.set=x0 %>% 
    mutate(delta=map2_dbl(x.lb,x.ub, ~(.y-.x)/n)) %>%  # stepsize 10% of initial design
    mutate(
      xx.lb=map2_dbl(x.lb,delta, ~.x-k*.y),
      xx.ub=map2_dbl(x.ub,delta, ~.x+k*.y)
    ) 
  pars.set=as.data.frame(pars.set)
  
  #目标函数 优化最大值还是最小值
  obj   =function(x) {
    xx               <- data.frame(rbind(x))
    colnames(xx)     <- pars.set$x.var
    return( predict(obj_model, xx))#note: -obj <=> max(obj)  min(-x)=max(x)
  }
  minus.obj = function(x) {
    xx               <- data.frame(rbind(x))
    colnames(xx)     <- pars.set$x.var
    return( -predict(obj_model, xx))#note: -obj <=> max(obj)  min(-x)=max(x)
  }
  
  library(Rsolnp)
  #函数 gosolnp()
  res.nlp=solnp(fun=minus.obj, pars=pars.set$x.mean,
                LB=pars.set$xx.lb, UB=pars.set$xx.ub) 
  tt <- system.time(res.nlp)
  x.pars=round(res.nlp$pars,3)
  x.solution=data.frame(rbind(x.pars)) #2
  rownames(x.solution)=NULL
  colnames(x.solution)=pars.set$x.var
  x.solution$relax.step=k*1/n*100  #百分多少
  x.solution$t.min=round(tt[3]/60,2) 
  x.solution$return.code=res.nlp$convergence
  ######
  return(x.solution)
}
