mean_plot = function(data, x, y, color=NULL, ...) {
  x_var = enquo(x)
  y_var = enquo(y)
  color = enquo(color)
  group_vars <- enquos(..., .named = TRUE)
  # g.l=length(group_vars)
  x_judge = pull(data, !!x_var)
  if (is.numeric(x_judge)) {
    # if(g.l==0){
    #   dat1=data %>%
    #     get_summary_stats(!!y_var, type = "mean_sd")
    #   p=ggplot(data, aes(x=!!x_var, y=!!y_var))+
    #     geom_point(aes(color=!!color))+
    #     theme_bw()+
    #     theme(axis.text.x = element_text(angle=90,vjust=0.5))+
    #     ggtitle(str_c(as_label(y_var), " vs. ", as_label(x_var)))
    # } else {
      # grp1=group_vars[[1]]
      dat1=data %>%
        group_by(!!!group_vars) %>%
        get_summary_stats(!!y_var, type = "mean_sd")
      p=ggplot(data, aes(x=!!x_var, y=!!y_var))+
        geom_point(aes(color=!!color))+
        theme_bw()+
        theme(axis.text.x = element_text(angle=90,vjust=0.5))+
        ggtitle(str_c(as_label(y_var), " vs. ", as_label(x_var)))+
        # labs(x=NULL)+
        facet_nested(cols=vars(!!!group_vars))
    # }
  } else {
    dat1=data %>%
      group_by(!!x_var, !!!group_vars) %>%
      get_summary_stats(!!y_var, type = "mean_sd")
    p=ggplot(data, aes(x=!!x_var, y=!!y_var))+
      geom_jitter(aes(color=!!color), width = 0.1)+ 
      stat_summary(fun = mean, geom = "crossbar",color = "black",
                   width = 0.3, size = 0.2)+
      stat_summary(fun.min = function(x) {mean(x) - sd(x)},
                   fun.max = function(x) {mean(x) + sd(x)},
                   geom = "errorbar", color = "black", width=0.2, size = 0.2)+
      geom_text(data=dat1, aes(label=mean, y=mean), 
                size=3, hjust =0, vjust=0, color="black")+
      geom_text(data=dat1, aes(label=paste("n=",{n}), y=-Inf), 
                size=3, hjust =0.5, vjust=-1, color="black")+
      theme_bw()+
      theme(axis.text.x = element_text(angle=90,vjust=0.5))+
      ggtitle(str_c(as_label(y_var), " vs. ", as_label(x_var)))+
      # labs(x=NULL)+
      facet_nested(cols=vars(!!!group_vars))
  }
  return(list(dat1, p))
}
