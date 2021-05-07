xmr <-
  function(x, index){
    require(data.table)
    require(ggplot2)

    lag <- data.table::shift(x, n=1)
    mr <- abs(x-lag)
    mean_s <- mean(x)
    mean_mr <- mean(na.omit(mr))

    UCL <- mean_s + (3*mean_mr / 1.128)
    LCL <- mean_s - (3*mean_mr / 1.128)

    rule <- ifelse(x>UCL,1,ifelse(x<LCL,3,2))

    ggplot(data.frame(index, x), aes(x=index, y=x))+
      geom_point(aes(col=factor(rule)))+
      geom_line()+
      geom_hline(yintercept=mean_s, col="black", linetype="dashed")+
      geom_hline(yintercept=UCL, col="dodgerblue2")+
      geom_hline(yintercept=LCL, col="purple")
  }


xmr(input$Crude.Death.rate, input$Year)