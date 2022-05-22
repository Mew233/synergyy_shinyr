library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggbeeswarm)

qq_plot = function(input) {
  
  #qq plot
  data <- input

    
  data %>% filter(Loewe.score >= -60 & Loewe.score <= 40)
  
  coolwarm_hcl <- colorspace::diverging_hcl(7, h = c(250, 10), c = 100, l = c(35, 95), power = c(0.7,1.3))
  p = ggplot(data, aes(y =Proba, x = Loewe.score,color=Loewe.score, ))
  fig1 = p + guides(fill=guide_legend(title="Actual Loewe score",label=FALSE)) + 
    scale_colour_gradient2(limits=c(-60, 40),midpoint = input$synscore)  +
    geom_point(aes(size = (Proba*10)^2)) +
    scale_size(name   = "predicted proba",
               breaks = fivenum(data$Proba*10)^2,
               labels = c("","","","","")) +
    xlim(-50, 50) + ylim(0, 1) + xlab('Actual score') +ylab('Predicted Proba') + labs(title=input$d1)
    
    ## qqplot
    
    # data$status <- ifelse((data$Proba > 0.5) == data$Actuals, 'true', 'false')
    # data = data %>% mutate(quantile = ntile(Loewe.score, 100))
    # set = data %>% count(status, quantile)
    # set2 = set %>% group_by(quantile) %>% mutate(PROC = n/sum(n))
    # set3 = set2 %>% group_by(quantile) %>% filter(!(status=="false" & n() > 1))
    # p <- ggplot(set3, aes(x= quantile, y=PROC))
    # fig2 = p+ geom_point() + geom_vline(xintercept=2)
    
    
  
  returns = list(fig1=NULL, fig2=NULL)
  returns$fig1 = fig1
  #returns$fig2 = fig2
  return(returns)
  
}