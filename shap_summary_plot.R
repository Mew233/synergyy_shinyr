library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggbeeswarm)
# library(ggdist)


shap = as_tibble(read.csv("data/deep.csv"))
shap_exp = as_tibble(read.csv("data/deep_exp.csv"))

shap_summary_plot = function() {
  
  #summary bar plot
  
  pairs_exp = shap %>% select(c(2:1001)) 
  means = pairs_exp %>% abs() %>% colMeans() 
  
  df <- data.frame(x = names(means), y = means) %>% arrange(-y)
  
  #aes(reorder(x, y)
  fig1 = df %>% slice(1:30) %>% ggplot(., aes(y = fct_reorder(x, y), y)) + geom_bar(stat="identity") + xlab('mean(|Shap value|)(average impact on model output magnitude)') +ylab('Genes')
  
  
  #beeswarm plot
  ## df_new contains top 30 selected genes
  df_new = df %>% slice(1:30) 
  pairs_exp_selected = pairs_exp[,df_new$x]
  pairs_exp_long = pairs_exp_selected %>%
    pivot_longer(colnames(.),names_to =  "genes", values_to = "value")
  
  exp_selected = shap_exp[,df_new$x]
  zscore =  as.data.frame(scale(exp_selected))
  zscore_long = zscore %>%
    pivot_longer(colnames(.),names_to =  "genes", values_to = "value")
  
  pairs_exp_long$exp = zscore_long$value
  
  #https://cran.r-project.org/web/packages/ggdist/vignettes/dotsinterval.html
  # y = genes
  fig1order = rep(as.array(levels(fct_reorder(df_new$x, df_new$y))), each=dim(pairs_exp_selected)[1])
  #geom_dots()
  coolwarm_hcl <- colorspace::diverging_hcl(7, h = c(250, 10), c = 100, l = c(20, 95), power = c(1,1))
  fig2 = (pairs_exp_long %>% arrange(match(genes, fig1order)) %>% ggplot(aes(y =fct_inorder(genes), x = value,color=exp)) 
          + scale_color_gradientn(colours = coolwarm_hcl) 
          + guides(fill=guide_legend(title="exp value",label=FALSE)) 
          + geom_beeswarm(cex = 3) + xlab('Shap value(impact on model output)') +ylab('Genes'))
 
  
  returns = list(fig1=NULL, fig2=NULL)
  returns$fig1 = fig1
  returns$fig2 = fig2
  return(returns)
  
}