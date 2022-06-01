library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggbeeswarm)
library(data.table)
# library(ggdist)


shap_summary_plot = function() {
  
  # shap = as_tibble(read.csv("data/multitask_shap.csv"))
  # shap_exp = as_tibble(read.csv("data/multitask_exp.csv"))
  
  shap = as_tibble(fread("data/multitask_shap.csv"))
  shap_exp = as_tibble(fread("data/multitask_exp.csv"))
  
  # filter by input drug1/drug2
  shap1 <- reactive( {
    if(input$model == "Multitaskdnn (Kim et al., 2021)"){
      if(input$d2 != "All"){
        if(input$cell == "All"){
          temp = shap %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2)}
        else{
          temp = shap %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2) %>% filter(DepMap_ID == input$cell)
      }}
      else{
      temp = shap %>% filter(Drug1 == input$d1 | Drug2 == input$d1)
      }
    }
    return(temp)
  })
  
  shap_exp1 <- reactive( {
    #temp = shap_exp %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
    #这里exp也需要像exp一样merge
    temp = shap_exp %>% filter(row_number() %in% rownames(shap1()))
    return(temp)
  })
  
  #summary bar plot
  withProgress(message = 'Making plot', value=0,{
    
  pairs_exp = shap1() %>% select(c(6178:(6178+1000-1)))
  #pairs_exp = shap1() %>% select(c(2:6177)) 
  means = pairs_exp %>% abs() %>% colMeans() 
  
  df <- data.frame(x = names(means), y = means) %>% arrange(-y)
  
  #aes(reorder(x, y)
  fig1 = df %>% slice(1:30) %>% ggplot(., aes(y = fct_reorder(x, y), y)) + geom_bar(stat="identity",fill = "#C0C0C0") + xlab('mean(|Shap value|)(average impact on model output magnitude)') +ylab('Genes')
  incProgress(1/4)
  
  #beeswarm plot
  ## df_new contains top 30 selected genes
  df_new = df %>% slice(1:30) 
  pairs_exp_selected = pairs_exp[,df_new$x]
  pairs_exp_long = pairs_exp_selected %>%
    pivot_longer(colnames(.),names_to =  "genes", values_to = "value")
  
  exp_selected = shap_exp1()[,df_new$x]
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
  incProgress(1/4)
  
  # 全局
  pairs_exp = shap1() %>% select(c(2:(6178+1000-1))) 
  means = pairs_exp %>% abs() %>% colMeans() 
  
  df <- data.frame(x = names(means), y = means) %>% arrange(-y)
  
  #aes(reorder(x, y)
  fig3 = df %>% slice(1:30) %>% ggplot(., aes(y = fct_reorder(x, y), y)) + geom_bar(stat="identity",fill = "#C0C0C0") + xlab('mean(|Shap value|)(average impact on model output magnitude)') +ylab('Features')
  incProgress(1/4)
  
  #全局 beeswarm plot
  ## df_new contains top 30 selected genes
  df_new = df %>% slice(1:30) 
  pairs_exp_selected = pairs_exp[,df_new$x]
  pairs_exp_long = pairs_exp_selected %>%
    pivot_longer(colnames(.),names_to =  "vars", values_to = "value")
  
  exp_selected = shap_exp1()[,df_new$x]
  # zscore =  as.data.frame(scale(exp_selected))
  # zscore_long = zscore %>%
  #   pivot_longer(colnames(.),names_to =  "genes", values_to = "value")
  zscore_long =  as.data.frame(exp_selected) %>%
      pivot_longer(colnames(.),names_to =  "vars", values_to = "value")
  pairs_exp_long$feature_value = zscore_long$value
  

  fig1order = rep(as.array(levels(fct_reorder(df_new$x, df_new$y))), each=dim(pairs_exp_selected)[1])
  #geom_dots()
  coolwarm_hcl <- colorspace::diverging_hcl(7, h = c(250, 10), c = 100, l = c(20, 95), power = c(1,1))
  fig4 = (pairs_exp_long %>% arrange(match(vars, fig1order)) %>% ggplot(aes(y =fct_inorder(vars), x = value,color=feature_value)) 
          + scale_color_gradientn(colours = coolwarm_hcl) 
          #+ guides(fill=guide_legend(title="feature value",label=FALSE)) 
          + geom_beeswarm(cex = 3) + xlab('Shap value(impact on model output)') +ylab('Features'))
  incProgress(1/4)
  
    
  })
  
  returns = list(fig1=NULL, fig2=NULL,fig3=NULL, fig4=NULL)
  returns$fig1 = fig1
  returns$fig2 = fig2
  returns$fig3 = fig3
  returns$fig4 = fig4
  
  return(returns)
  
}