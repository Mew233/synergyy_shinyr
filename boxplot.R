box_plot = function(df) {
  
    plot1 <- function(df, title){
      data = df %>% filter(Loewe.score >= -60 & Loewe.score <= 40)
      
      #attach(data)
      grob1 = grobTree(textGrob(paste("Pearson Correlation : ", round(cor(data$Proba, data$Loewe.score), 4) ), 
                                x = 0.03, y = 0.97, hjust = 0, gp = gpar(col = "red", fontsize = 11, fontface = "bold")))
      
      
      coolwarm_hcl <- colorspace::diverging_hcl(7, h = c(250, 10), c = 100, l = c(35, 95), power = c(0.7,1.3))
      p = ggplot(data, aes(y =Proba, x = Loewe.score,color=Proba))
      p + guides(fill=guide_legend(title="Actual Loewe score",label=FALSE)) +
        scale_colour_gradient2(limits=c(0, 1),midpoint = 0.5)  +
        geom_point() + annotation_custom(grob1) +
        #scale_color_gradientn(colours = coolwarm_hcl, limits=c(-50, 50),midpoint = 3)  +
        # geom_point(aes(size = (Proba*10)^2)) +
        # scale_size(name   = "predicted proba",
        #            breaks = fivenum(data$Proba*10)^2,
        #            labels = c("","","","","")) +
        xlim(-60, 40) + ylim(0, 1) + xlab('Actual score') +ylab('Predicted Proba') + labs(title=title) + 
        geom_vline(xintercept=0) + geom_hline(yintercept=0.5)
    }
    
    plot2 <- function(df, title=""){
      data = df %>% filter(Loewe.score >= -60 & Loewe.score <= 40)
      
      #attach(data)
      z_loewe = scale(data$Loewe.score)
      z_proba = scale(data$Proba)
      
      #lambda = round(median(z_proba^2, na.rm = TRUE) /  median(z_loewe^2, na.rm = TRUE) , 4)
      "λ = "
      ks = ks.test(z_loewe, z_proba) 
      grob1 = grobTree(textGrob(paste("D = ", round(ks$statistic,4), ", p = ", round(ks$p.value,4)), 
                                x = 0.03, y = 0.97, hjust = 0, gp = gpar(col = "red", fontsize = 11, fontface = "bold")))
      
      quantiles = seq(0, 1, 0.01)
      ggplot(mapping = aes(x = quantile(data$Loewe.score, quantiles), y = quantile(data$Proba, quantiles))) + 
        geom_point(size = 0.5) + 
        xlab('Actual score quantile') +ylab('Predicted proba quantile') + labs(title=paste0("Emperical qq plot: ",title)) +
        #geom_abline(aes(slope = 1, intercept = 0), linetype = 2) +
        xlim(-60, 40) + ylim(0, 1) + annotation_custom(grob1) + geom_abline(intercept = 0.5, slope = 0.5/40, linetype = "dashed", size = 0.25)
      
    }
    
    data <- df
    
    a = (input$d1 != "All")
    b = (input$d2 != "All")
    c = (input$cell != "All")
    returns = list(fig1=NULL, fig2=NULL)
    
    if (!a & !b & c){ data <- data %>% filter(DepMap_ID == input$cell)
    fig1 = plot1(data, title=input$cell)
    fig2 =plot2(data)
    returns$fig1 = fig1
    returns$fig2 = fig2}
    else if (a & !b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)
    fig1 = plot1(data, title=paste0("Selected drug: ",input$d1))
    fig2 =plot2(data)
    returns$fig1 = fig1
    returns$fig2 = fig2}
    else if (!a & b & !c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
    fig1 = plot1(data, title=paste0("Selected drug: ",input$d2))
    fig2 =plot2(data)
    returns$fig1 = fig1
    returns$fig2 = fig2}
    else if (!a & b & c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2 , DepMap_ID == input$cell)
    fig1 = plot1(data, title=paste0("Selected combo: ", input$d2, " ",input$cell))
    fig2 =plot2(data)
    returns$fig1 = fig1
    returns$fig2 = fig2}
    else if (a & !b & c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1 , DepMap_ID == input$cell)
    fig1 = plot1(data, title=paste0("Selected combo: ", input$d1, " ",input$cell))
    fig2 =plot2(data)
    returns$fig1 = fig1
    returns$fig2 = fig2}
    else if (a & b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
    fig1 = plot1(data, title=paste0("Selected combo: ", input$d1, " ",input$d2))
    fig2 =plot2(data)
    returns$fig1 = fig1
    returns$fig2 = fig2}
    else if (a & b & c){ 
    data1 <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2) %>% filter(DepMap_ID == input$cell)
    fig1 = plot1(data1, title=paste0("Selected combo: ", input$d1, " ",input$d2, " ",input$cell))
    data2 = data %>% filter(DepMap_ID == input$cell)
    fig2 =plot2(data2)
    returns$fig1 = fig1
    returns$fig2 = fig2}

    
    return(returns)

}