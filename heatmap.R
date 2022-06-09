heatImage = function(data) {
  
  
  data <- data
  
  # Create auxiliary data.frame.
  data <- data[data$DepMap_ID == input$cell,]
  data$Var1 = factor(data$Drug1)
  data$Var2 = factor(data$Drug2)
  
  frames = data %>% filter(Drug1 == input$d1, Drug2 == input$d2)
  frames$Var1 = as.integer(frames$Var1)
  frames$Var2 = as.integer(frames$Var2)
  frames$Color <- ifelse((frames$Proba > 0.5) == frames$Actuals, 'true', 'false')
  frames$Color <- factor(frames$Color, levels=c('true', 'false'))
  #https://stackoverflow.com/questions/66891122/colour-geom-rect-under-certain-condition
  
  frames1 = data %>% filter(Drug1 == input$d2, Drug2 == input$d1)
  frames1$Var11 = as.integer(frames1$Var2)
  frames1$Var21 = as.integer(frames1$Var1)
  frames1$Color <- ifelse((frames1$Proba > 0.5) == frames1$Actuals, 'true', 'false')
  frames1$Color <- factor(frames1$Color, levels=c('true', 'false'))
  
  if (input$cell == "All") {
  }else{
    options(repr.plot.width = 12, repr.plot.height = 12)
    p = ggplot(data, aes(x=Drug1,y=Drug2))
    p + labs(title=input$cell) + theme(axis.text.x=element_text(size=10,angle=45,hjust=1), axis.text.y=element_text(size=6)) +scale_y_discrete(labels = abbreviate) +
      (scale_fill_gradient2(low = "#0072B2", high = "#D55E00",midpoint=0.5)) + (geom_raster(aes(fill = Proba))) +
      
      geom_rect(data=frames, size=1, fill=NA,
                aes(xmin=Var1 - 0.5, xmax=Var1 + 0.5, ymin=Var2 - 0.5, ymax=Var2 + 0.5,colour=Color)) +
      
      geom_rect(data=frames1, size=1, fill=NA,
                aes(xmin=Var21 - 0.5, xmax=Var21 + 0.5, ymin=Var11 - 0.5, ymax=Var11 + 0.5, colour=Color))+
      
      # Fake discrete axis
      scale_x_discrete() +
      scale_colour_manual(name='correct prediction', values=c('true'='green', 'false'='red')) 
    
  }
    

  
  
  
  
  
  
  
  
  }