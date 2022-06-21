votingImage = function(df) {
  plot_list = list()
  
  if (input$d1 != "All" & input$d1 != "All") {
    for(i in 1:nrow(df)){
      df_temp = df[i,]
      actuals = df_temp$Actuals
      tr = data.frame(t(df_temp[, 9:ncol(df_temp)]))
      colnames(tr) <- "Proba"
      tr$votes <- ifelse((tr$Proba > 0.5) == actuals, 'true', 'false')
      z = ggplot(tr, aes(x = votes, colour=votes,fill=votes)) +geom_bar(stat = "count", position="dodge") +  
        scale_y_continuous(breaks=c(1,2,3,4,5,6)) + 
        scale_colour_manual(values=c("true"="green", "false" ="red")) +
        scale_fill_manual(values=c("true"="white", "false" ="white")) +
        labs(title=paste0("Count Votes of ",df_temp$DepMap_ID))
      plot_list[[i]] = z
  }
    }else{
    plot_list
  }
  plot_list
  
  
}