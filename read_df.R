rread = function(local_i) {
  

     file_path = paste0("data/proba_",local_i,"_",input$dataset,".csv")
      if(file_test("-f", file_path)){
          df = read.csv(paste0("data/proba_",local_i,"_",input$dataset,".csv"))[ ,-1]
      }else{
          df <- read.csv(paste0("data/proba_Deepsynergy (Preuer et al., 2018)_",input$dataset,".csv"))[ ,-1]
      }

  
}