preprocess = function(local_i){

  df = read.csv(paste0("data/proba_",local_i,"_","DrugComb v1.5",".csv"))[ ,-1]

  # # sort column in row-wise manner
  sort_rows <- function(x, nullFrame=NULL){
    #nullFrame = as.data.frame(x[1,])
    for(i in 1:nrow(x)){
      d1ID = x[i,3]
      d2ID = x[i,4]
      row = as.data.frame(x[i,])
      judge = table(x$Drug1_ID)[d1ID]
      judege2 = table(x$Drug2_ID)[d2ID]
      if((judge >= judege2)) {
        #print(row)
        col_names = c("Drug2", "Drug1", "Drug2_ID", "Drug1_ID","DepMap_ID","Tissue","Loewe.score","Actuals")
        extra_names <- setdiff(colnames(row), col_names)
        temp2 = row %>% select(all_of(extra_names))
        row = row[c("Drug2", "Drug1", "Drug2_ID", "Drug1_ID","DepMap_ID","Tissue","Loewe.score","Actuals")]
        row = bind_cols(row,temp2)}
      else{
          row = row
        }
      
      
      
      nullFrame <- rbindlist(list(nullFrame,row),use.names=FALSE)
    }
    return(nullFrame)
  }
  dat_split = split(df, df$Drug1_ID)
  results = lapply(dat_split, sort_rows)
  
  write.csv(do.call(rbind, results),paste0("data/proba_",local_i,"_","DrugComb v1.5_processed",".csv"))
}
# preprocess = function(local_i){
#   
#   df = read.csv(paste0("data/proba_",local_i,"_","DrugComb v1.5",".csv"))[ ,-1]
#   
#   #map ID to name
#   long <- melt(df, id.vars = c("Drug1", "Drug1_ID")) %>% rename(., Drug=Drug1,Drug_ID=Drug1_ID)
#   long2 <- melt(df, id.vars = c("Drug2", "Drug2_ID")) %>% rename(., Drug=Drug2,Drug_ID=Drug2_ID)
#   longs <- rbind(long,long2)
#   longs = longs[!duplicated(longs$Drug_ID), ]
# 
#   #interchangebale column assigned to a group
#   df = df %>%
#     mutate(group = as.integer(factor(map2_chr(
#       Drug1_ID, Drug2_ID, ~paste(sort(c(.x, .y)), collapse = "_")))))
# 
#   # # sort column in row-wise manner
#   # sort_rows <- function(df, col_names, dec=F){
#   # 
#   #   temp <- df %>%
#   #     select(all_of(col_names))
#   # 
#   #   extra_names <- setdiff(colnames(df), col_names)
#   # 
#   #   temp2 <- df %>%
#   #     select(all_of(extra_names))
#   # 
#   #   set_colnames <- `colnames<-`
#   #   #t(apply(df[3:4], 1, sort, decreasing = FALSE))
#   #   res <- t(apply(temp, 1, sort, decreasing=dec)) %>%
#   #     as_tibble %>%
#   #     set_colnames(col_names) %>%
#   #     bind_cols(temp2)
#   # 
#   #   return(res)
#   # 
#   # }
#   # # sort column in row-wise manner
#   sort_rows <- function(x, nullFrame, df_firstoccur){
#     nullFrame = as.data.frame(x[1,])
#     for(i in 2:nrow(x)){
#       d1ID = x[i,3]
#       d2ID = x[i,4]
#       row = as.data.frame(x[i,])
#       judge = table(df_firstoccur$Drug2_ID)[d1ID]
#       judege2 = table(df_firstoccur$Drug2_ID)[d2ID]
#       if((judge >= judege2)) {
#         #print(row)
#         row = row[c("Drug2", "Drug1", "Drug2_ID", "Drug1_ID","DepMap_ID","Tissue","Loewe.score","Actuals","Proba")]}
#       
#       nullFrame <- rbindlist(list(nullFrame,row),use.names=FALSE)
#     }
#     return(nullFrame)
#   }
#   
#   col_names <- c("Drug1_ID","Drug2_ID")
#   df_firstoccur = df[!duplicated(df$group), ]
#   #df_firstoccur_d1id = df_firstoccur[!duplicated(df_firstoccur$Drug1_ID), ] 
#   #df_firstoccur_d1id = df_firstoccur_d1id %>% sort_rows(., NULL, df_firstoccur_d1id)
#   
#   df_firstoccur = df_firstoccur %>% sort_rows(., NULL, df_firstoccur)
#   df_firstoccur$group = NULL
#   
#   reorderRows <- function(x, nullFrame, df_firstoccur){
#     for(i in 1:nrow(x)){
#       d1ID = x[i,3]
#       d2ID = x[i,4]
#       row = as.data.frame(x[i,])
#       judge = df_firstoccur %>% filter(Drug1_ID==d1ID)  %>% filter(Drug2_ID==d2ID) %>% dim()
#       if(judge[1] <= 0) {
#         print(row)
#         row = row[c("Drug2", "Drug1", "Drug2_ID", "Drug1_ID","DepMap_ID","Tissue","Loewe.score","Actuals","Proba","group")]}
#       
#       nullFrame <- rbindlist(list(nullFrame,row),use.names=FALSE)
#     }
#     return(nullFrame)
#   }
#   
#   df_switch = df[duplicated(df$group), ]  %>% reorderRows(., nullFrame = NULL,df_firstoccur = df_firstoccur)
#   df_switch$group = NULL
#   dfbind = rbind(df_firstoccur, df_switch,use.names=FALSE)
# 
#   a = right_join(longs,dfbind,by = c("Drug_ID" = "Drug1_ID"))[ , c("Drug", "Drug_ID", "Drug2_ID","DepMap_ID","Tissue","Loewe.score","Actuals","Proba")] %>%
#     rename(., Drug1=Drug,Drug1_ID=Drug_ID)
# 
#   b = right_join(longs, a, by = c("Drug_ID" = "Drug2_ID"))[ , c("Drug1", "Drug1_ID", "Drug","Drug_ID","DepMap_ID","Tissue","Loewe.score","Actuals","Proba")] %>%
#     rename(., Drug2=Drug,Drug2_ID=Drug_ID)
#   df = b[ , c("Drug1","Drug2","Drug1_ID","Drug2_ID","DepMap_ID","Tissue","Loewe.score","Actuals","Proba")]
#   
#   #write.csv(df,paste0("data/proba_",local_i,"_","DrugComb v1.5_processed",".csv"))
#   df
#   
# }
