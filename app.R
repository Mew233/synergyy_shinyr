library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(ggbeeswarm)
library(grid)

server <- function(input, output,session){
    # save user input of dataset as a global variable
  dummydf <- reactive( {
      if(input$dataset == "DrugComb v1.5"){
        dummydf <- read.csv("data/proba_Deepsynergy (Preuer et al., 2018)_DrugComb v1.5.csv")[ ,-1]
      } else{
        dummydf <- read.csv("data/proba_Deepsynergy (Preuer et al., 2018)_Sanger 2022.csv")[ ,-1]
      }
    return(dummydf)
    })
      
    # data <- reactive({
    #    file_path = paste0("data/proba_",i,"_",input$dataset,".csv")
    #     if(file_test("-f", file_path)){
    #         df = read.csv(paste0("data/proba_",i,"_",input$dataset,".csv"))[ ,-1]
    #     }else{
    #         df <- read.csv("data/proba_Deepsynergy (Preuer et al., 2018)_Sanger 2022.csv")[ ,-1]
    #     }
    #   return(df)
    # })

    #https://cloud.tencent.com/developer/article/1830442
    # data <- reactive({
    #     #req(input$file1)
    #     df <- fread(paste0("data/proba_",input$model,"_",input$dataset,".csv"))[ ,-1]
    #     return(df)
    # })
    
    output$columns1 <- renderUI({
        selectInput("d1", label = "Drug1", choices = c('All',sort(unique(as.character(dummydf()$Drug1)))), selectize=TRUE)
    })
    output$columns2 <- renderUI({
        selectInput("d2", label = "Drug2", choices = c('All',sort(unique(as.character(dummydf()$Drug2)))), selectize=TRUE)
    })
    output$columns3 <- renderUI({
        selectInput("cell", label = "Cell line", choices = c('All',sort(unique(as.character(dummydf()$DepMap_ID)))), selectize=TRUE)
    })
    
    #update d2
    observeEvent(input$d1, {
        if(input$d1 != "All"){
            chocies =  sort(unique(as.character(dummydf()$Drug2[dummydf()$Drug1==input$d1])))
            updateSelectInput(session, "d2", "Drug2", choices = c("All",chocies))
        } else{
            updateSelectInput(session, "d2", "Drug2", choices = c("All",sort(unique(as.character(dummydf()$Drug2)))))
        }

    })
    
    #update cell
    observeEvent(
        c(input$d1,input$d2), {
            if(input$d1 != "All"){
                if(input$d2 != "All"){
                    #choices = sort(unique(as.character(data()$DepMap_ID[data()$Drug1==input$d1 & data()$Drug2==input$d2])))
                    temp = dummydf() %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
                    choices = sort(unique(as.character(temp$DepMap_ID)))
                    if (length(choices)>0){
                        updateSelectInput(session, "cell", "Cell line", choices = c('All',choices))
                    }else{
                        updateSelectInput(session, "cell", "Cell line", choices = "")
                    }

                }else{
                    chocies =  sort(unique(as.character(dummydf()$DepMap_ID[dummydf()$Drug1==input$d1])))
                    updateSelectInput(session, "cell", "Cell line", choices = c("All",chocies))
                    }

            }else{
                if(input$d2 != "All"){
                    chocies =  sort(unique(as.character(dummydf()$DepMap_ID[dummydf()$Drug2==input$d2])))
                    updateSelectInput(session, "cell", "Cell line", choices = c("All",chocies))
                }else{
                    updateSelectInput(session, "cell", "Cell line", choices = c('All', sort(unique(as.character(dummydf()$DepMap_ID)))))
                }

            }

        })

    # =============================table
    source("./read_df.R", local = TRUE)
    observe({
    output$table <- renderDataTable(DT::datatable({
     
      local({
      dummy = rread(local_i="dummy")[,1:8]

      for (i in input$checkbox) {

            local_i <- i
            #data <- rread(local_i=local_i)
            col <- rread(local_i=local_i) %>% select(Proba)
            varname = paste0("Proba_", local_i)
            dummy[[varname]] <- with(dummy, col)

      }

            a = (input$d1 != "All")
            b = (input$d2 != "All")
            c = (input$cell != "All")
            if (a & b & c){ data <- dummy %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2) %>% filter(DepMap_ID == input$cell) }
            if (!a & !b & c){ data <- dummy %>% filter(DepMap_ID == input$cell) }
            if (a & !b & !c){ data <- dummy %>% filter(Drug1 == input$d1 | Drug2 == input$d1) }
            if (!a & b & !c){ data <- dummy %>% filter(Drug1 == input$d2 | Drug2 == input$d2) }
            if (!a & b & c){ data <- dummy %>% filter(Drug1 == input$d2 | Drug2 == input$d2 , DepMap_ID == input$cell) }
            if (a & !b & c){ data <- dummy %>% filter(Drug1 == input$d1 | Drug2 == input$d1 , DepMap_ID == input$cell) }
            if (a & b & !c){ data <- dummy %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2) }
            if (!a & !b & !c){data<- dummy}

            data
        })
        
      })

        )
    })

    

    source("./boxplot.R", local = TRUE)
    source("./heatmap.R", local = TRUE)
    
    # =============================boxplot
    # Insert the right number of plot output objects into the web page
    output$plots <- renderUI({
      plot_output_list <- 
        lapply(input$checkbox,
               function(i){ 
                 plotOutput(paste0("plot", i))
               })
      
      do.call(tagList, plot_output_list)
    })
    
    # =============================boxplot2
    output$plots2 <- renderUI({
      plot_output_list2 <- 
        lapply(input$checkbox,
               function(i){ 
                 plotOutput(paste0("plot2", i))
               })
      
      do.call(tagList, plot_output_list2)
    })
    
    
    # =============================heatmap
    output$heatImages <- renderUI({
      plot_output_list3 <- 
        lapply(input$checkbox,
               function(i){ 
                 plotOutput(paste0("plot3", i))
               })
      
      do.call(tagList, plot_output_list3)
    })
    
    
    observe({
      for (i in input$checkbox) {
        local({
          local_i <- i
          df <- rread(local_i=local_i)
          
          output[[paste0("plot", local_i)]] <- 
            renderPlot({
              box_plot(df)$fig1 + 
                labs(caption=paste("plotted with", local_i, "option")) + 
                theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))
              
            })
          
          output[[paste0("plot2", local_i)]] <-
            renderPlot({
              box_plot(df)$fig2 + 
                labs(caption=paste("plotted with", local_i, "option")) + 
                theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

            })
          
          output[[paste0("plot3", local_i)]] <- 
            renderPlot({
              heatImage(df) + 
                labs(caption=paste("plotted with", local_i, "option")) + 
                theme(plot.caption = element_text(hjust=0.5, size=rel(1.2)))

            })
          
          
          
        })
      }
    })
    
    
    source("./shap_summary_plot.R", local = TRUE)
    output$shapImage1 <- renderPlot({
      shap_summary_plot()$fig1
      # summary plot
    })
    
    output$shapImage2 <- renderPlot({
      shap_summary_plot()$fig2
      # beeswarm plot
    })
    
    output$shapImage3 <- renderPlot({
      shap_summary_plot()$fig3
      # whole summary plot
    })
    
    output$shapImage4 <- renderPlot({
      shap_summary_plot()$fig4
      # whole beeswarm plot
    })
    
}


ui <- fluidPage(
    titlePanel('SynergyY Shiny R'),
    sidebarLayout(
        sidebarPanel(
            radioButtons('dataset','Dataset', choices = c('DrugComb v1.5','Sanger 2022'), selected='DrugComb v1.5'),
            uiOutput('columns1'),
            uiOutput('columns2'),
            uiOutput('columns3'),
            #'LR','XGBOOST','RF','ERT',
            # radioButtons('model','Prediction model', choices = c('Deepsynergy (Preuer et al., 2018)','Multitaskdnn (Kim et al., 2021)',
            #                                                      'Matchmaker (Brahim et al., 2021)','Deepdds (Wang et al., 2021)','TGSynergy from TGSA (Zhu et al., 2022)'),selected='Multitaskdnn (Kim et al., 2021)'),

              checkboxGroupInput("checkbox", "Prediction model",
                                 choices = c('Deepsynergy (Preuer et al., 2018)','Multitaskdnn (Kim et al., 2021)',
                                             'Matchmaker (Brahim et al., 2021)','Deepdds (Wang et al., 2021)','TGSynergy from TGSA (Zhu et al., 2022)'))
 
            ),
        

        mainPanel(
            tabsetPanel(
                
                tabPanel("data table",
                         dataTableOutput("table"), 
                         conditionalPanel(
                           condition = "input.dataset != 'Sanger 2022'",
                           fluidRow(
                               column(
                                   width = 6,uiOutput("plots")),
                               column(
                                   width = 6,uiOutput("plots2")),

                               )

                         ),
                         # 
                         conditionalPanel(
                           condition = "input.cell != 'All'",
                         uiOutput("heatImages") 
                         )
                         ),
                
                tabPanel("SHAP analysis",
                         fluidRow(h5('Top 30 gene expression features',style="display:inline-block")),
                         fluidRow(
                             column(
                                 width = 6,plotOutput("shapImage1")),
                             column(
                                 width = 6,plotOutput("shapImage2"))),
                         
                         fluidRow(h5('Top 30 features (from all features)',style="display:inline-block")),
                         
                         conditionalPanel(
                           condition = "input.d2 != 'All'",
                         fluidRow(
                           column(
                             width = 6,plotOutput("shapImage3")),
                           column(
                             width = 6,plotOutput("shapImage4")))
                         ),

                )

            )
            
        )
    ),
    tags$footer(p("contact: cx229@cornell.edu, Elemento Lab"), align="left")
)

# Run the application 
shinyApp(ui = ui, server = server)