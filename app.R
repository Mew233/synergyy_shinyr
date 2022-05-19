library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

server <- function(input, output,session){

    
    data <- reactive({
       file_path = paste0("data/proba_",input$model,"_",input$dataset,".csv")
        if(file_test("-f", file_path)){
            df = read.csv(paste0("data/proba_",input$model,"_",input$dataset,".csv"))[ ,-1]
        }else{
            df <- read.csv("data/proba_Deepsynergy (Preuer et al., 2018)_Sanger 2022.csv")[ ,-1]
        }
      return(df)
    })

    #https://cloud.tencent.com/developer/article/1830442
    # data <- reactive({
    #     #req(input$file1)
    #     df <- read.csv(paste0("data/proba_",input$model,"_",input$dataset,".csv"))[ ,-1]
    #     return(df)
    # })
    
    output$columns1 <- renderUI({
        selectInput("d1", label = "Drug1", choices = c('All',sort(unique(as.character(data()$Drug1)))), selectize=TRUE)
    })
    output$columns2 <- renderUI({
        selectInput("d2", label = "Drug2", choices = c('All',sort(unique(as.character(data()$Drug2)))), selectize=TRUE)
    })
    output$columns3 <- renderUI({
        selectInput("cell", label = "Cell line", choices = c('All',sort(unique(as.character(data()$DepMap_ID)))), selectize=TRUE)
    })
    
    #update d2
    observeEvent(input$d1, {
        if(input$d1 != "All"){
            chocies =  sort(unique(as.character(data()$Drug2[data()$Drug1==input$d1])))
            updateSelectInput(session, "d2", "Drug2", choices = c("All",chocies))
        } else{
            updateSelectInput(session, "d2", "Drug2", choices = c("All",sort(unique(as.character(data()$Drug2)))))
        }

    })
    
    #update cell
    observeEvent(
        c(input$d1,input$d2), {
            if(input$d1 != "All"){
                if(input$d2 != "All"){
                    #choices = sort(unique(as.character(data()$DepMap_ID[data()$Drug1==input$d1 & data()$Drug2==input$d2])))
                    temp = data() %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
                    choices = sort(unique(as.character(temp$DepMap_ID)))
                    if (length(choices)>0){
                        updateSelectInput(session, "cell", "Cell line", choices = c('All',choices))
                    }else{
                        updateSelectInput(session, "cell", "Cell line", choices = "")
                    }

                }else{
                    chocies =  sort(unique(as.character(data()$DepMap_ID[data()$Drug1==input$d1])))
                    updateSelectInput(session, "cell", "Cell line", choices = c("All",chocies))
                    }

            }else{
                if(input$d2 != "All"){
                    chocies =  sort(unique(as.character(data()$DepMap_ID[data()$Drug2==input$d2])))
                    updateSelectInput(session, "cell", "Cell line", choices = c("All",chocies))
                }else{
                    updateSelectInput(session, "cell", "Cell line", choices = c('All', sort(unique(as.character(data()$DepMap_ID)))))
                }

            }

        })

    
    output$table <- renderDataTable(DT::datatable({
            data <- data()
            a = (input$d1 != "All") 
            b = (input$d2 != "All")
            c = (input$cell != "All")
            if (a & b & c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2) %>% filter(DepMap_ID == input$cell) }
            if (!a & !b & c){ data <- data %>% filter(DepMap_ID == input$cell) }
            if (a & !b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1) }
            if (!a & b & !c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2) }
            if (!a & b & c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2 , DepMap_ID == input$cell) }
            if (a & !b & c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1 , DepMap_ID == input$cell) }
            if (a & b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2) }
            if (!a & !b & !c){data<- data}
            
            # if (input$d1 != "All") {
            #     data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)
            #     
            # }
            # if (input$d2 != "All") {
            #     data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
            # }
            # 
            # if (input$cell != "All") {
            #     data <- data %>% filter(DepMap_ID == input$cell)
            # }
            data
        }))


    output$heatImage <- renderPlot({
        data <-  data()
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
    
}


ui <- fluidPage(
    titlePanel('SynergyY Shiny R'),
    sidebarLayout(
        sidebarPanel(
    
            radioButtons('model','Prediction model', choices = c('LR','XGBOOST','RF','ERT','Deepsynergy (Preuer et al., 2018)','Multitaskdnn (Kim et al., 2021)',
                        'Matchmaker (Brahim et al., 2021)','Deepdds (Wang et al., 2021)','TGSynergy (Zhu et al., 2022)'),selected='Deepsynergy (Preuer et al., 2018)'),
            uiOutput('columns1'),
            uiOutput('columns2'),
            uiOutput('columns3'),
            radioButtons('dataset','Dataset', choices = c('DrugComb v1.5','Sanger 2022'), selected='DrugComb v1.5'),
            ),
        
        mainPanel(
            tabsetPanel(
                
                tabPanel("data table",
                         dataTableOutput("table"), plotOutput("heatImage") ),
                tabPanel("SHAP analysis",
                         fluidRow(
                             column(
                                 width = 6,plotOutput("shapImage1")),
                             column(
                                 width = 6,plotOutput("shapImage2")))
                )

            )
            
        )
    ),
    tags$footer(p("contact: cx229@cornell.edu, Elemento Lab"), align="left")
)

# Run the application 
shinyApp(ui = ui, server = server)