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
    
    
    
    output$boxplot <- renderPlot({
        data <- data()
        
        plot1 <- function(df, title){
            data = df %>% filter(Loewe.score >= -60 & Loewe.score <= 40)
            
            attach(data)
            grob1 = grobTree(textGrob(paste("Pearson Correlation : ", round(cor(Proba, Loewe.score), 4) ), 
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
        
        a = (input$d1 != "All")
        b = (input$d2 != "All")
        c = (input$cell != "All")
        if (!a & !b & c){ data <- data %>% filter(DepMap_ID == input$cell) 
            plot1(data, title=input$cell)}
        else if (a & !b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1) 
            plot1(data, title=paste0("Selected drug: ",input$d1))}
        else if (!a & b & !c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2) 
            plot1(data, title=paste0("Selected drug: ",input$d2))}
        else if (!a & b & c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2 , DepMap_ID == input$cell) 
            plot1(data, title=paste0("Selected combo: ", input$d2, " ",input$cell))}
        else if (a & !b & c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1 , DepMap_ID == input$cell) 
            plot1(data, title=paste0("Selected combo: ", input$d1, " ",input$cell))}
        else if (a & b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
            plot1(data, title=paste0("Selected combo: ", input$d1, " ",input$d2))}
        else if (a & b & c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2) %>% filter(DepMap_ID == input$cell)
            plot1(data, title=paste0("Selected combo: ", input$d1, " ",input$d2, " ",input$cell))}

    })
    
    output$boxplot2 <- renderPlot({
        data <- data()

        
        plot1 <- function(df, title=""){
        data = df %>% filter(Loewe.score >= -60 & Loewe.score <= 40)
        
        attach(data)
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
        
        a = (input$d1 != "All")
        b = (input$d2 != "All")
        c = (input$cell != "All")
        if (!a & !b & c){ data <- data %>% filter(DepMap_ID == input$cell) 
        plot1(data)}
        else if (a & !b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1) 
        plot1(data)}
        else if (!a & b & !c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2) 
        plot1(data)}
        else if (!a & b & c){ data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2 , DepMap_ID == input$cell) 
        plot1(data)}
        else if (a & !b & c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1 , DepMap_ID == input$cell) 
        plot1(data)}
        else if (a & b & !c){ data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)  %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
        plot1(data)}
        else if (a & b & c){ data <- data %>% filter(DepMap_ID == input$cell)
        plot1(data, title=input$cell)}
        
        
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
            radioButtons('model','Prediction model', choices = c('Deepsynergy (Preuer et al., 2018)','Multitaskdnn (Kim et al., 2021)',
                                                                 'Matchmaker (Brahim et al., 2021)','Deepdds (Wang et al., 2021)','TGSynergy from TGSA (Zhu et al., 2022)'),selected='Multitaskdnn (Kim et al., 2021)'),
            # sliderInput(
            #     "synscore", label = "Synergy score:",
            #     min = 0, value = 0, max = 30
            # ),
            # sliderInput(
            #     "proba", label = "Predicted proba:",
            #     min = 0, value = 0.5, max = 1
            # ),
            ),
        

        mainPanel(
            tabsetPanel(
                
                tabPanel("data table",
                         dataTableOutput("table"), 
                         
                         conditionalPanel(
                           condition = "input.dataset != 'Sanger 2022'",
                           fluidRow(
                               column(
                                   width = 6,plotOutput("boxplot")),
                               column(
                                   width = 6,plotOutput("boxplot2"))

                               )
                         ),
                         
                         plotOutput("heatImage") 
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