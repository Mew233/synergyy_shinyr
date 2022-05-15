library(shiny)
library(DT)
library(ggplot2)
library(dplyr)

df <- read.csv("./proba_sanger2022.csv")[ ,2:9]

server <- function(input, output,session){
    
    data <- df

    # Update as soon as Month gets populated according to the year and month selected
    observeEvent(
        input$d2, {
            if(input$d1 != "All"){
                if(input$d2 != "All"){
                    choices = unique(as.character(data$DepMap_ID[data$Drug1==input$d1 & data$Drug2==input$d2]))
                    if (length(choices)>0){
                        updateSelectInput(session, "cell", "Cell line", choices = c('All',choices))
                    }else{
                        updateSelectInput(session, "cell", "Cell line", choices = "")
                    }
                    
                }else{
                    chocies =  unique(as.character(data$DepMap_ID[data$Drug1==input$d1]))
                    updateSelectInput(session, "cell", "Cell line", choices = c("All",chocies))    
                    }

            }else{
                updateSelectInput(session, "cell", "Cell line", choices = c('All', unique(as.character(data$DepMap_ID))))
            }
            
            
        })
    
    output$table <- renderDataTable(DT::datatable({
        data <- df
        if (input$d1 != "All") {
            data <- data %>% filter(Drug1 == input$d1 | Drug2 == input$d1)
        }
        if (input$d2 != "All") {
            data <- data %>% filter(Drug1 == input$d2 | Drug2 == input$d2)
        }
        
        if (input$cell != "All") {
            data <- data[data$DepMap_ID == input$cell,]
        } 
        data
    }))
    
    output$heatImage <- renderPlot({
        data <-  df
        # Create auxiliary data.frame.
        data <- data[data$DepMap_ID == input$cell,]
        data$Var1 = factor(data$Drug1)
        data$Var2 = factor(data$Drug2)
        frames = data %>% filter(Drug1 == input$d1, Drug2 == input$d2)
        frames$Var1 = as.integer(frames$Var1)
        frames$Var2 = as.integer(frames$Var2)
        
        frames1 = data %>% filter(Drug1 == input$d2, Drug2 == input$d1)
        frames1$Var11 = as.integer(frames1$Var2)
        frames1$Var21 = as.integer(frames1$Var1)
        
        if (input$cell == "All") {
        }else{
            p = ggplot( data, aes(x=Drug1,y=Drug2))
            p +labs(title=input$cell) + theme(axis.text.x=element_text(angle=45,hjust=1)) + (
                scale_fill_gradient2(low = "#0072B2", high = "#D55E00",,midpoint=0.5)) +
                (geom_raster(aes(fill = Proba_deepsynergy_preuer))) +
                geom_rect(data=frames, size=1, fill=NA, colour="black",
                          aes(xmin=Var1 - 0.5, xmax=Var1 + 0.5, ymin=Var2 - 0.5, ymax=Var2 + 0.5))  +
                geom_rect(data=frames1, size=1, fill=NA, colour="black",
                          aes(xmin=Var21 - 0.5, xmax=Var21 + 0.5, ymin=Var11 - 0.5, ymax=Var11 + 0.5))
        }
        
    })
    
}


ui <- fluidPage(
    titlePanel('SynergyY Shiny R'),
    sidebarLayout(
        sidebarPanel(
            selectInput("d1", "Drug1", choices = c('All', unique(as.character(df$Drug1)))),
            selectInput("d2", "Drug2", choices = c('All', unique(as.character(df$Drug2)))),
            selectInput("cell", "Cell line", choices = c('All', unique(as.character(df$DepMap_ID))))),

        
        mainPanel(
            tabsetPanel(
                tabPanel("data table",
                         dataTableOutput("table"), plotOutput("heatImage") ),
                tabPanel("SHAP analysis",
                         
                )

            )
            
        )
    ),
    tags$footer(p("contact: cx229@cornell.edu, Elemento Lab"), align="left")
)

# Run the application 
shinyApp(ui = ui, server = server)