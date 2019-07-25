# Define UI ----
ui = fluidPage(
     theme="styles/custom.css",
     
     navbarPage(
         "AI PROJECT",
         id="menuBarValue",
         inverse=TRUE,
         collapsible=TRUE, 	
        fluid=TRUE,
        
            tabPanel(
                 "Graph"                                                           
             ) ,
            tabPanel(
                 "Data Set"                                              
             ),
            tabPanel(
                 "Prediction"                                 
             )
          
     ),
    fluidRow(
        column(
        width=12,
        align="center",
        div(
            class="menuBar",
            tabsetPanel(
            type="pills",
            id = "tabSet",                        
            tabPanel(        
                "1 Hidden Nodes",
                value = "tab1"
            ),
            tabPanel(
                "5 Hidden Nodes",
                value = "tab2"
            ),
            tabPanel(        
                "10 Hidden Nodes",
                value = "tab3"
            ),
            tabPanel(        
                "20 Hidden Nodes",
                value = "tab4"
            )
            )
        )                                                        
        )
    ),
    htmlOutput("mainOutput"),
    
    fluidRow(
        column(
            width=12,
            textOutput("debug")
        )
    )            

    
    
    
)