# Define UI ----
ui = fluidPage(
    titlePanel(
        
        h1("AI Project",align="center"),
        windowTitle = "AI Project"
        
    ),
    
        mainPanel(
            width=12,
            style="background-color:white; border-radius:20px;",
            tabsetPanel(
                type="pills",
                id = "tabSet",            
                tabPanel(        
                    "1 Hidden Nodes",
                    value = "tab1"
                ),
                tabPanel(
                    "2 Hidden Nodes",
                    value = "tab2"
                ),
                tabPanel(        
                    "3 Hidden Nodes",
                    value = "tab3"
                ),
                tabPanel(        
                    "4 Hidden Nodes",
                    value = "tab4"
                )
    ),
            fluidRow(
                column(
                    width=6,
                    h4(
                        align='center',
                        htmlOutput("accuracyRate")
                    )                
                ),
                column(
                    width=6,
                    h4(
                    htmlOutput("errorRate"),
                    align='center')
                )
            ),
            fluidRow(
                column(
                    width=2,
                    checkboxGroupInput("keyCol", "",
                            c(
                            "Strength" = "strength",
                            "Predicted Strength" = "predictedStr"
                            )                                                            
                        )
                ),
                column(
                    width=8,
                    plotOutput("myGraph",click="clickEvent")
                ),
                column(
                    width=2,
                    checkboxGroupInput("shownCol", "Columns to show",
                            c(
                            "Cement" = "cement",
                            "Slag" = "slag",
                            "Ash" = "ash",
                            "Water" = "water",
                            "Superplastic" = "superPlastic",
                            "Coarseagg" = "coarseagg",
                            "Fineagg" = "fineagg",
                            "Age" = "age"
                            )                                             
                        )
                )
            ),
            fluidRow(
                column(
                    width=12,
                    sliderInput(
                    "xControl",
                    "Row Amount",
                    min=1,
                    max=250,
                    value=c(1,250),
                    step=1,
                    width="50%"                    
                    ),
                    align="center"
                )
            ),
            fluidRow(
                column(
                    width=12,

                    h3(
                        htmlOutput(
                            "summaryText"
                        )
                    ),
                    align="center"
                )
            )
            # h3(textOutput("debug"))
        
        )
    
)