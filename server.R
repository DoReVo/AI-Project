source("helperScripts/models.R")
library(ggplot2)
library(DT)

# GENERATE GRAPH LIST
frameList = generatePrediction()

# EXTRACT DATA FRAMES FROM LIST
graphDataFrame1 = data.frame(frameList[1])
write.csv(graphDataFrame1,"1HN.csv")
graphDataFrame2 = data.frame(frameList[2])
graphDataFrame3 = data.frame(frameList[3])
graphDataFrame4 = data.frame(frameList[4])


renderedDataFrame = graphDataFrame1
graphAccuracy = c()
graphErrorRate = c()
summaryText = NULL

 masterGraph = function(x,shownCol,keyCol,renderedDataFrame){        

    #  Graph Object
    graph = ggplot(renderedDataFrame,aes(x=1:nrow(renderedDataFrame),y=strength)) +    
    labs(x="Row",y="Actual Strength Vs Predicted Strength") +            
    theme(legend.position="none") +
    xlim(x[1],x[2])

    # Control key columns to be rendered on graphs // Strength & Predicted Strength
    if(!is.null(keyCol)){
        for(x in keyCol){
            switch(x,
            "strength"={
                graph = graph + geom_line() +  geom_point()                
            },
            "predictedStr" = {
                graph = graph + geom_line(aes(y=PredictedStrength,col="#5fb56c")) +  geom_point(aes(y=PredictedStrength),col="#5fb56c")
            })
        }
    }

    # Control optional columns to be rendered on graphs // Columns in csv file
    if(!is.null(shownCol)){        
        for(x in shownCol){
            switch(x,
            "cement"={
                graph = graph + geom_line(aes(y=cement,col="#4b6d97")) +  geom_point(aes(y=cement),col="#4b6d97")                
            },
            "slag"={
                graph = graph + geom_line(aes(y=slag,col="#71a4a1")) +  geom_point(aes(y=slag),col="#71a4a1")                
            },
            "ash"={
                graph = graph + geom_line(aes(y=ash,col="#d0dde0")) +  geom_point(aes(y=ash),col="#d0dde0")                
            },
            "water"={
                graph = graph + geom_line(aes(y=water,col="#867a90")) +  geom_point(aes(y=water),col="#867a90")                
            },
            "superPlastic"={
                graph = graph + geom_line(aes(y=superplastic,col="#b762dd")) +  geom_point(aes(y=superplastic),col="#b762dd")                
            },
            "coarseagg"={
                graph = graph + geom_line(aes(y=coarseagg,col="#5137a5")) +  geom_point(aes(y=coarseagg),col="#5137a5")                
            },
            "fineagg"={
                graph = graph + geom_line(aes(y=fineagg,col="#54aadf")) +  geom_point(aes(y=fineagg),col="#54aadf")                
            },
            "age"={
                graph = graph + geom_line(aes(y=age,col="black")) +  geom_point(aes(y=age),col="black")                
            }
            )
        }
    }

    return (graph)            
}


server = function(input, output) {
        
    # Observe which tab is currently opened and control rendred graphs dataframe
    observeEvent(input$tabSet,
        {
            # Assign data frame to be rendered on graphs
            switch(input$tabSet,
                "tab1" = {
                    renderedDataFrame = graphDataFrame1                                                    
                },
                "tab2" = {
                    renderedDataFrame = graphDataFrame2                    
                },
                "tab3" = {
                    renderedDataFrame = graphDataFrame3                    
                },
                "tab4" = {
                    renderedDataFrame = graphDataFrame4                    
                }
            )
            # Set graph accuracy text
            graphAccuracy = format(cor(renderedDataFrame$PredictedStrength,concrete_test$strength),digits=4)
            # Set graph error rate text
            graphErrorRate = format(1-cor(renderedDataFrame$PredictedStrength,concrete_test$strength),digits=4)

            # summaryText = toString(input$shownCol)
            # Ui output for accuracy
            output$accuracyRate = renderText(
                    {
                        paste("Accuracy Rate: <mark>" , graphAccuracy,"</mark>")
                        
                    }
            )
            # Ui output for error rate
            output$errorRate = renderText(
                {
                    paste("Error Rate: <mark>", graphErrorRate,"</mark>")
                }        
            )
 
            output$dataTable = DT::renderDataTable(
                format(renderedDataFrame,digits=4),
                # renderedDataFrame,
                class="table-sm table-condensed table-bordered customTable",
                style = "bootstrap4",
                options = list(
                    columnDefs = list(
                        list(title="Row",visible=TRUE,targets=0),
                        list(targets=c(1:10),className=""),                        
                        list(title="Cement",targets=1),
                        list(title="Slag",targets=2),
                        list(title="Ash",targets=3),
                        list(title="Water",targets=4),
                        list(title="Super Plastic",targets=5),
                        list(title="Courseagg",targets=6),
                        list(title="Fineagg",targets=7),
                        list(title="Age",targets=8),
                        list(title="Strength",targets=9),                        
                        list(title="Predicted Strength",targets=10)
                    )
                                                           
                )            
                
                
            )          
            
            
            # Render the graph
            output$myGraph = renderPlot(
                {
                    masterGraph(input$xControl,input$shownCol,input$keyCol,renderedDataFrame)    
                }                
            )
        }
    )

     observeEvent(input$tabSet2,
        {
            # Assign data frame to be rendered on graphs
            switch(input$tabSet2,
                "tab1" = {
                    renderedDataFrame = graphDataFrame1                                                    
                },
                "tab2" = {
                    renderedDataFrame = graphDataFrame2                    
                },
                "tab3" = {
                    renderedDataFrame = graphDataFrame3                    
                },
                "tab4" = {
                    renderedDataFrame = graphDataFrame4                    
                }
            )
            # Set graph accuracy text
            graphAccuracy = format(cor(renderedDataFrame$PredictedStrength,concrete_test$strength),digits=4)
            # Set graph error rate text
            graphErrorRate = format(1-cor(renderedDataFrame$PredictedStrength,concrete_test$strength),digits=4)

            # summaryText = toString(input$shownCol)
            # Ui output for accuracy
            output$accuracyRate = renderUI(
                    {
                        h6(
                            "Accuracy Rate:",
                            tags$mark(graphAccuracy),
                            class="TESTCLASS"
                        )
                        # paste("Accuracy Rate: <mark>" , graphAccuracy,"</mark>")
                        
                    }
            )
            # Ui output for error rate
            output$errorRate = renderText(
                {
                    paste("Error Rate: <mark>", graphErrorRate,"</mark>")
                }        
            )

            output$dataTable = DT::renderDataTable(
                renderedDataFrame,
                class="table-sm table-condensed table-bordered table-hover table-dark customTable",
                style = "bootstrap4",
                options = list(
                    columnDefs = list(
                        list(visible=FALSE,targets=0),
                        list(targets=c(1:10),className="mx-auto"),                        
                        list(title="Cement",targets=1),
                        list(title="Slag",targets=2),
                        list(title="Ash",targets=3),
                        list(title="Water",targets=4),
                        list(title="Super Plastic",targets=5),
                        list(title="Courseagg",targets=6),
                        list(title="Fineagg",targets=7),
                        list(title="Age",targets=8),
                        list(title="Strength",targets=9),                        
                        list(title="Predicted Strength",targets=10)
                    )
                                                           
                )            
                
                
            )          

            # Render summary text
            # output$summaryText = renderText(
                # paste("hello ",summaryText,"<br>")
                # paste(typeof(input$shownCol))
                # paste(input$shownCol,input$keyCol)
                
                # summaryText
                # paste("Graph columns",input$shownCol)
                # paste(input$shownCol)
            # )

            # Render the graph
            output$myGraph = renderPlot(
                {
                    masterGraph(input$xControl,input$shownCol,input$keyCol,renderedDataFrame)    
                }                
            )
        }
    )

    output$mainOutput = renderUI (
        {
            switch(input$menuBarValue,
                "Graph" = {

                    div(
                       
                        fluidRow(
                            column(
                                width=6,
                                h3(
                                    align='center',
                                    htmlOutput("accuracyRate")
                                )                
                            ),
                            column(
                                width=6,
                                h3(
                                htmlOutput("errorRate"),
                                align='center')
                            )
                        ),
                        fluidRow(
                            class="graphRow",
                            column(
                                width=2,
                                class="keyColGroup",
                                checkboxGroupInput("keyCol", "Strength",
                                        c(
                                        "Actual" = "strength",
                                        "Predicted" = "predictedStr"
                                        ),
                                        selected = c('strength','predictedStr')                                                            
                                    )
                            ),
                            column(
                                width=8,
                                plotOutput("myGraph",click="clickEvent")
                            ),
                            column(
                                width=2,
                                class="shownColGroup",
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
                        )
                    )

                },
                "Normalized Data" = {
                    div(                        
                        fluidRow(
                        column(
                            width=12,
                            dataTableOutput("dataTable"),
                            class="dataTableGroup"
                        )
                        )
                    )
                },
                "Raw Data" = {

                    div(                        
                        fluidRow(
                        column(
                            width=12,
                            dataTableOutput("dataTableRaw"),
                            class="dataTableGroup"
                        )
                        )
                    )
                    
                },
                "Prediction"={
                    div(
                        flowLayout(
                            class="textBoxGroup",
                            textInput(
                                "cementValue",
                                "Cement"
                            ),
                            textInput(
                                "slagValue",
                                "Slag"
                            ),
                            textInput(
                                "ashValue",
                                "Ash"
                            ),
                            textInput(
                                "waterValue",
                                "Water"
                            ),
                            textInput(
                                "superPlasticValue",
                                "Super Plastic"
                            ),
                            textInput(
                                "coarseaggValue",
                                "Coarseagg"
                            ),
                            textInput(
                                "fineaggValue",
                                "FineAgg"
                            ),
                            textInput(
                                "ageValue",
                                "Age"
                            )
                                                                                                                                                                                                                               
                        ),
                        fluidRow(
                            column(
                                width=12,
                                align="center",
                             textInput(
                                "strengthValue",
                                "Strength"
                            ) 
                            )
                        ),
                        fluidRow(
                            column(
                                width=12,
                                align="center",
                                class="buttonGroup",
                            actionButton("predictButton","PREDICT")
                            )
                            
                        ),
                        htmlOutput("predictedValueOutput")
                        
                        
                    )
                },
               

            )
        }
    )

        raw = data.frame(read.csv("rawData/concrete.csv"))
        # print(raw)
    output$dataTableRaw = DT::renderDataTable(
        raw,
        
        class="table-sm table-condensed table-bordered customTable",
        style = "bootstrap4",
        options = list(
            columnDefs = list(
                list(title="Row",targets=0),
                # list(targets=c(1:10),className=""),                        
                list(title="Cement",targets=1),
                list(title="Slag",targets=2),
                list(title="Ash",targets=3),
                list(title="Water",targets=4),
                list(title="Super Plastic",targets=5),
                list(title="Courseagg",targets=6),
                list(title="Fineagg",targets=7),
                list(title="Age",targets=8),
                list(title="Strength",targets=9)                        
                
            )
                                                    
        )            
        
        
    ) 

                


    observeEvent(
        input$predictButton,
        {
            switch(input$tabSet,
                "tab1" = {
                    modelUsed = readRDS("models/1HN.rda")
                },
                "tab2" = {
                    modelUsed = readRDS("models/5HN.rda")
                },
                "tab3" = {
                    modelUsed = readRDS("models/10HN.rda")
                },
                "tab4" = {
                    modelUsed = readRDS("models/20HN.rda")
                }
            )

            data = data.frame(
                "cement" = input$cementValue,
                "slag" = input$slagValue, 
                "ash" = input$ashValue, 
                "water" = input$waterValue,
                "superplastic" = input$superPlasticValue,
                "coarseagg" = input$coarseaggValue,
                "fineagg" = input$fineaggValue,
                "age" = input$ageValue,
                "strength" = input$strengthValue
            )

            result = customPrediction(modelUsed,data)



            output$predictedValueOutput =  renderUI(
                h1(
                    "Predicted Strength is ",
                    tags$mark(format(result,digits=4)),
                    align="center"
                )
            
            )
        }
    )


    
  
}