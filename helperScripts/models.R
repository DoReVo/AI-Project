library(neuralnet)

print(getwd())
concrete = read.csv("rawData/concrete.csv")



normalize = function(x){
    return (
        (x-min(x)) / (max(x)-min(x))
    )
}

concrete_norm = as.data.frame(lapply(concrete, normalize))
concrete_train = concrete_norm[1:773, ] 
concrete_test =concrete_norm[774:1030, ]

generatePrediction = function(){
    # LOGIC
            
    model1 = readRDS("models/1HN.rda")
    model2 = readRDS("models/5HN.rda")
    model3 = readRDS("models/10HN.rda")
    model4 = readRDS("models/20HN.rda")

    result1 = compute(model1, concrete_test[1:8])
    result2 = compute(model2, concrete_test[1:8])
    result3 = compute(model3, concrete_test[1:8])
    result4 = compute(model4, concrete_test[1:8])

    pStrength1 = result1$net.result
    pStrength2 = result2$net.result
    pStrength3 = result3$net.result
    pStrength4 = result4$net.result

    graphDataFrame1 = data.frame("PredictedStrength"= pStrength1,concrete_test)
    graphDataFrame2 = data.frame("PredictedStrength"= pStrength2,concrete_test)
    graphDataFrame3 = data.frame("PredictedStrength"= pStrength3,concrete_test)
    graphDataFrame4 = data.frame("PredictedStrength"= pStrength4,concrete_test)

    return (list(graphDataFrame1,graphDataFrame2,graphDataFrame3,graphDataFrame4))

    # return (pStrength1)

}