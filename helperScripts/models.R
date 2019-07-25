library(neuralnet)

# print(getwd())
concrete = read.csv("rawData/concrete.csv")

normalize = function(x){
    return (
        (x-min(x)) / (max(x)-min(x))
    )
}

denormalize = function(x){
    return (
        x*(max(x)-min(x))+min(x)
    )
}

concrete_norm = as.data.frame(lapply(concrete, normalize))
concrete_train = concrete_norm[1:773, ] 
concrete_test =concrete_norm[774:1030, ]

write.csv(concrete[774:1030,],"rawData/testData.csv")

generatePrediction = function(){
    # LOGIC
            
    model1 = readRDS("models/1HN.rda")
    model2 = readRDS("models/5HN.rda")
    model3 = readRDS("models/10HN.rda")
    model4 = readRDS("models/20HN.rda")

    result1 = compute(model1, concrete_test)
    result2 = compute(model2, concrete_test)
    result3 = compute(model3, concrete_test)
    result4 = compute(model4, concrete_test)

    pStrength1 = result1$net.result
    pStrength2 = result2$net.result
    pStrength3 = result3$net.result
    pStrength4 = result4$net.result

    graphDataFrame1 = data.frame(concrete_test,"PredictedStrength"= pStrength1)
    graphDataFrame2 = data.frame(concrete_test,"PredictedStrength"= pStrength2)
    graphDataFrame3 = data.frame(concrete_test,"PredictedStrength"= pStrength3)
    graphDataFrame4 = data.frame(concrete_test,"PredictedStrength"= pStrength4)

    return (list(graphDataFrame1,graphDataFrame2,graphDataFrame3,graphDataFrame4))

    # return (pStrength1)

}

customPrediction = function(model,data){

data = rbind(concrete,data)

write.csv(data,"temp.csv")

dataCsv = read.csv("temp.csv")

dataNorm = as.data.frame(lapply(dataCsv, normalize))

result = compute(model, dataNorm[774:1031, ])

resultDataFrame = data.frame(
    dataNorm
)

resultDataFrame[1031,10] = tail(result$net.result,1)

write.csv(resultDataFrame,"resultDataFrame.csv")

toDenormalized = read.csv("resultDataFrame.csv")

dataUn = as.data.frame(lapply(toDenormalized, denormalize))

write.csv(dataUn,"Denormalized.csv")

pre = tail(dataUn,1)

return(
    dataUn[1031,11]
)


}