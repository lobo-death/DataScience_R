install.packages("bnlearn")
install.packages("bnviewer")
install.packages("StatMeasures")
library(bnlearn)
library(bnviewer)
#install.packages("StatMeasures")
library(StatMeasures)

df_wine = read.csv("winequality-red.csv", sep = ";")

df_wine$quality = as.numeric(df_wine$quality)

bn.learn.wine = hc(df_wine) #90.47664
#bn.learn.wine = bnlearn::tabu(df_wine) #90.47664
#bn.learn.wine = bnlearn::mmhc(df_wine) #88.75108

###
# ---------------------------------------------
# Strength.viewer
# ---------------------------------------------
###
bayesianNetwork.boot.strength = boot.strength(df_wine, R = 20, algorithm = "hc")

avg.bayesianNetwork = averaged.network(bayesianNetwork.boot.strength, threshold = 0.81)

strength.viewer(
  avg.bayesianNetwork,
  bayesianNetwork.boot.strength,
  bayesianNetwork.background = "#282c34",
  bayesianNetwork.arc.strength.threshold.expression = c("@threshold > 0 & @threshold < 0.5",
                                                        "@threshold >= 0.5 & @threshold <= 0.8",
                                                        "@threshold > 0.8 & @threshold <= 1"),
  
  bayesianNetwork.arc.strength.threshold.expression.color  = c("red", "yellow", "blue"),
  bayesianNetwork.arc.strength.threshold.alternative.color =  "white",
  
  bayesianNetwork.arc.strength.label = TRUE,
  bayesianNetwork.arc.strength.label.prefix = "",
  bayesianNetwork.arc.strength.label.color = "white",
  
  bayesianNetwork.arc.strength.tooltip = TRUE,
  
  bayesianNetwork.edge.scale.min = 1,
  bayesianNetwork.edge.scale.max = 3,
  
  bayesianNetwork.edge.scale.label.min = 14,
  bayesianNetwork.edge.scale.label.max = 14,
  
  bayesianNetwork.width = "100%",
  bayesianNetwork.height = "800px",
  bayesianNetwork.layout = "layout_with_sugiyama",
  node.colors = list(background = "black",
                     border = "#2b7ce9",
                     highlight = list(background = "#e91eba",
                                      border = "#2b7ce9")),
  
  node.font = list(color = "white", face="Arial"),
  edges.dashes = FALSE
)

# ---------------------------------------------

viewer(bn.learn.wine,
       bayesianNetwork.width = "100%",
       bayesianNetwork.height = "80vh",
       bayesianNetwork.layout = "layout_with_sugiyama",
       bayesianNetwork.title = "Quality - Wine"
)

training.set <- df_wine[1:1000,]

test.set <- df_wine[1000:500,]

fit.bayesian <- bn.fit(bn.learn.wine, data = training.set, method = "mle")

predict.bayesian <- predict(fit.bayesian, "quality", test.set)

real <- test.set[,"quality"]
previsto <- predict.bayesian

mape <- mape(y = real, yhat = previsto)
100 - mape*100
