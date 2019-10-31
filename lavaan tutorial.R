garden<-read.csv(file.choose())

library(lavaan)

## example from tutorials
#CFA
myModel <- ' GMW  ~ nutrients + plants + microbes
                  nutrients ~ plants + Ca2012+microbes
                  microbes ~ plants+nutreints+SporeAbundance

           
               plants =~ GDTrt+SDTrt 
                nutrients =~ Ca2012 
               microbes =~ SporeAbundance
   
               Ca2012 ~~ Ca2008 
               K2012 ~~ K2008
pH2012+pH2008~~Ca2012+Ca2008
        

               
           '
HS.model<-'visual =~ x1+x2+x3
            textual=~ x4+x5+x6
            speed =~ x7 +x8+x9'
fit<-cfa(HS.model,data=HolzingerSwineford1939)
summary(fit,fit.measures=TRUE)

## SEM example
model <- '
  # measurement model
ind60 =~ x1 + x2 + x3
dem60 =~ y1 + y2 + y3 + y4
dem65 =~ y5 + y6 + y7 + y8
# regressions
dem60 ~ ind60
dem65 ~ ind60 + dem60
# residual correlations
y1 ~~ y5
y2 ~~ y4 + y6
y3 ~~ y7
y4 ~~ y8
y6 ~~ y8
'
fit<-sem(model, data=PoliticalDemocracy)
summary(fit,standardized=TRUE)

## attempt with garden data; make figure
library(dagitty)
g<-dagitty("dag{GDTrt-> Diversity
                MDTrt-> Diversity
                SoilMoisture->SoilFactors
                SporeAbundance -> FungalAbundance
                Diversity -> SoilAg
                SoilFactors -> SoilAg}")
plot(graphLayout(g))


                