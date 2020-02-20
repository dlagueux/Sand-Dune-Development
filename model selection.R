##packages needed
library(lavaan)
library(car)
library(emmeans)
library(semPlot) ##makes good diagrams with numbers
##ex. semPaths(fit2,whatLabels = "std",edge.label.cex = 1) ##need to read up on this, not sure what this diagram shows

##read in data and prepare
garden<-read.csv(file.choose())
##garden$GDTrt<-as.factor(garden$GDTrt) don't use this right now
##garden$SDTrt<-as.factor(garden$SDTrt)
garden$GDTrt<-as.numeric(garden$GDTrt)
garden$SDTrt<-as.numeric(garden$SDTrt)

#clean the dataset, remove outlier obs 71
garden2<-garden[1:166,]
garden<-garden2[-71,]

##fixing scaling issues with calcium
garden$Ca2012n<-scale(garden$Ca2012)
garden$Ca08n<-scale(garden$Ca08)
garden$Ca2012<-garden$Ca2012n
garden$Ca08<-garden$Ca08n
garden$K08n<-scale(garden$K08)
garden$K2012n<-scale(garden$K2012)


##model 1 - doesn't converge when K is included!
model1<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes + GDTrt + SDTrt
                  
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit1<-cfa(model1,data=garden, estimator = "MLM")
summary(fit1,fit.measures=TRUE) #report p-value, robust CFI, and robust SRMR in table

##model 2 does converge with K included
model2<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture + v6*K08n + v6*K2012n
                  SoilAg =~ v5*GMW + v5*MWD
                  


                 SoilAg  ~ nutrients + GDTrt + SDTrt
                 nutrients ~ microbes
                

               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit2<-cfa(model2,data=garden, estimator = "MLM")
summary(fit2,fit.measures=TRUE)

##model3
model3<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                    nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                 SoilAg  ~ nutrients + GDTrt + SDTrt
                 nutrients ~ microbes
                 microbes ~ GDTrt + SDTrt
                

               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit3<-cfa(model3,data=garden, estimator = "MLM")
summary(fit3,fit.measures=TRUE)

##model 4
model4<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n +v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes
                  microbes ~ GDTrt + SDTrt
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit4<-cfa(model4,data=garden, estimator = "MLM")
summary(fit4,fit.measures=TRUE)


#soilag by chem, chem by everything else
model5<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients 
                  nutrients ~ GDTrt + SDTrt + microbes
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '

library(dagitty)
g<-dagitty("dag{GDTrt-> Diversity
                MDTrt-> Diversity
                SoilMoisture->SoilFactors
                SporeAbundance -> FungalAbundance
                Diversity -> SoilAg
                SoilFactors -> SoilAg}")
plot(graphLayout(g))


fit5<-cfa(model5,data=garden, estimator = "MLM")
summary(fit5,fit.measures=TRUE)

#soil ag by microbes and chem, chem by plants
model6<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes
                 nutrients ~ GDTrt + SDTrt
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit6<-cfa(model6,data=garden, estimator = "MLM")
summary(fit6,fit.measures=TRUE)


##model 7
model7<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + GDTrt + SDTrt
                
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               
               GMW~~MWD
           '


fit7<-cfa(model7,data=garden, estimator = "MLM")
summary(fit7,fit.measures=TRUE)

# model 8 , no chem
model8<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ microbes +GDTrt + SDTrt
                 
                
               
               
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit8<-cfa(model8,data=garden, estimator = "MLM")
summary(fit8,fit.measures=TRUE)
AIC(cfafite) ##2521


#soilag by chem, chem by plants, no microbes
model9<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients
                nutrients ~ GDTrt + SDTrt
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
              
               GMW~~MWD
           '


fit9<-cfa(model9,data=garden, estimator = "MLM")
summary(fit9,fit.measures=TRUE)
AIC(cfafite) ##2521

#soilag~chem +GD, chemistry~ microbes,exclude SD
model10<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + GDTrt
                 nutrients ~ microbes
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit10<-cfa(model10,data=garden, estimator = "MLM")
summary(fit10,fit.measures=TRUE)

# same thing, no GD
model11<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + SDTrt
                 nutrients ~ microbes
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit11<-cfa(model11,data=garden, estimator = "MLM")
summary(fit11,fit.measures=TRUE)



##soil ag by nutrients, microbes SD.
model12<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes + SDTrt
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit12<-cfa(model12,data=garden, estimator = "MLM")
summary(fit12,fit.measures=TRUE)



##soil ag by nutrients, microbes GD.
model13<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes + GDTrt
                
               K08n~~K2012n
               pH08+pH2012~~K08n+K2012n
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit13<-cfa(model13,data=garden, estimator = "MLM")
summary(fit13,fit.measures=TRUE)


# try including K and P; doesn't converge, we should continue to exclude them 
garden$K2012<-scale(garden$K2012)
garden$K08<-scale(garden$K08)
fullmodel1<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture + v6*K2012 + v6*K08 + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes + GDTrt + SDTrt
                  
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012 ~~ K08
               P2012~~P08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '





########extra########
fullfit1<-cfa(fullmodel1,data=garden, estimator = "MLM")
summary(fullfit1,fit.measures=TRUE)
AIC(cfafite) ##2521




#soil ag by microbes and chem, chem by plants
mymodeld<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes
                 nutrients ~ GDTrt + SDTrt
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012n ~~ K08n
               P2012~~P08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodeld,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521


##no microbes
mymodele<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + GDTrt + SDTrt
                
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012n ~~ K08n
               P2012~~P08
               GMW~~MWD
           '


cfafite<-cfa(mymodele,data=garden2, estimator = "MLM")

summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

# no chem
mymodelf<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ microbes +GDTrt + SDTrt
                 
                
               
               
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodelf,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

#soil by chem and plants, chem by microbes
garden2$K2012n<-log(garden2$K2012)
garden2$K08n<-log(garden2$K08)
mymodelg<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture 
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + GDTrt + SDTrt
                 nutrients ~ microbes
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodelg,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521
varTable(cfafite)

#soilag by chem, chem by plants, no microbes
mymodelh<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients
                nutrients ~ GDTrt + SDTrt
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012n ~~ K08n
               P2012~~P08
              
               GMW~~MWD
           '


cfafite<-cfa(mymodelh,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

#soilag~chem +GD, chemistry~ microbes,exclude SD
mymodeli<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + GDTrt
                 nutrients ~ microbes
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012n ~~ K08n
               P2012~~P08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodeli,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

# same thing, no GD
mymodelj<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + SDTrt
                 nutrients ~ microbes
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012n ~~ K08n
               P2012~~P08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodelj,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

##soil ag by nutrients, microbes SD.
mymodelk<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes + SDTrt
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012n ~~ K08n
               P2012~~P08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodelk,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521


##soil ag by nutrients, microbes GD.
mymodell<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes + GDTrt
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               K2012n ~~ K08n
               P2012~~P08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodell,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521