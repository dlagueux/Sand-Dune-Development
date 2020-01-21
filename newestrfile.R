library("lavaan")


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
                                               
                                               
                                               
  mymodelnew<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
        nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture 
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                 SoilAg  ~ nutrients + GDTrt + SDTrt
                 nutrients ~ microbes
                 microbes ~ GDTrt + SDTrt
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '
                                               
                                               
                                               cfafite<-cfa(mymodelnew,data=garden2, estimator = "MLM")
                                               summary(cfafite,fit.measures=TRUE)
                                               AIC(cfafite) ##2521
                                               varTable(cfafite)
                                               
                                               
                                               
                                               
                                               
library("emmeans")
model<-lm(Mg2012~SDTrt+GDTrt,data=datawithout71)
nova(model)
library(car)


datawithout71$GDTrt<-as.factor(datawithout71$GDTrt)
datawithout71$SDTrt<-as.factor(datawithout71$SDTrt)
x<-emmeans(model,pairwise~SDTrt|GDTrt)
x<-emmeans(model,"SDTrt")
x
