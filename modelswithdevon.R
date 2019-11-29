datawithout71$GDTrt<-as.integer(datawithout71$GDTrt)
datawithout71$SDTrt<-as.integer(datawithout71$SDTrt)
fullm<- lm((GMW~Mg2012+Ca2012+K2012+P2012+pH2012+SoilMoisture+GDTrt+SDTrt+SporeAbun10)^2)
anova(fullm)



#########Models with devon##########
mymodela<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + microbes
                  microbes ~ GDTrt + SDTrt
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodela,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

semfite<-sem(mymodele, data=garden2)
summary(semfite,standardized=TRUE)
AIC(semfite) 

summary(cfafite,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)

# soil ag from everything
mymodelb<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
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


cfafite<-cfa(mymodelb,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521


#soilag by chem, chem by everything else
mymodelc<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients 
                  nutrients ~ GDTrt + SDTrt + microbes
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodelc,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

#soil ag by microbes and chem, chem by plants
mymodeld<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + microbes
                 nutrients ~ GDTrt + SDTrt
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodeld,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521


##no microbes
mymodele<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + GDTrt + SDTrt
                
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               
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
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + GDTrt + SDTrt
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


cfafite<-cfa(mymodelg,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521
varTable(cfafite)
#soilag by chem, chem by plants, no microbes
mymodelh<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients
                nutrients ~ GDTrt + SDTrt
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
              
               GMW~~MWD
           '


cfafite<-cfa(mymodelh,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

#soilag~chem +GD, chemistry~ microbes,exclude SD
mymodeli<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + GDTrt
                 nutrients ~ microbes
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodeli,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

# same thing, no GD
mymodelj<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + SDTrt
                 nutrients ~ microbes
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodelj,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

##soil ag by nutrients, microbes SD.
mymodelk<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + microbes + SDTrt
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodelk,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521


##soil ag by nutrients, microbes GD.
mymodell<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + microbes + GDTrt
                

               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


cfafite<-cfa(mymodell,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

###with weird####

##Models with devon

mymodela<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture + v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + microbes
                  microbes ~ GDTrt + SDTrt
                

               
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


cfafite<-cfa(mymodela,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521

semfite<-sem(mymodele, data=garden2)
summary(semfite,standardized=TRUE)
AIC(semfite) 

summary(cfafite,fit.measures=TRUE,standardized=TRUE,rsquare=TRUE)

# soil ag from everything
mymodelb<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture + v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients + microbes + GDTrt + SDTrt
                  
                

               
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


cfafite<-cfa(mymodelb,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
AIC(cfafite) ##2521


#soilag by chem, chem by everything else
mymodelc<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture + v6*K2012n + v6*K08n + v7*P2012 + v7*P08
                  SoilAg =~ v5*GMW + v5*  MWD
                  


                  SoilAg  ~ nutrients 
                  nutrients ~ GDTrt + SDTrt + microbes
                

               
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


cfafite<-cfa(mymodelc,data=garden2, estimator = "MLM")
summary(cfafite,fit.measures=TRUE)
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



