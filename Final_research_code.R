datawithout71$Ca2012n<-scale(datawithout71$Ca2012)
datawithout71$Ca08n<-scale(datawithout71$Ca08)
datawithout71$Ca2012<-datawithout71$Ca2012n
datawithout71$Ca08<-datawithout71$Ca08n
datawithout71$K08n<-scale(datawithout71$K08)
datawithout71$K2012n<-scale(datawithout71$K2012)


garden2$K2012n<-log(garden2$K2012)
garden2$K08n<-log(garden2$K08)


library(lavaan)

model1<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+ v7*P08+v7*P2012+v6*K08n+v6*K2012n
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ nutrients + microbes + GDTrt + SDTrt
                  
                
               
               pH08+pH2012~~Mg08+Mg2012
               pH08+pH2012~~Ca08+Ca2012
               Mg08~~Mg2012
               Ca2012 ~~ Ca08 
               pH2012~~pH08
               SporeAbun09~~SporeAbun10
               GMW~~MWD
               P08~~P2012
           '


fit1<-cfa(model1,data=datawithout71, estimator = "MLM")
summary(fit1,fit.measures=TRUE) #report p-value, robust CFI, and robust SRMR in table

##model 2 does converge with K included
model2<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture + v6*K08n + v6*K2012n+ v7*P08+v7*P2012
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
               P08~~P2012
               
           '


fit2<-cfa(model2,data=datawithout71, estimator = "MLM")
summary(fit2,fit.measures=TRUE)

##model3
model3<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                    nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '


fit3<-cfa(model3,data=datawithout71, estimator = "MLM")
summary(fit3,fit.measures=TRUE)

##model 4
model4<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n +v6*K2012n+ v7*P08+v7*P2012
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
               P08~~P2012
           '


fit4<-cfa(model4,data=datawithout71, estimator = "MLM")
summary(fit4,fit.measures=TRUE)


#soilag by chem, chem by everything else
model5<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '
fit5<-cfa(model5,data=datawithout71, estimator = "MLM")
summary(fit5,fit.measures=TRUE)




fit5<-cfa(model5,data=garden, estimator = "MLM")
summary(fit5,fit.measures=TRUE)

#soil ag by microbes and chem, chem by plants
model6<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '


fit6<-cfa(model6,data=datawithout71, estimator = "MLM")
summary(fit6,fit.measures=TRUE)


##model 7
model7<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture +v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '


fit7<-cfa(model7,data=datawithout71, estimator = "MLM")
summary(fit7,fit.measures=TRUE)

# model 8 , no chem
model8<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  
                  SoilAg =~ v5*GMW + v5*  MWD
                  
                  SoilAg  ~ microbes +GDTrt + SDTrt
                 
                
               
               
               SporeAbun09~~SporeAbun10
               GMW~~MWD
           '


fit8<-cfa(model8,data=datawithout71, estimator = "MLM")
summary(fit8,fit.measures=TRUE)
AIC(cfafite) ##2521


#soilag by chem, chem by plants, no microbes
model9<-   '    
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n+ v7*P08+v7*P2012
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
               P08~~P2012
           '



fit9<-cfa(model9,data=datawithout71, estimator = "MLM")
summary(fit9,fit.measures=TRUE)
AIC(cfafite) ##2521

#soilag~chem +GD, chemistry~ microbes,exclude SD
model10<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '


fit10<-cfa(model10,data=datawithout71, estimator = "MLM")
summary(fit10,fit.measures=TRUE)

# same thing, no GD
model11<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '


fit11<-cfa(model11,data=datawithout71, estimator = "MLM")
summary(fit11,fit.measures=TRUE)



##soil ag by nutrients, microbes SD.
model12<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '


fit12<-cfa(model12,data=datawithout71, estimator = "MLM")
summary(fit12,fit.measures=TRUE)



##soil ag by nutrients, microbes GD.
model13<-   '    microbes =~ v1*SporeAbun09+ v1*SporeAbun10 
                  nutrients =~ v2*Mg2012+ v2*Mg08+ v3*Ca2012+ v3*Ca08+ v4*pH2012+ v4*pH08 + SoilMoisture+v6*K08n+v6*K2012n+v7*P08+v7*P2012
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
               P08~~P2012
           '


fit13<-cfa(model13,data=datawithout71, estimator = "MLM")
summary(fit13,fit.measures=TRUE)


