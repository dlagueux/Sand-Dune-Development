garden<-read.csv(file.choose())

library(lavaan)

########
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
summary(fit,standardized=TRUE)#example from tutorial
#######
## attempt with garden data; make figures
library(dagitty)
fulladd<-dagitty("dag{GDTrt-> Plants
                SDTrt-> Plants
                SoilMoisture -> Chemistry
                SporeAbundance -> Fungi
                Plants -> SoilAg
                Chemistry -> SoilAg
                Fungi -> SoilAg
                pH -> Chemistry
                P -> Chemistry
                K -> Chemistry
                Ca -> Chemistry
                Mg -> Chemistry}")
plot(graphLayout(fulladd),main="Full Additive Model")

## plants effect everything model
planteffect<-dagitty("dag{
                     Plants->SoilAg
                     Chemistry->SoilAg
                     Fungi->SoilAg
                     Plants->Chemistry
                     Plants->Fungi}")
plot(graphLayout(planteffect))

##plants effect everything and fungi and chemistry affect each other
complicated<-dagitty("dag{
                     Plants->SoilAg
                     Chemistry->SoilAg
                     Fungi->SoilAg
                     Plants->Chemistry
                     Plants->Fungi
                     Chemistry->Fungi
                     Fungi->Chemistry}")
plot(graphLayout(complicated))

##let's test some of these
#model ignoring plants - finally got it to converge, but still not a good fit 
noplants <- ' 
# measurements
microbes =~ SporeAbun10n + SporeAbun09n
chemistry =~ pH2012 + pH08 + SoilMoisture + Mg2012n + Mg08n + Ca2012n + Ca08n
SoilAg =~ GMW + MWD
# regressions
SoilAg ~ chemistry + microbes
chemistry ~ microbes
# residual correlations
SporeAbun10n ~~ Spore Abun09n
pH2012~~pH08
Mg2012n~~Mg08n
Ca2012n~~Ca08n
GMW~~MWD

'
fit1<-sem(noplants,data=garden2)
summary(fit1)
varTable(fit1)
## web for this would be...
shownoplants<-dagitty("dag{microbes->SporeAbundance
                      chemistry->pH
                      chemistry->SoilMoisture
                      chemistry->Ca
                      chemistry->Mg
                      SoilAg->GMW
                      SoilAg->MWD
                      chemistry->SoilAg
                      microbes->SoilAg
                      chemistry->microbes}")
plot(graphLayout(shownoplants))

## plants don't effect chemistry OR microbes, but do effect soil aggregation - not a great fit either
model2 <- ' 
# measurements
microbes =~ SporeAbun10n + SporeAbun09n
chemistry =~ pH2012 + pH08 + SoilMoisture + Mg2012n + Mg08n + Ca2012n + Ca08n
SoilAg =~ GMW + MWD
# regressions
SoilAg ~ chemistry + microbes + SDTrt + GDTrt
chemistry ~ microbes
# residual correlations
SporeAbun10n ~~ Spore Abun09n
pH2012~~pH08
Mg2012n~~Mg08n
Ca2012n~~Ca08n
GMW~~MWD

'
fit1<-sem(model2,data=garden2)
summary(fit1)

## fixing the variables with insane vars; also removing a bunch of NAs, not sure where those came from.
garden2<-na.omit(garden)
#ordered, not integers (can't have factors for SEM)
garden2$GDTrt<-as.integer(garden2$GDTrt)
garden2$SDTrt<-as.integer(garden2$SDTrt)
garden2$Ca2012n<-log(garden2$Ca2012)
garden2$Ca08n<-log(garden2$Ca08)
# had a 0 entry for Mg08, so can't just log it
garden2$Mg2012n<-log((garden2$Mg2012)+1)
garden2$Mg08n<-log((garden2$Mg08)+1)
garden2$SporeAbun09n<-log(garden2$SporeAbun09)
garden2$SporeAbun10n<-log(garden2$SporeAbun10)

## new model - 