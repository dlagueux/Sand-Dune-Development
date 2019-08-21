rm(list=ls(all=TRUE)) #give R a blank slate

setwd("C:/Users/dlagueux/Desktop/My Documents/DSE project/EDGE sites/")

library(stringr)

library(car)
library(visreg)


#import OTU file for each sample
#NOTE: this has already been merged with env data
edge_rich<-read.csv(file.choose())
#in original file from Ari: use the text to columns excel function to convert group column
#to separate out library identifier (delimited by ".") R does not like separating by "."
#put rest of info into group2 column
#also in excel convert SEV_A_ to SEVB and SEV_B_ to SEVG (B = blue grama grassland, G = black grama grassland)
edge_env<-read.csv(file.choose())

##### LINK Richness/Diversity file with Environmental Data--------------
#to merge env data with richness, we need a common code that indicates siteXplot
#in richness file, create separate columns for siteXplot
#we can do this by subdividing the group column, and saving the components in unique columns
r1<-as.data.frame(str_split_fixed(edge_rich$group2,"_",n=3))
#d1 is just a temporary data file
edge_rich$site<-r1$V1
edge_rich$species<-r1$V2
edge_rich$plot<-r1$V3
summary(edge_rich$plot)
edge_rich$site_plot<-as.factor(paste(edge_rich$site,edge_rich$plot,sep="_"))
#check to see that it worked
summary(edge_rich$site_plot)

#now we are ready to merge in the env data
edge<-merge(edge_rich,edge_env,by.x=c("site_plot","site","plot","block","trt"),by.y=c("site_plot","site","plot","block","trt"),
                all.x=TRUE,incomparables=NULL)
#check that it worked
summary(edge)
# write out the file 
write.csv(edge,"edge.csv")

edge<-read.csv(file.choose())

CHR<-subset(edge,trt=='CHR')
summary(CON$trt)
CON<-subset(edge,trtrtt=='CON')
mean(CHR$avg_GWC)
mean(CON$avg_GWC)
  #### RICHNESS -------------
#make sure that site, gradient, and species are understood to be factors by R

#need to log(sobs) for normality
edge$sobsl<-log(edge$sobs)
m.r.EDGE<-lm(sobsl~site+trt+species+site*trt+species*trt,data=edge)
#analysis of deviance results for the mixed model
Anova(m.r.EDGE,type=3)
#site is significant



#test analysis assumptions
#normality of residuals
hist(resid(m.r.EDGE))
qqnorm(resid(m.r.EDGE))
shapiro.test(resid(m.r.EDGE))
# one high outlier
#note: cut-off for Shapiro is recommended at p=0.01
#as long as p>0.01, normality assumption is met

#homogeneity of variances
plot(m.r.EDGE)

####Hit <Return> to see next plot: 
#looks fine, should not show any pattern

#visualize the model
visreg(m.r.EDGE,"trt",by="species") ## figure 1
#not a lot of difference here
library(emmeans)
emmeans(m.r.EDGE,pairwise~trt)
emmeans(m.r.EDGE,pairwise~site)
#CPR-HAR, CPR-HPG, CPR-SEVB, CPR-SEVG, HAR-SEVB, HAR-SEVG, HPG-SEVB, HPG-KNZ, HPG-SEVB, HPG-SEBG, KNZ-SEVB, KNZ-SEVG, SEVB-SEVG are all significant once you log(sobs)
#by site, not species; pairwise comparison of sites to each other
visreg(m.r.EDGE,"site",main="Figure 1",xlab="Site",ylab="Log OTUs Observed") ## figure 2

#ANGE, SCSC look like they could be significant
emmeans(m.r.EDGE,pairwise~trt|species)  ## BOER and BOGR different than each other, no other pairs significant
#no significance



#### CHAO 1 ------------- prediction of richness
#more sensitive to rare species-- look up
#make sure that site, gradient, and species are understood to be factors by R
#use log(chao)
edge$chao1<-as.factor(edge$chao)
m.c.EDGE<-lm(chao~site+species+trt,data=edge)
#analysis of deviance results for the mixed model
Anova(m.c.EDGE,type=3)
#site is significant
library(MuMIn)
m.c.EDGE.lat<-lm((chao)~latitude+trt+species+latitude*trt+species*trt,data=edge)
outlierTest(m.c.EDGE.lat)
leveragePlots(m.c.EDGE.lat)
m.c.e.lat<-lm((chao)~latitude+trt+species+latitude*trt+species*trt,data=e)
outlierTest(m.c.e.lat)
plot(chao,latitude,data=m.c.e.lat)
AICc(m.c.EDGE.lat)
Anova(m.c.EDGE.lat)
e<-subset(edge,edge$X!=74)
#test analysis assumptions
#normality of residuals
hist(resid(m.c.EDGE))
qqnorm(resid(m.c.EDGE))
#another single high outlier
shapiro.test(resid(m.c.EDGE))
#note: cut-off for Shapiro is recommended at p=0.01
#as long as p>0.01, normality assumption is met

#homogeneity of variances
plot(m.c.EDGE)

#looks fine, should not show any pattern

#visualize the model
visreg(m.c.EDGE.lat,"trt",by="site")
#HAR may be significant
emmeans(m.c.EDGE,pairwise~trt|species)
x<-lm(chao~latitude,data=edge)
plot(chao,latitude,data=edge)
#not significant but HAR is the closest at 0.1; so site is overall significant but no particular site is
#by species
visreg(m.c.EDGE,"site")
#SCSC, ANGE, BOER may be sig
emmeans(m.c.EDGE,pairwise~trt|site)
#not significant



#### SIMPSON EVENNESS -------------
#make sure that site, gradient, and species are understood to be factors by R
m.e.EDGE<-lm(simpsoneven~site+trt+species+species*trt,data=edge)
#analysis of deviance results for the mixed model
Anova(m.e.EDGE,type=3)
#species, and trt:species significant; site:trt close
#how to examine site:trt and trt:species more closely?----------------------

#test analysis assumptions
#normality of residuals
hist(resid(m.e.EDGE))
qqnorm(resid(m.e.EDGE))
shapiro.test(resid(m.e.EDGE))
#looks fine
#note: cut-off for Shapiro is recommended at p=0.01
#as long as p>0.01, normality assumption is met

#homogeneity of variances
plot(m.e.EDGE)

#looks fine, should not show any pattern

#visualize the model
visreg(m.e.EDGE,"trt",by="site") ######
#SEVG, CPR, SEVB look sig
emmeans(m.e.EDGE,pairwise~trt|site)
#SEVB, SEVG significant (although site is not overall sig); control greater than drought
#by species
visreg(m.e.EDGE,"trt",by="species")
#BOCU, BOER, SCSC look sig
emmeans(m.e.EDGE,pairwise~trt|species)
# only BOCU is sig, control more evenness than drought

library(emmeans)
e.spp<-lsmeans(m.e.EDGE,~trt|species)
e.spp
contrast(e.spp,adjust="none")
#BOCU is significant
tapply(edge$simpsoneven,edge$trt:edge$species,mean)
#not sure what these values mean
e.site<-lsmeans(m.e.EDGE,~trt|site)
e.site
contrast(e.site,adjust="none")
#SEVB significant, SEVG; averages trt effects for all sp in plots
tapply(edge$simpsoneven,edge$trt:edge$site,mean)
#not sure what these values mean

#### SHANNON DIV -------------
#make sure that site, gradient, and species are understood to be factors by R
m.h.EDGE<-lm(shannon~site+trt+species+species*trt,data=edge)
#analysis of deviance results for the mixed model
m.h.EDGE.lat<-lm(shannon~trt+species+species*trt+latitude, data=edge)
Anova(m.h.EDGE.lat,type=3)
#site, sp, and trt:species sig_______________ again how to go forward?
plot(edge$latitude, edge$shannon, data=edge)
plot(edge$avg_pH,edge$shannon)
#test analysis assumptions
#normality of residuals
hist(resid(m.h.EDGE))
qqnorm(resid(m.h.EDGE))
shapiro.test(resid(m.h.EDGE))
#looks fine
#note: cut-off for Shapiro is recommended at p=0.01
#as long as p>0.01, normality assumption is met

#homogeneity of variances
plot(m.h.EDGE.lat)
#looks fine, should not show any pattern

#visualize the model
visreg(m.h.EDGE,"trt",by="site")
#SEBV, SEVG look sig
emmeans(m.h.EDGE,pairwise~trt|site)
#SEVB sig, SEVG
#by species
visreg(m.h.EDGE,"trt",by="species")
emmeans(m.h.EDGE,pairwise~trt|species)
#BOCU sig

library(emmeans)
h.spp<-lsmeans(m.h.EDGE,~species)
h.spp
contrast(h.spp,adjust="none")
tapply(edge$shannon,edge$trt:edge$species,mean)
tapply(edge$shannon,edge$trt:edge$species,length)
h.site<-lsmeans(m.h.EDGE,~shannon|latitude)
h.site
contrast(h.site,adjust="none")
tapply(edge$shannon,edge$trt:edge$site,mean)
emmeans(m.h.EDGE,pairwise~trt|species)

CPR<-subset(edge, site=="CPR")
mean(CPR$avg_pH ) ##6.22

HAR<-subset(edge,site=="HAR")
mean(HAR$avg_pH) ##7.598

HPG<-subset(edge,site=="HPG")
mean(HPG$avg_pH) ##6.54

KNZ<-subset(edge,site=="KNZ")
mean(KNZ$avg_pH) ##6.28222

SEVB<-subset(edge,site=="SEVB")
mean(SEVB$avg_pH) ##7.94

SEVG<-subset(edge,site=="SEVG")
mean(SEVG$avg_pH) ##8.34


## sample size calculations for species*site*trt

sum(with(edge,trt=="CHR" & code =="SEVG_SCSC"))
