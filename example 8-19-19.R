  modelx<-lm((simpsoneven)~latitude+trt+species+longitude+GDD30yr,data=edge)
modelx
Anova(modelx)

modely<-lm((simpsoneven)~latitude+species,data=edge)
Anova(modely,type=3)

modela<-lm((simpsoneven)~latitude+trt+species+latitude*trt,data=edge)
Anova(modela)

### to change categorical variables from a character into a factor, use as.factor(Type) before the analysis

class(edge)
chao<-edge$chao
class(edge$chao)
