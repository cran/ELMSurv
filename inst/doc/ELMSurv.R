## ---- echo=TRUE, message=FALSE, warning=FALSE, results='asis'------------
set.seed(123)
require(ELMSurv)
require(survival)
## Survival Ensemble of ELM  with default settings
#Lung DATA
data(lung)
lung=na.omit(lung)
lung[,3]=lung[,3]-1
n=dim(lung)[1]
L=sample(1:n,ceiling(n*0.5))
trset<-lung[L,]
teset<-lung[-L,]
rii=c(2,3)
elmsurvmodel=ELMSurvEN(x=trset[,-rii],y=Surv(trset[,rii[1]], trset[,rii[2]]),testx=teset[,-c(rii)])

## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
testpretimes=elmsurvmodel$precitedtime
#The predicted survival times on the first test example
head(testpretimes[1,])
#The predicted survival times of all test examples by the third model
head(testpretimes[,3])
#We also get c-index values of the model
#library(survcomp)
#ci_elm=concordance.index(-rowMeans(elmsurvfit$precitedtime),teset$days,teset$status)[1] 
#print(ci_elm)

## ---- echo=TRUE, message=FALSE, warning=FALSE, results='base'------------
# Get the 1th base model
firstbasemodel=elmsurvmodel$elmsurvfit[[1]]
#Print the c-index values
#library(survcomp)
#ci_elm=concordance.index(-rowMeans(elmsurvfit$precitedtime),teset$days,teset$status)[1] 
#print(ci_elm)

## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
testpredicted=firstbasemodel$testpre
head(testpredicted)

## ---- echo=TRUE, message=FALSE, warning=FALSE----------------------------
trlength=length(elmsurvmodel$elmsurvfit)
msevalue=rep(0,trlength)
for (i in 1:trlength)
   msevalue[i]=elmsurvmodel$elmsurvfit[[i]]$trainMSE
plot(msevalue,xlab="base model number", ylab="MSE")

