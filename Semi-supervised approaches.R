# a test based on iris dataset
library(DMwR)
library(e1071)
data(iris)
idx <- sample(150,100)
tr <- iris[idx,]
ts <- iris[-idx,]
nb <- naiveBayes(Species~.,tr)
table(predict(nb,ts),ts$Species)
trST <- tr
nas <- sample(100,90)
trST[nas,'Species']<-NA
func <- function(m,d){
    p<-predict(m,d,type='raw')
    data.frame(cl=colnames(p)[apply(p,1,which.max)],
               p=apply(p,1,max))
}
nbSTbase <- naiveBayes(Species~.,trST[-nas,])
table(predict(nbSTbase,ts),ts$Species)
nbST <- SelfTrain(Species~.,trST,learner('naiveBayes',list()),'func')
table(predict(nbST,ts),ts$Species)

# implement to our dataset
pred.nb <- function(m,d){
    p<-predict(m,d,type='raw')
    data.frame(cl=colnames(p)[apply(p,1,which.max)],
               p=apply(p,1,max))
}
nb.st <- function(train,test){
    require(e1071,quietly = T)
    train <- train[,c('ID','Prod','Uprice','Insp')]
    train[which(train$Insp=='unkn'),'Insp']<-NA
    train$Insp <- factor(train$Insp,levels=c('ok','fraud'))
    model <- SelfTrain(Insp~.,train,learner('naiveBayes',list()),'pred.nb')
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
    return(list(rankOrder=order(preds[,'fraud'],decreasing = T),rankScore=preds[,'fraud']))
}
ho.nb.st <- function (form, train, test,...){
    res<-nb.st(train,test)
    structure(evalOutlierRanking(test,res$rankOrder,...),
              itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=='fraud',1,0)))
}
nb.st.res <- holdOut(learner('ho.nb.st',pars=list(Threshold=.1,statsProds=globalStats)),
                     dataset(Insp~.,sales),
                     hldSettings(3,.3,1234,T),itsInfo=T)
summary(nb.st.res)

# plot
png('nb_st.png')
par(mfrow=c(1,2))
info <- attr(nb.st.res,'itsInfo')
PTs.nb.st <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))
PRcurve(PTs.nb[,,1],PTs.nb[,,2],main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],add=T,lty=1,col='grey',avg='vertical')
PRcurve(PTs.nb.st[,,1],PTs.nb.st[,,2],add=T,lty=2,avg='vertical')
legend('topright',c('NaiveBayes','ORh','NaiveBayes-ST'),lty=c(1,1,2),col=c('black','grey','black'))
CRchart(PTs.nb[,,1],PTs.nb[,,2],main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],add=T,lty=1,col='grey',avg='vertical')
CRchart(PTs.nb.st[,,1],PTs.nb.st[,,2],add=T,lty=2,avg='vertical')
legend('bottomright',c('NaiveBayes','ORh','NaiveBayes-ST'),lty=c(1,1,2),col=c('black','grey','black'))
dev.off()
