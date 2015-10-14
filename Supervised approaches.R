## Naive Bayes
nb <- function(train,test){
    require(e1071,quietly=T)
    sup<-which(train$Insp !='unkn')
    data<-train[sup,c('ID','Prod','Uprice','Insp')]
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    model <- naiveBayes(Insp~.,data)
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
    return(list(rankOrder=order(preds[,'fraud'],decreasing=T),rankScore=preds[,'fraud']))
}

ho.nb<-function(form,train,test,...){
    res <- nb(train,test)
    structure(evalOutlierRanking(test,res$rankOrder,...),
              itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=='fraud',1,0)))
}

nb.res <- holdOut(learner('ho.nb',
                          pars=list(Threshold=.1,statsProds=globalStats)),
                  dataset(Insp~.,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE)

summary(nb.res)
png('nb_orh.png')
par(mfrow=c(1,2))
info <- attr(nb.res,'itsInfo')
PTs.nb <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))
PRcurve(PTs.nb[,,1],PTs.nb[,,2], main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=2,col='grey',
        avg='vertical')
legend('topright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))
CRchart(PTs.nb[,,1],PTs.orh[,,2],main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=2,col='grey',
        avg='vertical')
legend('bottomright',c('NaiveBayes','ORh'),lty=1,col=c('black','grey'))
dev.off()

## Naive Bayes with modified training set
nb.s <- function(train,test){
    require(e1071,quietly=T)
    sup<-which(train$Insp !='unkn')
    data<-train[sup,c('ID','Prod','Uprice','Insp')]
    data$Insp <- factor(data$Insp,levels=c('ok','fraud'))
    # SMOTE to fix inbalanced sampling by over-sampling
    newData <- SMOTE(Insp~.,data,perc.over=700)
    model <- naiveBayes(Insp~.,newData)
    preds <- predict(model,test[,c('ID','Prod','Uprice','Insp')],type='raw')
    return(list(rankOrder=order(preds[,'fraud'],decreasing=T),rankScore=preds[,'fraud']))
}

ho.nbs<-function(form,train,test,...){
    res <- nb.s(train,test)
    structure(evalOutlierRanking(test,res$rankOrder,...),
              itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=='fraud',1,0)))
}


nbs.res <- holdOut(learner('ho.nbs',
                          pars=list(Threshold=.1,statsProds=globalStats)),
                  dataset(Insp~.,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=TRUE)

summary(nbs.res)

png('nb_over_sampling.png')
par(mfrow=c(1,2))
info <- attr(nbs.res,'itsInfo')
PTs.nbs <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))
PRcurve(PTs.nb[,,1],PTs.nb[,,2], main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
PRcurve(PTs.nbs[,,1],PTs.nbs[,,2], add=T,lty=2,avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=2,col='grey',
        avg='vertical')
legend('topright',c('NaiveBayes','smoteNaiveBayes','ORh'),lty=c(1,2,1),col=c('black','black','grey'))
CRchart(PTs.nb[,,1],PTs.orh[,,2],main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),avg='vertical')
CRchart(PTs.nbs[,,1],PTs.nbs[,,2], add=T,lty=2,avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=2,col='grey',
        avg='vertical')
legend('bottomright',c('NaiveBayes','smoteNaiveBayes','ORh'),lty=c(1,2,1),col=c('black','black','grey'))
dev.off()
