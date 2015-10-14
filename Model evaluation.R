## ROCR - Precision/Recall curves
library(ROCR)
par(mfcol = c(1,2))
data(ROCR.simple)
pred <- prediction(ROCR.simple$predictions, ROCR.simple$labels)
perf <- performance(pred,'prec','rec')
plot(perf)

## Interpolated precision
PRcurve <- function(preds,trues,...){
    require(ROCR, quietly=T)
    pd <- prediction(preds,trues)
    pf<-performance(pd,'prec','rec')
    pf@y.values<- lapply(pf@y.values,function(x)rev(cummax(rev(x))))
    plot(pf,...)
}
PRcurve(ROCR.simple$predictions, ROCR.simple$labels)

## Lift chart
pred <- prediction(ROCR.simple$predictions,ROCR.simple$labels)
perf <- performance(pred,'lift','rpp')
plot(perf,main='Lift Chart')

## Cumulative recall chart
CRchart <- function(preds, trues,...){
    require(ROCR,quietly=T)
    pd <- prediction(preds,trues)
    pf <- performance(pd,'rec','rpp')
    plot(pf,...)
}
CRchart(ROCR.simple$predictions, ROCR.simple$labels,main='Cumulative Recall Chart')

## Normalized Distance to Typical Price
avgNDTP <- function(toInsp,train,stats){
    if(missing(train)&&missing(stats))
        stop('Provide either the training data or the product stats')
    if (missing(stats)){
        notF <- which(train$Insp != 'fraud')
        stats <- tapply(train$Uprice[notF],
                        list(Prod=train$Prod[notF]),
                        function(x){
                            bp<-boxplot.stats(x)$stats
                            c(median=bp[3],iqr=bp[4]-bp[2])
                        })
        stats <- matrix(unlist(stats),length(stats),2,byrow=T,
                        dimnames=list(names(stats),c('median','iqr')))
        stats[which(stats[,'iqr']==0),'iqr']<- stats[which(stats[,'iqr']==0),'median']
    }
    mdtp <- mean(abs(toInsp$Uprice-stats[toInsp$Prod,'median'])/stats[toInsp$Prod,'iqr'])
    return(mdtp)
}

## evaluation matrix
evalOutlierRanking <- function(testSet, rankOrder, Threshold, statsProds){
    ordTS <- testSet[rankOrder,]
    N <- nrow(testSet)
    nF<- if(Threshold < 1) as.integer(Threshold*N) else Threshold
    cm <- table(c(rep('fraud',nF), rep('ok',N-nF)), ordTS$Insp)
    prec <- cm['fraud','fraud']/sum(cm['fraud',])
    rec <- cm['fraud','fraud']/sum(cm[,'fraud'])
    AVGndtp <- avgNDTP(ordTS[nF,],stats=statsProds)
    return(c(Precision=prec, Recall=rec,avgNdtp=AVGndtp))
}