## Normalized distance to typical unit price
## The modified box plot rule
BPrule <- function(train,test){
    notF <- which(train$Insp != 'fraud')
    ms <- tapply(train$Uprice[notF],list(Prod=train$Prod[notF]),
                 function(x){
                     bp <- boxplot.stats(x)$stats
                     c(median=bp[3],iqr=bp[4]-bp[2])
                 })
    ms <- matrix(unlist(ms), length(ms),2,byrow=T,
                 dimnames=list(names(ms),c('median','iqr')))
    ms[which(ms[,'iqr']==0),'iqr']<-ms[which(ms[,'iqr']==0),'median']
    ORscore <- abs(test$Uprice-ms[test$Prod,'median'])/ms[test$Prod,'iqr']
    return(list(rankOrder=order(ORscore,decreasing=T),rankScore=ORscore))
}

notF <- which(sales$Insp != 'fraud')
globalStats <- tapply(sales$Uprice[notF],list(Prod=sales$Prod[notF]),
                      function(x){
                          bp<-boxplot.stats(x)$stats
                          c(median=bp[3],iqr=bp[4]-bp[2])
                      })
globalStats<-matrix(unlist(globalStats),length(globalStats),2,byrow=T,
                    dimnames=list(names(globalStats),c('median','iqr')))
globalStats[which(globalStats[,'iqr']==0),'iqr']<-globalStats[which(globalStats[,'iqr']==0),'median']

## holdOut
ho.BPrule <- function(form,train,test,...){
    res<-BPrule(train,test)
    structure(evalOutlierRanking(test,res$rankOrder,...),
              itInfo=list(preds=res$rankScore,trues=ifelse(test$Insp=='fraud',1,0)
                          )
              )
    }
bp.res <- holdOut(learner('ho.BPrule',
                          pars=list(Threshold=0.1, statsProds=globalStats)),
                  dataset(Insp~.,sales),
                  hldSettings(3,0.3,1234,T),
                  itsInfo=T)
summary(bp.res)

par(mfrow=c(1,2))
info <- attr(bp.res,'itsInfo')
PTs.bp <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),c(1,3,2))
PRcurve(PTs.bp[,,1],PTs.bp[,,2],main='PR curve',avg='vertical')
CRchart(PTs.bp[,,1],PTs.bp[,,2],main='Cumulative Recall curve',avg='vertical')

## Local Outlier Factors
ho.LOF <-function(form, train,test,k,...){
    ntr<-nrow(train)
    all <- rbind(train,test)
    N<-nrow(all)
    ups<-split(all$Uprice,all$Prod)
    r<-list(length=ups)
    for(u in seq(along=ups))
        r[[u]]<-if(NROW(ups[[u]])>3)
            lofactor(ups[[u]],min(k,NROW(ups[[u]]) %/% 2))
    else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]]))
    else NULL
    all$lof <- vector(length=N)
    split(all$lof,all$Prod)<-r
    all$lof[which(!(is.infinite(all$lof)|is.nan(all$lof)))]<-
        SoftMax(all$lof[which(!(is.infinite(all$lof)|is.nan(all$lof)))])
    structure(evalOutlierRanking(test,order(all[(ntr+1):N,'lof'],decreasing=T),...),
              itInfo=list(preds=all[(ntr+1):N,'lof'],
                          trues=ifelse(test$Insp=='fraud',1,0)))
}
lof.res <- holdOut(learner('ho.LOF',pars=list(k=7, Threshold=0.1,statsProds=globalStats)),
                   dataset(Insp~.,sales),
                   hldSettings(3,0.3,1234,T),
                   itsInfo=T)
summary(lof.res)

## compare two method
gc()
png('PR_CR_curves.png')
par(mfrow=c(1,2))
info <- attr(lof.res,'itsInfo')
PTs.lof <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2))
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
legend('topright',c('BPrule','LOF'),lty=c(1,2))
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
legend('bottomright',c('BPrule','LOF'),lty=c(1,2))
dev.off()

## Clustering-Based Outlier Rankings
ho.ORh <- function(form,train,test,...){
    ntr<-nrow(train)
    all<-rbind(train,test)
    N<-nrow(all)
    ups<-split(all$Uprice,all$Prod)
    r<-list(length=ups)
    for(u in seq(along=ups))
        r[[u]]<-if(NROW(ups[[u]])>3)
            outliers.ranking(ups[[u]])$prob.outliers
    else if (NROW(ups[[u]])) rep(0,NROW(ups[[u]]))
    else NULL
    all$orh <- vector(length=N)
    split(all$orh,all$Prod)<-r
    all$orh[which(!(is.infinite(all$orh)|is.nan(all$orh)))]<-
        SoftMax(all$orh[which(!(is.infinite(all$orh)|is.nan(all$orh)))])
    structure(evalOutlierRanking(test,order(all[(ntr+1):N,'orh'],trues=ifelse(test$Insp=='fraud',1,0))))
}

orh.res <- holdOut(learner('ho.ORh',pars=list(Threshold=0.1, statsProds=globalStats)),
                   dataset(Insp~.,sales),
                   hldSettings(3,0.3,1234,T),
                   itsInfo=TRUE)
summary(orh.res)
## plot
par(mfrow=c(1,2))
info <- attr(orh.res,'itsInfo')
PTs.orh <- aperm(array(unlist(info),dim=c(length(info[[1]]),2,3)),
                 c(1,3,2))
PRcurve(PTs.bp[,,1],PTs.bp[,,2],
        main='PR curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
PRcurve(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
PRcurve(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=2,col='grey',
        avg='vertical')
legend('topright',c('BPrule','LOF','ORh'),lty=c(1,2,1),col=c('black','black','grey'))
CRchart(PTs.bp[,,1],PTs.bp[,,2],
        main='Cumulative Recall curve',lty=1,xlim=c(0,1),ylim=c(0,1),
        avg='vertical')
CRchart(PTs.lof[,,1],PTs.lof[,,2],
        add=T,lty=2,
        avg='vertical')
CRchart(PTs.orh[,,1],PTs.orh[,,2],
        add=T,lty=2,col='grey',
        avg='vertical')
legend('bottomright',c('BPrule','LOF','ORh'),lty=c(1,2,1),col=c('black','black','grey'))

