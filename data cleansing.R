totS <- table(ID)
totP <- table(Prod)
nas <- sales[which(is.na(Quant)&is.na(Val)),c('ID','Prod')]
propS <- 100*table(nas$ID)/totS
propS[order(propS,decreasing=T)[1:10]]

propP <- 100*table(nas$Prod)/totP
propP[order(propP,decreasing=T)[1:10]]

## join small groups together
detach(sales)
sales <- sales[-which(is.na(sales$Quant)&is.na(sales$Val)),]

## Percent of NA in VAL / QUANT by ID and PROD
nnasQp <- tapply(sales$Quant,list(sales$Prod),function(x)sum(is.na(x)))
propNAsQp <- nnasQp/table(sales$Prod)
propNAsQp[order(propNAsQp,decreasing=T)[1:10]]

sales <- sales[!sales$Prod %in% c('p2442','p2443'),]

nlevels(sales$Prod)
sales$Prod <- factor(sales$Prod)
nlevels(sales$Prod)

nnasQs <- tapply(sales$Quant,list(sales$ID),function(x)sum(is.na(x)))
propNAsQs <- nnasQs/table(sales$ID)
propNAsQs[order(propNAsQs,decreasing = T)[1:10]]

nnasVp <- tapply(sales$Val,list(sales$Prod),function(x)sum(is.na(x)))
propNAsVp<-nnasVp/table(sales$Prod)
propNAsVp[order(propNAsVp,decreasing = T)[1:10]]

nnasVs <- tapply(sales$Val,list(sales$ID),function(x)sum(is.na(x)))
propNAsVs<-nnasVs/table(sales$ID)
propNAsVs[order(propNAsVs,decreasing = T)[1:10]]

## Data imputation
tPrice <- tapply(sales[sales$Insp !='fraud','Uprice'],list(sales[sales$Insp != 'fraud','Prod']),median,na.rm=T)
noQuant <- which(is.na(sales$Quant))
sales[noQuant,'Quant'] <- ceiling(sales[noQuant,'Val']/tPrice[sales[noQuant,'Prod']])
noVal <- which(is.na(sales$Val))
sales[noVal,'Val']<-sales[noVal,'Quant']*tPrice[sales[noVal,'Prod']]
sales$Uprice <- sales$Val/sales$Quant
save(sales,file='salesClean.Rdata')

## Few transactions of some products
attach(sales)
notF <- which(Insp != 'fraud')
ms <- tapply(Uprice[notF],list(Prod=Prod[notF]),function(x){
    bp <- boxplot.stats(x)$stats
    c(median=bp[3],iqr=bp[4]-bp[2]) #boxplot.stats() to obtain median, first and third quartiles
})
ms <- matrix(unlist(ms),length(ms),2, byrow=T, dimnames=list(names(ms),c('median','iqr')))
# iqr - inter-quartile range
head(ms)
par(mfrow=c(1,2))
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='')
plot(ms[,1],ms[,2],xlab='Median',ylab='IQR',main='',col='grey',log='xy')
smalls <- which(table(Prod)<20)
points(log(ms[smalls,1]),log(ms[smalls,2]),pch='+')

dms <- scale(ms)
smalls <- which(table(Prod)<20)
prods <- tapply(sales$Uprice,sales$Prod,list)
similar <- matrix(NA,length(smalls),7,dimnames=list(names(smalls),c('Simil','ks.stat','ks.p','medP','iqrP','medS','iqrS')))
for (i in seq(along=smalls)){
    d<-scale(dms,dms[smalls[i],],FALSE)
    d<-sqrt(drop(d^2 %*% rep(1,ncol(d))))
    stat<-ks.test(prods[[smalls[i]]],prods[[order(d)[2]]]) 
    #Kolmogorov Smirnov Tests, to compare the two distributions of unit prices.
    similar[i,]<-c(order(d)[2],stat$statistic,stat$p.value,ms[smalls[i],],ms[order(d)[2],])
}
# save similar matrix
head(similar)
levels(Prod[similar[1,1]])
nrow(similar[similar[,'ks.p']>=0.9,])
sum(similar[,'ks.p']>=0.9)
save(similar, file='similarProducts.Rdata')


