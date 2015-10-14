## Load data
require(DMwR)
data(sales)
head(sales)
summary(sales)
nlevels(sales$ID)
nlevels(sales$Prod)
sum(is.na(sales$Quant)& is.na(sales$Val))
table(sales$Insp)/nrow(sales)*100

totS <- table(sales$ID)
totP <- table(sales$Prod)
par(mfcol = c(1,2))
barplot(totS, main="Transactions per salespeople", name.arg='',
        xlab='Salespeople',ylab='Amount')
barplot(totP, main='Transactions per product', names.arg='',
        xlab='Products',ylab='Amount')

sales$Uprice <- sales$Val/sales$Quant
summary(sales$Uprice)

attach(sales)
upp <- aggregate(Uprice,list(Prod),median,na.rm=T)
topP <- sapply(c(T,F),function(o)
    upp[order(upp[,2],decreasing=o)[1:5],1])
colnames(topP)<-c('Expensive','Cheap')
topP
tops <- sales[Prod %in% topP[1,],c('Prod','Uprice')]
tops$Prod <- factor(tops$Prod)
par(mfcol = c(1,2))
boxplot(Uprice~Prod, data=tops,ylab='Uprice',log='y')

vs <- aggregate(Val,list(ID),sum,na.rm=T)
scoresSs <- sapply(c(T,F),function(o)
    vs[order(vs$x,decreasing=o)[1:5],1])
colnames(scoresSs)<-c('Most','Least')
scoresSs
scores <- sales[ID %in% scoresSs[1,],c('ID','Val')]
scores$ID <- factor(scores$ID)
boxplot(Val~ID,data=scores,ylab='Val',log='y')

sum(vs[order(vs$x,decreasing=T)[1:100],2])/sum(Val,na.rm=T)*100
sum(vs[order(vs$x,decreasing=F)[1:2000],2])/sum(Val,na.rm=T)*100

qs <- aggregate(Quant,list(Prod),sum,na.rm=T)
scoresPs <- sapply(c(T,F),function(o)
    qs[order(qs$x,decreasing=o)[1:5],1])
colnames(scoresPs) <- c('Most','Least')
scoresPs
sum(as.double(qs[order(qs$x,decreasing=T)[1:100],2]))/sum(as.double(Quant),na.rm=T)*100
sum(as.double(qs[order(qs$x,decreasing=F)[1:4000],2]))/sum(as.double(Quant),na.rm=T)*100


out <- tapply(Uprice,list(Prod=Prod),function(x)length(boxplot.stats(x)$out))
out[order(out,decreasing = T)[1:10]]
sum(out)
sum(out)/nrow(sales)*100
