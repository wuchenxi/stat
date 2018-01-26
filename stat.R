library(data.table)
X=as.matrix(fread("data.csv"))
ns=nrow(X)

###outlier removal
library(outliers)
Y=rm.outlier(X)
while(is.null(nrow(Y))==0 && nrow(Y)<ns){
    X=Y
    ns=nrow(X)
    Y=rm.outlier(X,fill=TRUE)
}
###print(X)
X=rbind(X[,1:13],X[,14:26],X[,27:39])

###pca biplots
library(pcaMethods)
res=pca(X, method="bpca", nPcs=6)
labs=c(rep("·",nrow(X)/3),rep("+",nrow(X)/3),rep("x",nrow(X)/3))
plotPcs(res,sl=labs,cex=0.5)

###hca
library(swamp)
Y=t(X)
colnames(Y)=1:ncol(Y)
o=data.frame(Factor=factor(labs),row.names=colnames(Y))
hca.plot(Y,o)

###similarity tree in 3 circumstances
library(ape)
X1=X[1:(nrow(X)/3),]
X2=X[(nrow(X)/3+1):(nrow(X)/3*2),]
X3=X[(nrow(X)/3*2+1):(nrow(X)),]
D1=dist(X1)
D2=dist(X2)
D3=dist(X3)
plot(as.phylo(hclust(D1)),type="radial",cex=0.2)
plot(as.phylo(hclust(D2)),type="radial",cex=0.2)
plot(as.phylo(hclust(D3)),type="radial",cex=0.2)
library(vegan)
print(mantel(D1,D2))
print(mantel(D1,D3))

###SVM
library(e1071)
train=sample(1:nrow(X),nrow(X)/2)
m=svm(X[train,],(factor(labs))[train],kernel="linear")
print(m)
pred=predict(m,X[-train,])
table(pred,(factor(labs))[-train])

###ANOVA
Xg=X
for(i in 1:(nrow(X)/3)){
    Xg[i,]=(X[i,]+X[i+nrow(X)/3,]+X[i+nrow(X)/3*2])/3
    Xg[i+nrow(X)/3,]=Xg[i,]
    Xg[i+nrow(X)/3*2,]=Xg[i,]
}
Xe=X
for(i in 1:(nrow(X)/3))
    Xe[i,]=colMeans(X1)
for(i in (nrow(X)/3+1):(nrow(X)/3*2))
    Xe[i,]=colMeans(X2)
for(i in (nrow(X)/3*2+1):nrow(X))
    Xe[i,]=colMeans(X3)
colvars=colSums((X-colMeans(X))^2)/(dim(X)[1]-1)
colvarsg=colSums((X-Xe)^2)/(dim(X)[1]-1)
colvarse=colSums((X-Xg)^2)/(dim(X)[1]-1)
print(colvarsg/colvars)
print(colvarse/colvars)

###mixed model
library(lme4)
fe=factor(labs)
fg=factor(c(1:(nrow(X)/3),1:(nrow(X)/3),1:(nrow(X)/3)))
for(i in 1:13){
    res=glmer(X[,i]~1+(1|fe)+(1|fg))
    print(res)}

###Phenotype correlation
library(psych)
options(scipen=999,nsmall=6)
heatmap(corr.test(X)$p)
