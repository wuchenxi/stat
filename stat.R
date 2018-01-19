library(data.table)
library(outliers)
library(pcaMethods)
X=as.matrix(fread("data.csv"))
ns=nrow(X)

###outlier removal
Y=rm.outlier(X)
while(is.null(nrow(Y))==0 && nrow(Y)<ns){
    X=Y
    ns=nrow(X)
    Y=rm.outlier(X)
}
print(X)
X=rbind(X[,1:13],X[,14:26],X[,27:39])

###pca biplots
res=pca(X, nPcs=6)
labs=c(rep("·",nrow(X)/3),rep("+",nrow(X)/3),rep("x",nrow(X)/3))
biplot(res, xlabs=labs, ylabs = rep("·", ncol(X)))
plotPcs(res,sl=labs)

###hca
Y=t(X)
colnames(Y)=1:ncol(Y)
o=data.frame(Factor=factor(labs),row.names=colnames(Y))
hca.plot(Y,o)

