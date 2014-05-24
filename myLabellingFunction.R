myLabellingFunction<-function(){
	data("bodyfat",package="mboost")
	dim(bodyfat)
	attributes(bodyfat)
	library(rpart)
	myFormula<-DEXfat ~ age + waistcirc + hipcirc + elbowbreadth + kneebreadth
	bodyfat_rpart <- rpart(myFormula, data=bodyfat, control=rpart.control(minsplit=10))
	print(bodyfat_rpart$cptable)
	print(bodyfat_rpart)
	plot(bodyfat_rpart)
	text(bodyfat_rpart, use.n=TRUE)
	opt<-which.min(bodyfat_rpart$cptable[,"xerror"])
	cp<-bodyfat_rpart$cptable[opt,"CP"]
	bodyfat_prune<-prune(bodyfat_rpart,cp=cp)
	print(bodyfat_prune)
	DEXfat_pred<-predict(bodyfat_prune,newdata=bodyfat)
	xlim<-range(bodyfat$DEXfat)
	plot(DEXfat_pred ~DEXfat, data=bodyfat, xlab="Observed", ylab="Predicted", ylim=xlim)
	abline(a=0,b=1)
	return (DEXfat_pred)
	
}