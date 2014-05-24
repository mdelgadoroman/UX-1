myTrainingFunction<-function(datos,semilla){
	str(datos)
	set.seed(semilla)
	ind<-sample(2,nrow(datos),replace=TRUE,prob=c(0.7,0.3))
	trainData<-datos[ind==1,]
	testData<-datos[ind==2,]
	library(party)
	myFormula<-Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width
	datos_ctree<-ctree(myFormula, data=trainData)
	table(predict(datos_ctree),trainData$Species)
	print(datos_ctree)
	plot(datos_ctree)
	testPred<-predict(datos_ctree,newdata = testData)
	table(testPred, testData$Species)
}