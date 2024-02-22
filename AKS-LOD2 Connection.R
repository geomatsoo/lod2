# This code was written according to Google Styleguide https://google.github.io/styleguide/Rguide.xml
# Copyright: Matthias Soot
# Author: Matthias Soot
# Main file for derive inside building metrics from LOD2 Models by non-parametric Random Forest Regression and parametric Regression analysis
# Version: 0.1

# Programmierungscode fuer eingelesene Daten aus AKS
# date: 13.11.2022
# Erstellung erfolgte auf Betriebsystem: macOS Monterey, Version 12.5.1
# Zeichenkodierung UTF-8
# Erstellt mit R version 4.1.2 (2021-11-01) -- "Bird Hippie"
# Erstellt in RStudio 2022.07.1 

#driver for connection to postgres database
library("RPostgres")
#data handling and database data handling
library("dplyr")
library("dbplyr")
# Library for random forest
library(randomForest)


####################################### Laden der Daten aus der Datenbank #######################################

# create a connection
# save the password that we can "hide" it as best as we can by collapsing it
pw <- "!$MSOM2+3?"
################ Programmablauf ###############################
    
    # loads the PostgreSQL driver
    #drv <- dbDriver("RPostgreSQL")
    # creates a connection to the postgres database
    # note that "con" will be used later in each connection to the database
    con <- DBI::dbConnect(RPostgres::Postgres(), dbname = "aks-oga",
                          host = "localhost", port=5432,
                          user = "ben-oga",
                          password = pw
    )
    
   
#Joining Purchase Price Database information with LOD2 Modell information by adress
#"select wbb.wofl, wbb.vges, wbb.kgesun,wbb.bauj, wbb.koorrw, wbb.koorhw, wbb.datu, wbb.grua, wbb.geba, lod.\"VOLU\", lod.\"GRUF\",lod.\"AH\", lod.\"MTH\",lod.\"FH\",lod.\"RT\",lod.\"HO\", lod.\"id\", wbb.kfkz, wbb.gede, wbb.labe, wbb.haus  from wbb inner join lod ON gede= \"GEME\" and wbb.labe = \"LABE\" and wbb.haus = \"HAUS\";"
Datensatz <- dbGetQuery(con, statement= "select wbb.\"WOFL\", wbb.\"RTYP\", wbb.\"DGESAU\", wbb.\"VGES\", wbb.\"KGESUN\",wbb.\"BAUJ\", wbb.\"KOORRW\", wbb.\"KOORHW\", wbb.\"DATU\", wbb.\"GRUA\", wbb.\"GEBA\", lod.volu, lod.gruf,lod.ah, lod.mth,lod.fh,lod.rt,lod.ho, lod.id, lod.fu, wbb.\"KFKZ\", wbb.\"GEDE\", wbb.\"LABE\", wbb.\"HAUS\"  from wbb inner join lod ON wbb.\"GEDE\"= lod.geme and wbb.\"LABE\" = lod.labe and wbb.\"HAUS\"=lod.haus;")
    
### Select useful cases
#complete cases
LOD2wofl <- Datensatz[complete.cases(Datensatz[c(1:6,10:19,20)]),]
#cases with residential use
LOD2wofl <- LOD2wofl[LOD2wofl$fu=="31001_1000",]
#One Family houses
LOD2wofl <- LOD2wofl[LOD2wofl$GEBA==101,]
#Number of floors maximum 4 minimum 0
LOD2wofl <- LOD2wofl[LOD2wofl$VGES<4 & LOD2wofl$VGES>=0,]
#Maximum Volume smaller 5000 kubic meters and ground surface < 300 sqmare meter 
LOD2wofl <- LOD2wofl[LOD2wofl$volu<5000 & LOD2wofl$gruf<300,]
# Maximum heigth minimum 2 Meters over ground
LOD2wofl <- LOD2wofl[LOD2wofl$ho>2,]
# Living space maximum 350 sqm, minimum 40 sqm
LOD2wofl <- LOD2wofl[LOD2wofl$WOFL<350,]
LOD2wofl <- LOD2wofl[LOD2wofl$WOFL>40,]
# Year of construction after 1800
LOD2wofl <- LOD2wofl[LOD2wofl$BAUJ>1800,]
LOD2wofl <- LOD2wofl[LOD2wofl$BAUJ<2020,]
### formating and deriving further information 
LOD2wofl$vges <- as.ordered(LOD2wofl$VGES)
# Height from absolut heigth and ground heigth
LOD2wofl$hohe <- LOD2wofl$fh - LOD2wofl$ah
# Heigth of walls from the ground heigth and average 
LOD2wofl$hohemauer <- LOD2wofl$mth - LOD2wofl$ah
# rooftype as factor
LOD2wofl$RTfact <- as.factor(LOD2wofl$rt)
# Roof heigth difference of complete heigtha and wall heigth
LOD2wofl$DACH <- LOD2wofl$hohe - LOD2wofl$hohemauer
# Erase cases where construction year is unknown
# 
LOD2wofl <- LOD2wofl[LOD2wofl$DGESAU<3,]




### Stats
#number of floors vs. usable rooftop rooms
table(LOD2wofl$VGES, LOD2wofl$DGESAU)


### Visualisation
# Height of buildings compared to number of floors
plot(LOD2wofl$ho, LOD2wofl$VGES, xlab="Complete heigth in meters", ylab="Number of complete floors", col=(LOD2wofl$DGESAU+1))
#from calculated heigth
plot(LOD2wofl$hohe, LOD2wofl$VGES, xlab="Complete (calculated) heigth in meters ", ylab="Number of complete floors", col=(LOD2wofl$DGESAU+1))

# Heigth of the roof and number of floors
plot(LOD2wofl$DACH, LOD2wofl$VGES + LOD2wofl$DGESAU, xlab="Differenc between av. eaves heigth and ridge height in m", ylab="Number of complete floors", col=(LOD2wofl$DGESAU+1))
legend("topright", c("1","2","3"), fill=(LOD2wofl$DGESAU))

# Living space and Volume
rbPal <- colorRampPalette(c('red','green'))
colorierung <- rbPal(42)[as.numeric(cut(LOD2wofl$gruf, seq(min(LOD2wofl$gruf), max(LOD2wofl$gruf), by = 10)))]
plot(LOD2wofl$WOFL,LOD2wofl$volu, xlab="Wohnfläche", ylab="Volumen", col=colorierung)



# ground space and volume
plot(LOD2wofl$gruf,LOD2wofl$volu, xlab="Grundfläche", ylab="Volumen")



#living space and height 
rbPal <- colorRampPalette(c('red','green'))
colorierung2 <- rbPal(44)[as.numeric(cut(LOD2wofl$hohe, seq(min(LOD2wofl$hohe), max(LOD2wofl$hohe), by = 0.5)))]
plot(LOD2wofl$WOFL,LOD2wofl$volu, xlab="Wohnfläche", ylab="Volumen", col=colorierung2)


rbPal <- colorRampPalette(c('red','green'))
colorierung2 <- rbPal(4)[as.numeric(cut((LOD2wofl$VGES + LOD2wofl$DGESAU), seq(min((LOD2wofl$VGES + LOD2wofl$DGESAU)), max((LOD2wofl$VGES + LOD2wofl$DGESAU)), by = 1)))]
plot(LOD2wofl$volu,LOD2wofl$hohe, xlab="Wohnfläche", ylab="Höhe", col=colorierung2, pch=20,cex=0.5 )

plot((LOD2wofl$gruf*(LOD2wofl$VGES + LOD2wofl$DGESAU)),LOD2wofl$volu, xlab="Grundfläche * Vollgeschoss", ylab="Volumen")



hist(LOD2wofl$ho)
sort(LOD2wofl$ho, decreasing = TRUE)[1:4]
LOD2wofl[which(LOD2wofl$ho == sort(LOD2wofl$ho, decreasing = TRUE)[2]),]

aggregate(LOD2wofl$ho, list(LOD2wofl$VGES), mean)
aggregate(LOD2wofl$ho, list(LOD2wofl$VGES), sd)

aggregate(LOD2wofl$hohemauer, list(LOD2wofl$VGES), mean)
aggregate(LOD2wofl$hohemauer, list(LOD2wofl$VGES), sd)


plot(LOD2wofl$hohemauer, LOD2wofl$VGES)
hist(LOD2wofl$hohemauer)
min(LOD2wofl$hohemauer)

plot(LOD2wofl$DACH, LOD2wofl$DGESAU, col=LOD2wofl$rt)

### Evaluation 
## Create test and training sample
ita <- 1
sasi <- nrow(LOD2wofl)/10
sp <- sample(rep(1:10,times=c(sasi,sasi,sasi,sasi,sasi,sasi,sasi,sasi,sasi,sasi)))
splitsets <- split(LOD2wofl, sp)
#create copy of dataset
splitsetstmp <- splitsets
#erase one out of 10 parts
splitsetstmp[ita] <- NULL
#combine the remaining parts for training
training_set  <- dplyr::bind_rows((splitsetstmp))
test_set <- splitsets[[ita]]

### Train the random Forests

classifierWofl = randomForest(x = training_set[c("volu","gruf","RTfact","hohe","hohemauer","DACH")],
                          y = training_set$WOFL,
                          ntree = 200, random_state = 0, mtyr=3, type="regression", keep.forest=TRUE)
plot(classifierWofl, main="Error vs. trees - \n Livingspace vs. LOD info")
importanceWofl <- classifierWofl$importance

classifierVges = randomForest(x = training_set[c("volu","gruf","RTfact","hohe","hohemauer","DACH")],
                           y = training_set$VGES,
                           ntree = 200, random_state = 0, mtyr=3)
plot(classifierVges, main="Error vs. trees - \n Livingspace vs. LOD info")
importanceZawo <- classifierVges$importance

# including Year of construction

classifierWoflBauj = randomForest(x = training_set[c("BAUJ","volu","gruf","RTfact","hohe","hohemauer","DACH")],
                           y = training_set$WOFL,
                           ntree = 200, random_state = 0, mtyr=3)
plot(classifierWoflBauj, main="Error vs. trees - \n Livingspace LOD info + Year of Construction")
importanceWoflBauj <- classifierWoflBauj$importance

classifierVgesBauj = randomForest(x = training_set[c("BAUJ","volu","gruf","RTfact","hohe","hohemauer","DACH")],
                           y = training_set$VGES,
                           ntree = 200, random_state = 0, mtyr=3)
plot(classifierVgesBauj, main="Error vs. trees - \n noFloors LOD info + Year of Construction")
importanceWoflVges <- classifierVgesBauj$importance

### Including Region Type
# Livingspace - Including Region Type
classifierWoflBaujRegion = randomForest(x = training_set[c("RTYP","BAUJ","volu","gruf","RTfact","hohe","hohemauer","DACH")],
                               y = training_set$WOFL,
                               ntree = 200, random_state = 0, mtyr=3)
plot(classifierWoflBaujRegion, main="Error vs. trees - \n Livingspace LOD info + Year of Construction + Region")
importanceWoflRegion <- classifierWoflBaujRegion$importance

# Number of floors - Including Region Type
classifierVgesBaujRegion = randomForest(x = training_set[c("RTYP","BAUJ","volu","gruf","RTfact","hohe","hohemauer","DACH")],
                               y = training_set$VGES,
                               ntree = 200, random_state = 0, mtyr=3)
#number of trees vs. accuracy
plot(classifierVgesBaujRegion, main="Error vs. trees - \n noFloors LOD info + Year of Construction + Region ")
#importance of parameters
importanceVgesRegion <- classifierVgesBaujRegion$importance






plot(classifier2)
plot(classifier3bauj)
plot(classifier4bauj)
cor(training_set[c("WOFL","volu","gruf","hohe","hohemauer","dach")])
hist(LOD2wofl$WOFL, xlab = "Net internal area", main="")
plot(LOD2wofl$VGES, xlab = "Number of storeys", main="")


classifier1$


yPred1 = predict(classifier1, newdata = test_set[c("volu","gruf","RTfact","hohe","hohemauer","DACH")])
yPred1bauj = predict(classifier3bauj, newdata = test_set[c("BAUJ","volu","gruf","RTfact","hohe","hohemauer","DACH")])
yPred1BaujRtyp = predict(classifierWoflBaujRegion, newdata = test_set[c("RTYP","BAUJ","volu","gruf","RTfact","hohe","hohemauer","DACH")])


#Building year yes and no
#hist(y_pred1-test_set$wofl, main="ohne Baujahr", ylim=c(0,12000) )
#hist(y_pred1bauj-test_set$wofl, main="mit Baujahr",ylim=c(0,12000))
Metrics::rmse(yPred1,test_set$WOFL)
Metrics::rmse(yPred1bauj,test_set$WOFL)
Metrics::rmse(yPred1BaujRtyp,test_set$WOFL)
Metrics::mae(yPred1,test_set$WOFL)
Metrics::mae(yPred1bauj,test_set$WOFL)
Metrics::mae(yPred1BaujRtyp,test_set$WOFL)
Metrics::mape(yPred1,test_set$WOFL)
Metrics::mape(yPred1bauj,test_set$WOFL)
Metrics::mape(yPred1BaujRtyp,test_set$WOFL)



hist(yPred1-test_set$WOFL, main="Random Forest ohne Baujahr", xlab="Abweichungen in qm")
hist(mean(test_set$WOFL)-test_set$WOFL, main="Abweichungen zum Mittelwert", xlab="Abweichungen in qm")
sd(test_set$WOFL)
hist(yPred1bauj-test_set$WOFL, main="Random Forest ohne Baujahr", xlab="Abweichungen in qm")


signifikantunterschied <- wilcox.test(x=abs(y_pred1-test_set$wofl),y=abs(y_pred1bauj-test_set$wofl), var.equal=TRUE, alternative="two.sided")
hist(abs(y_pred1-test_set$wofl), xlab = "Absolute difference in sqm", main="Excluding building year", ylim=c(0,20000), xlim = c(0,150))
abline(v=Metrics::rmse(y_pred1,test_set$wofl),col="blue", lwd=2)
abline(v=Metrics::mae(y_pred1,test_set$wofl),col="red", lwd=2)
hist(abs(y_pred1bauj-test_set$wofl),xlab = "Absolute difference in sqm", main="Including building year", ylim=c(0,20000), xlim = c(0,150))
abline(v=Metrics::rmse(y_pred1bauj,test_set$wofl),col="blue", lwd=2)
abline(v=Metrics::mae(y_pred1bauj,test_set$wofl),col="red", lwd=2)
paste("The p-value of the significance test is", round(signifikantunterschied$p.value,10))
hist(test_set$wofl, main="Deviation of living space in test sample", xlab="Living space in sqm")
sd(test_set$wofl)


y_pred2 = predict(classifier2, newdata = test_set[c("VOLU","GRUF","RTfact","hohe","hohemauer","DACH")])
y_pred1bauj4 = predict(classifier4bauj, newdata = test_set[c("bauj","VOLU","GRUF","RTfact","hohe","hohemauer","DACH")])

#Building year yes and no
#hist(y_pred1-test_set$vges, main="ohne Baujahr", ylim=c(0,12000) )
#hist(y_pred1bauj-test_set$vges, main="mit Baujahr",ylim=c(0,12000))
Metrics::rmse(as.numeric(y_pred2),as.numeric(test_set$vges))
Metrics::rmse(as.numeric(y_pred1bauj4),as.numeric(test_set$vges))
Metrics::mae(as.numeric(y_pred2),as.numeric(test_set$vges))
Metrics::mae(as.numeric(y_pred1bauj4),as.numeric(test_set$vges))
Metrics::mape(as.numeric(y_pred2),as.numeric(test_set$vges))
Metrics::mape(as.numeric(y_pred1bauj4),as.numeric(test_set$vges))
signifikantunterschied <- wilcox.test(x=abs(y_pred2-test_set$vges),y=abs(y_pred1bauj4-test_set$vges), var.equal=TRUE, alternative="two.sided")
hist(abs(as.numeric(y_pred2)-as.numeric(test_set$vges)), xlab = "Absolute difference in floors", main="Excluding building year", ylim=c(0,12000), xlim = c(0,3))
abline(v=Metrics::rmse(as.numeric(y_pred2),as.numeric(test_set$vges)),col="blue", lwd=2)
abline(v=Metrics::rmse(as.numeric(y_pred2),as.numeric(test_set$vges)),col="red", lwd=2)
hist(abs(as.numeric(y_pred1bauj4)-as.numeric(test_set$vges)),xlab = "Absolute difference in floors", main="Including building year", ylim=c(0,12000), xlim = c(0,3))
abline(v=Metrics::rmse(as.numeric(y_pred1bauj4),as.numeric(test_set$vges)),col="blue", lwd=2)
abline(v=Metrics::rmse(as.numeric(y_pred1bauj4),as.numeric(test_set$vges)),col="red", lwd=2)
paste("The p-value of the significance test is", round(signifikantunterschied$p.value,10))
hist(as.numeric(test_set$vges), main="Deviation of living space in test sample", xlab="Living space in sqm", ylim=c(0,12000), xlim = c(0,3))
sd(as.numeric(test_set$vges))
library(caret)

#Creates vectors having data points
expected_value <- test_set$vges
predicted_value <- y_pred2

#Creating confusion matrix
example <- confusionMatrix(data=predicted_value, reference = expected_value)
table(expected_value,predicted_value)






plot(test_set$wofl, test_set$VOLU, xlab="Living space", ylab="Volume")

clasiffierlm <- lm(wofl~GRUF+VOLU+hohe+hohemauer+RT, data=training_set)
ks.test(clasiffierlm$residuals,  y="pnorm", mean=mean(clasiffierlm$residuals),sd=sd(clasiffierlm$residuals))
hist(clasiffierlm$residuals)
summary(clasiffierlm)
y_predlm <- predict.lm(clasiffierlm, newdata=test_set)
Metrics::rmse(y_predlm,test_set$wofl)
Metrics::mae(y_predlm,test_set$wofl)
Metrics::mape(y_predlm,test_set$wofl)
hist(abs(y_predlm-test_set$wofl), xlab = "Absolute difference in sqm", main="Excluding building year", ylim=c(0,20000), xlim = c(0,300))
abline(v=Metrics::rmse(y_predlm,test_set$wofl),col="blue", lwd=2)
abline(v=max(y_predlm-test_set$wofl),col="red", lwd=2)

clasiffierlmb <- lm(wofl~GRUF+VOLU+hohe+hohemauer+RT+bauj, data=training_set)
summary(clasiffierlmb)
y_predlmb <- predict.lm(clasiffierlmb, newdata=test_set)
Metrics::rmse(y_predlmb,test_set$wofl)
Metrics::mae(y_predlmb,test_set$wofl)
Metrics::mape(y_predlmb,test_set$wofl)
hist(abs(y_predlmb-test_set$wofl), xlab = "Absolute difference in sqm", main="Including building year", ylim=c(0,20000), xlim = c(0,300))
abline(v=Metrics::rmse(y_predlmb,test_set$wofl),col="blue", lwd=2)
abline(v=max(y_predlmb-test_set$wofl),col="red", lwd=2)




y_pred2 = predict(classifier2, newdata = test_set[c("VOLU","GRUF","RT","hohe","hohemauer","DACH")])
y_pred2bauj = predict(classifier4bauj, newdata = test_set[c("BAUJ", "VOLU","GRUF","RT","hohe","hohemauer","DACH")])
plot(test_set$vges)
hist(as.numeric(y_pred2)-as.numeric(test_set$vges))
hist(as.numeric(y_pred2)-as.numeric(test_set$vges), main="ohne Baujahr")
hist(as.numeric(y_pred2bauj)-as.numeric(test_set$vges), main="mit Baujahr")

plot(test_set$vges, test_set$hohe)
plot(test_set$wofl, test_set$hohe)
plot(test_set$wofl, test_set$VOLU)
plot(test_set$wofl, test_set$GRUF)
plot(test_set$vges,test_set$hohemauer)




randomForest::randomForest(x=LOD2wofl[c(4,8:16),], y = LOD2wofl[c(1,2,3),], keep.forest=TRUE)

summary(lm(formula =LOD2wofl$GRUF ~LOD2wofl$wofl, data = LOD2wofl))

library(MultivariateRandomForest)
#build_forest_predict(trainX=training_set[,c("VOLU","GRUF","hohe","hohemauer","DACH")], trainY=training_set[,c("wofl","dgesau")], n_tree=200, 3,min_leaf = 100, testX=test_set[,c("VOLU","GRUF","hohe","hohemauer","DACH")])

LOD2wofltemp <- LOD2wofl
LOD2wofltemp$vges <- as.numeric(LOD2wofltemp$vges)

cor(LOD2wofltemp[,c("wofl","dgesau","vges")],method="kendall")

nn <- neuralnet(as.formula(wofl + vges + dgesau ~ VOLU+GRUF+hohe+hohemauer+DACH),data=training_set, hidden=c(3,2), err.fct="sse",
                linear.output=TRUE, rep=5)
