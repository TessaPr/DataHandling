setwd("\\\\soliscom.uu.nl/uu/Users/Pronk008/My Documents/onderwijs/DataHandlingR")
PD1 <- read.delim("PatientDATA1.txt", stringsAsFactors = FALSE )
PD2 <- read.delim("PatientDATA2.txt", stringsAsFactors = FALSE )
Pdata<-merge(PD1,PD2,by="PATNO",all=FALSE ) 
Pdata$VISIT<-as.Date(Pdata$VISIT,format="%d-%m-%Y")
Pdata$PATNO <-as.factor(Pdata$PATNO) 
Pdata$AE <-as.factor(Pdata$AE)
Pdata$DX <-as.factor(Pdata$DX) 
Frepl<-grep("F",Pdata$GENDER)
Pdata$GENDER[Frepl]<-"Female"
codes<-c("Male","Female")
D <- adist(Pdata$GENDER, codes)
colnames(D) <- codes
rownames(D) <- Pdata$GENDER
i <- apply(D, 1, which.min)
Pdata$GENDER<-codes[as.numeric(i)]
replaceNA<-which(Pdata$HR<40)
Pdata$HR[replaceNA]<-NA
sepcol<-strsplit(Pdata$SBP_DBP,"_")
New<-matrix(unlist(sepcol), nrow = 7, ncol = 2, byrow=TRUE)
colnames(New)<-c("SBP","DBP")
New <- apply(New,2,as.numeric)
Pdatanew<-cbind(Pdata,New)
Pdatanew<-Pdatanew[,!names(Pdatanew)=="SBP_DBP"]
Pdatanew[,"diff"] <- Pdatanew$DBP-Pdatanew$SBP
Pdatanew<-Pdatanew[order(Pdatanew$HR),]
Pdatanew<-na.omit(Pdatanew)
cols <- ifelse(Pdatanew$GENDER == "Male", "red","darkred")
par(mfrow=c(1,2))
x1<-barplot(Pdatanew$HR, col=cols,ylim=c(0,200),xlab="Patient number", ylab="Heart rate",names.arg=Pdatanew$PATNO) #change the shape to triangles
points(Pdatanew$SBP~x1, col=cols, pch=15)
points(Pdatanew$DBP~x1, col=cols, pch=16)
legend(1, 210, c("Male", "Female"), col = c("red","darkred"), pch = c(15,15), cex=0.5)
legend(0, 210, c("HR", "SBP","DBP"), col = "black", pch = c(15,11,10), cex=0.5)
Pdatanew$GENDER<-as.factor(Pdatanew$GENDER)
boxplot(Pdatanew$HR~Pdatanew$GENDER,col=c("darkred","red"),ylab="Heart rate")
