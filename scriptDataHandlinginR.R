setwd("\\\\soliscom.uu.nl/uu/Users/Pronk008/My Documents/onderwijs/DataHandlingR")
PD1<-read.delim("PatientDATA1.txt", stringsAsFactors = FALSE )
PD2<-read.delim("PatientDATA2.txt", stringsAsFactors = FALSE )
PD2$VISIT<-as.Date(PD2$VISIT,format="%d-%m-%Y")
diff(PD2$VISIT)
PD1$AE <-as.factor(PD1$AE)
PD2$DX <-as.factor(PD2$DX) 
Pdata<-merge(PD1,PD2,by="PATNO",all=FALSE )
Pdata_random<-Pdata[sample(1:nrow(Pdata), 3, replace=FALSE),] 
table(Pdata$AE)
table(Pdata$AE,Pdata$GENDER)
prop.table(table(Pdata$AE,Pdata$GENDER))
Pdata[order(Pdata$HR),]
sepcol<-strsplit(Pdata$SBP_DBP,"_")
Pdata_SBP_DBP<-do.call(rbind, sepcol)
colnames(Pdata_SBP_DBP)<-c("SBP","DBP")
Pdata_SBP_DBP<-apply(Pdata_SBP_DBP,2,as.numeric)
Pdatanew<-cbind(Pdata, Pdata_SBP_DBP)
Pdatanew<-Pdatanew[,!names(Pdatanew)=="SBP_DBP"]
Pdatanew[,"diff"]<-Pdatanew$DBP-Pdatanew$SBP
Frepl<-grep("F",Pdatanew$GENDER)
Pdatanew$GENDER_CLEAN = Pdatanew$GENDER
Pdatanew$GENDER_CLEAN[Frepl]<-"Female"
codes<-c("Male","Female")
D<-adist(Pdatanew$GENDER_CLEAN, codes)
colnames(D)<-codes
rownames(D)<-Pdatanew$GENDER
i<-apply(D, 1, which.min)
Pdatanew$GENDER_CLEAN<-codes[as.numeric(i)]
replaceNA<-which(Pdatanew$HR<40)
Pdatanew$HR_CLEAN<-Pdatanew$HR
Pdatanew$HR_CLEAN[replaceNA]<-NA
options(repr.plot.width=6, repr.plot.height=5)
par(mar=c(1,1,1,1))
plot(c(1:20),pch=1:20,col=1:20)
Pdatanew<-Pdatanew[order(Pdatanew$HR_CLEAN),]
Pdata_plot<-na.omit(Pdatanew)
cols<-ifelse(Pdata_plot$GENDER_CLEAN == "Male", "red","darkred")
x1<-barplot(Pdata_plot$HR, col=cols, ylim=c(0,200),xlab="Patient number", ylab="Heart rate",names.arg=Pdata_plot$PATNO) 
legend(0, 190, c("Male", "Female"), col = c("red","darkred"), pch = c(15,15))
Pdata_plot$GENDER_CLEAN<-as.factor(Pdata_plot$GENDER_CLEAN)
boxplot(Pdata_plot$HR~Pdata_plot$GENDER_CLEAN,col=c("darkred","red"),ylab="Heart rate")