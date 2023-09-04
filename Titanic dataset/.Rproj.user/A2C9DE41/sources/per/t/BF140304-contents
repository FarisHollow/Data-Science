dataset<-read.csv("Dataset_midterm_Section(B).csv")
print(dataset)

xdataset <- dataset
print(xdataset)

is.na(xdataset)
sum(is.na(xdataset))

xdataset$Gender[is.na(xdataset$Gender)]<-mean(xdataset$Gender,na.rm=TRUE)
xdataset$age[is.na(xdataset$age)]<-mean(xdataset$age,na.rm=TRUE)
xdataset$sibsp[is.na(xdataset$sibsp)]<-mean(xdataset$sibsp,na.rm=TRUE)
xdataset$parch[is.na(xdataset$parch)]<-mean(xdataset$parch,na.rm=TRUE)
xdataset$fare[is.na(xdataset$fare)]<-mean(xdataset$fare,na.rm=TRUE)
xdataset$alone[is.na(xdataset$alone)]<-mean(xdataset$alone,na.rm=TRUE)
xdataset$survived[is.na(xdataset$survived)]<-mean(xdataset$survived,na.rm=TRUE)

print(xdataset)

xdataset$Gender <- as.integer(xdataset$Gender)
xdataset$age <- as.integer(xdataset$age)
xdataset$fare <- as.integer(xdataset$fare)

print(xdataset)

xdataset$class <- ifelse(xdataset$class == "Third",3,
                         ifelse(xdataset$class == "Second",2,
                                ifelse(xdataset$class == "First",1,NA)))

xdataset$class[is.na(xdataset$class)]<-mean(xdataset$class,na.rm=TRUE)
xdataset$class <- as.integer(xdataset$class)
print(xdataset)

xdataset$class <- ifelse(xdataset$class == 3,"Third",
                         ifelse(xdataset$class == 2,"Second",
                                ifelse(xdataset$class == 1,"First",NA)))
print(xdataset)

xdataset$alone <- ifelse(xdataset$alone == 1,"TRUE",
                         ifelse(xdataset$alone == 0,"FALSE",NA))

print(xdataset)

if (!require("ggplot2")) {
  install.packages("ggplot2")
}

library(ggplot2)

ggplot(data = xdataset, aes(x = age)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "darkblue") +
  labs(title = "Histogram of Passenger Ages", x = "Age", y = "Frequency")