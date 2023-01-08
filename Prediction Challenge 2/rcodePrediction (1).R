data1 = read.csv("~/Downloads/citybike.train.csv")
data2 = read.csv("~/Downloads/citybike.test20000.csv")
data1$Day <- as.numeric(substr(data1$starttime,9,10))
data1$Hour <- as.numeric(substr(data1$starttime,12,13))
data1$Minutes <- as.numeric(substr(data1$starttime,15,16))
data1$Second <- as.numeric(substr(data1$starttime,18, 19))
x2 = data1$start.station.latitude
y2= data1$end.station.longitude
x1= data1$end.station.latitude
y1= data1$start.station.longitude

xfin = (x2 - x1)^2
yfin = (y2 - y1)^2
distance = sqrt(xfin + yfin)
data1$distance = distance

mod1 = lm(tripduration~start.station.id + end.station.id + I(log(bikeid)) + Day + Hour + Minutes + Second + 
            start.station.latitude + end.station.latitude + start.station.longitude + end.station.longitude +
            Distance + usertype, data=data1)

data2$Day <- as.numeric(substr(data2$starttime,9,10))

data2$Hour <- as.numeric(substr(data2$starttime,12,13))

data2$Minutes <- as.numeric(substr(data2$starttime,15,16)
                            )
data2$Second <- as.numeric(substr(data2$starttime,18, 19))

x1 = data2$end.station.latitude

x2 = data2$start.station.latitude

y1 = data2$start.station.longitude
y2 = data2$end.station.longitude

x2fin = (x2 - x1)^2
y2fin = (y2 - y1)^2
distance = sqrt(xfin2+ yfin2)
data2$Distance = distance

prediction = predict(mod1, data2)
summary(mod1)
write.csv(prediction, file ="/test.csv")
data = read.csv("~/test.csv")
colnames(data) = c("Id", "Predicted")

write.csv(data, file="~/predictionsss.csv", row.names = FALSE)
}

create_model()