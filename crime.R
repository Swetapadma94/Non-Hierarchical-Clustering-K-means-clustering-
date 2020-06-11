crime<-read.csv("E:\\Assignment\\clustering\\crime_data.csv")
View(crime)
normaliized_data<-scale(crime[,2:5])
View(normaliized_data)
d<-dist(normaliized_data,method="euclidean")
fit<-hclust(d, method = "complete")
plot(fit)
plot(fit, hang=-1)
rect.hclust(fit, k=4, border = "blue")
groups <- cutree(fit, k=4)
View(groups)
#membership<-as.matrix(groups)# groups or cluster numbers
#View(membership)
final <- data.frame(crime,groups)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(crime[,-1],by=list(final$groups),mean)
