#Data frame to store the CUSUM values for each year
cusum_df<-data.frame(matrix(nrow=nrow(temps),ncol=length(1996:2015)+1))
#Assign columns names to CUSUM data frame
colnames(cusum_df)<-colnames(temps[,1:21])
# converting to Date 
print(cusum_df$DAY<-temps$DAY)

#using Zero start method ,calculating cusum values for each year
for(y in 2:ncol(cusum_df)){
  cusum_df[1,y]<-0 
  mu<-mean(temps[,y]) 
  std<-sd(temps[,y]) 
  threshold<-5*std 
  change<-NULL 
  
  for(i in 2:nrow(cusum_df)){
    cusum_df[i,y]<-max(0,cusum_df[i-1,y]+(mu-temps[i,y]-std))
    if (cusum_df[i,y]>=threshold){
      change<-append(change,cusum_df[i,y])}}
  cat("In year of",colnames(cusum_df[y]),"the day Summer actually started is:",
      cusum_df[which(cusum_df[,y]==change[1]),"DAY"],"\n")
}