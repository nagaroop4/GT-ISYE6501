#creating a data frame to store the CUSUM values for each year
cusum_df<-data.frame(matrix(nrow=nrow(temps),ncol=length(1996:2015)+1))
#assigning columns names to cusum data frame
colnames(cusum_df)<-colnames(temps[,1:21])
#tidying up the Day and converting it to Date for ease of processing
print(cusum_df$DAY<-temps$DAY)

#using Zero start method
#calculating cusum values for each year
for(y in 2:ncol(cusum_df)){
  cusum_df[1,y]<-0 #initial St value for each column,set to zero
  mu<-mean(temps[,y]) #mean of each sample space(each year's observations)
  std<-sd(temps[,y]) #sd of each sample,also used as allowable slack
  threshold<-5*std #using 5 sd as threshold value,different T for each year 
  change<-NULL # to store dates with St over threshold,first value:first day change detected
  
  for(i in 2:nrow(cusum_df)){
    cusum_df[i,y]<-max(0,cusum_df[i-1,y]+(mu-temps[i,y]-std))
    if (cusum_df[i,y]>=threshold){
      change<-append(change,cusum_df[i,y])}}
  cat("In year of",colnames(cusum_df[y]),"the day Summer started is:",
      cusum_df[which(cusum_df[,y]==change[1]),"DAY"],"\n")
}