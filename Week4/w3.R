library(readr)
#crime_data <- read_tsv('H:/My Drive/Georgia Tech/Online Master of Science in Analytics/Courses/ISYE-6501/Week 3/uscrime.txt')
data <- as.matrix(crime_data)
crime <- data[,16]

#see basic information about crimes:
summary(crime)

hist(crime,xlab = "Crime",main = "Histogram of crime",breaks = sqrt(length(crime))) # set number of bins


library(outliers)

#Case1:-Identifying whether or not the maximum value is an outlier:
grubbs.test(crime)
#There is no sufficient evidence to say that the maximum value of 1993 is an outlier. (p-value > 0.05)

#Case2:-Identifying whether or not the minimum value is an outlier:
grubbs.test(crime, opposite = TRUE)
#There is no sufficient evidence to say that the minimum value of 342 is an outlier. (p-value > 0.05)

sort(crime)

#Case3:-Identifying whether or not the smallest and hightest values are outliers or not 
grubbs.test(crime, type=11)
#There is no enough evidence to determine that the values 342 and 1993 are outliers (p-value > 0.05)

#use another approach to find possible outliers:
boxplot(crime,
        ylab = "Crime"
)

boxplot.stats(crime)$out

out<-boxplot.stats(crime)$out
out_index <- which(crime %in% c(out))
out_index
crime_data[out_index,16 ]