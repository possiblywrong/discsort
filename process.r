library(ggplot2)
options(scipen=999)
ns <- c(100,1000,10000,100000)
vals <- data.frame(Time=numeric(0), Sort=character(0), N=numeric(0))
for( i in ns ){
   print(i)
   vals <- rbind(vals,data.frame(Time=median(read.csv(paste0("~/tmp/dsort_",i,".data"),header=FALSE)$V1),Sort="discrimination",N=i))
   vals <- rbind(vals,data.frame(Time=median(read.csv(paste0("~/tmp/dsort_cmp_",i,".data"),header=FALSE)$V1),Sort="compare",N=i))
}
ggplot(data=vals, aes(x=N, y=Time, group=Sort, colour=Sort)) + geom_line() + geom_point()
