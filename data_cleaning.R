# install.packages("stringr")

library(data.table)
library(stringr)
install.packages("ggplot2")
library(ggplot2)
remove.packages(c("ggplot2", "data.table"))
install.packages('Rcpp', dependencies = TRUE)
install.packages('ggplot2', dependencies = TRUE)
install.packages('data.table', dependencies = TRUE)
get(load('apt_item_sales_dt.Rdata'))
head(item)
# rm(list=ls())
# price <- as.character(apt_data[,1])%>%str_trim()%>%sub(",","",.)%>%as.numeric()

apt_data<-data.table(item)

colnames(apt_data) <- c('price','con_year','year','dong','aptnm','month','dat','area','bungi','loc','floor')
# apt_data$price_num <- NULL (특정행 삭제)
apt_data[,price:=as.character(price)%>%str_trim()%>%sub(",","",.)%>%as.numeric()]
apt_data[,con_year:=as.character(con_year)%>%str_trim()%>%as.numeric()]
apt_data[,year:=as.character(year)%>%str_trim()%>%as.numeric()]
apt_data[between(as.numeric(as.character(month)),1,3),qrt:='Q1']
apt_data[between(as.numeric(as.character(month)),4,6),qrt:='Q2']
apt_data[between(as.numeric(as.character(month)),7,9),qrt:='Q3']
apt_data[between(as.numeric(as.character(month)),10,12),qrt:='Q4']
apt_data[,dong:=as.character(dong)%>%str_trim()]
apt_data[,yyyyqrt:=paste0(year,qrt)]
apt_data[,month:=as.character(month)%>%str_trim()%>%as.numeric()]
apt_data[,yyyym:=paste0(year,month)]
#서울 지역 아파트 매매가격 추이, 매매가격 예측이 가능한가?

# 먼저 서울 지역 분기별 아파트 매매가격 변화

ggplot(apt_data,aes(x=yyyyqrt,y=price))+
  geom_boxplot(aes(fill=yyyyqrt),outlier.size=0.5) +
  xlab("2016년 분기별")+
  ylab("아파트매매가격")
  


# 서울 특정 동 분기별 아파트 매매 가격

apt_data_by_dong <- aggregate(apt_data$price, apt_data[,c("yyyym","dong")], mean)
apt_data_by_gu <- aggregate(apt_data$price, apt_data[,c("yyyym","loc")], mean)

unique(apt_data_by_gu$loc)
names(apt_data_by_dong) <- c('yyyym','dong','pricemean')
names(apt_data_by_gu) <- c('yyyym','gu','pricemean')


ggplot(apt_data_by_gu,aes(x=yyyym,y=pricemean))+
  geom_point()+
  facet_grid(gu~.)+
  geom_smooth(method="lm")+
  xlab("2016년 분기별")+
  ylab("아파트평균매매가격")

#지역별구 매매가 평균
#편차가 가장 큰 곳
#동별 평균 매매가 추이
#이상치는 어떻하지
#
