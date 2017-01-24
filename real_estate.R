#문제정의  : 1. 내가 살고 있는 집 혹은 우리 동네 관심을 갖고 있는 집값에 대한 예측
# 2.전체 매매, 전세 추이 및 내가 살고 있는 지역의 매매,전세 추이

install.packages("XML")
library(XML)
library(data.table)

#data 수집 과정 (data.go.kr에서 제공하는 API 사용)
service_key <- "EeBjN2xdCzzcqHvefO0rZXaycAim0uGpKxnOX72PY1UpkSZnifzIK1kxLm61XXaQ4pFxhbW%2F%2FZbmQDKFiAFNVA%3D%3D"
#서울시 지역코드
locCode <-c("11110","11140","11170","11200","11215","11230","11260","11290","11305","11320","11350","11380","11410","11440","11470","11500","11530","11545","11560","11590","11620","11650","11680","11710","11740")
datelist <-c("201601","201602","201603","201604","201605","201606","201607","201608","201609","201610","201611","201612")


url <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=11110&DEAL_YMD=201601&serviceKey=",service_key)

urllist <- list()
cnt <-0
for(i in 1:length(locCode)){
  for(j in 1:length(datelist)){
    cnt=cnt+1
    urllist[cnt] <-paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=",locCode[i],"&DEAL_YMD=",datelist[j],"&serviceKey=",service_key) 
  }
}
# raw.data <- xmlTreeParse(url, useInternalNodes = TRUE,encoding = "utf-8")

#item <- data.table()
total<-list()
for(i in 1:length(urllist)){
  item <- list()
  raw.data <- xmlTreeParse(urllist[i], useInternalNodes = TRUE,encoding = "utf-8")
  rootNode <- xmlRoot(raw.data)
  items <- rootNode[[2]][['items']]

  size <- xmlSize(items)
  for(i in 1:size){
     item_temp <- xmlSApply(items[[i]],xmlValue)
     item[[i]]<-list(t(item_temp))
     #item<-rbind(item,t(as.data.table(item_temp)),fill=TRUE)
  }
  total[[i]]<-rbindlist(item,fill = TRUE)
}
result_apt_data <- rbindlist(total,fill = TRUE)
#컬럼명
names(result_apt_data)<-c("거래금액","건축년도","년","법정동","아파트","월","일","전용면적","지번","지역코드","층")
save(result_apt_data, file="apt_item_sales_dt.Rdata")


library(stringr)
library(ggplot2)
load("apt_item_sales_dt.Rdata")

apt_data<-data.table(item)
apt_data<-data.table(result_apt_data)
colnames(apt_data) <- c('price','con_year','year','dong','aptnm','month','dat','area','bungi','loc','floor')


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

summary(apt_data)
str(apt_data)
head(apt_data)
#번지가 없는 데이터에 대한 처리
index_na <-is.na(apt_data$floor)
apt_data[index_na]$floor <- apt_data[index_na]$loc
apt_data[index_na]$loc <- apt_data[index_na]$bungi
apt_data[index_na]$bungi <- NA
apt_data[index_na]$floor

#서울 지역 아파트 매매가격 추이, 매매가격 예측이 가능한가?

# 먼저 서울 지역 분기별 아파트 매매가격 변화

ggplot(apt_data,aes(x=yyyyqrt,y=price))+
  geom_boxplot(aes(fill=yyyyqrt),outlier.size=0.5) +
  xlab("2016년 분기별")+
  ylab("아파트매매가격")



# 서울 특정 동 분기별 아파트 매매 가격

apt_data_by_dong <- aggregate(apt_data$price, apt_data[,c("yyyym","dong")], mean)
apt_data_by_gu_qrt <- aggregate(apt_data$price, apt_data[,c("yyyym","loc")], mean)

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
apt_data_by_gu_price <- aggregate(apt_data$price,apt_data[,c('loc')],mean)

#편차가 가장 큰 곳
#특정구 동별 평균 매매가 추이
# 일단 구별(loc)로 나눈 구별 테이블이 필요
dt<-list()
for(i in 1:length(locCode)){
  dt[[i]] <- subset(apt_data,loc==locCode[[i]])
}

# 동별 분기별 평균 매매가
data1<-as.data.table(dt[[1]])
data2<-as.data.table(dt[[2]])
data3<-as.data.table(dt[[3]])
data4<-as.data.table(dt[[4]])
data5<-as.data.table(dt[[5]])
data6<-as.data.table(dt[[6]])
data7<-as.data.table(dt[[7]])
data8<-as.data.table(dt[[8]])
data9<-as.data.table(dt[[9]])
data10<-as.data.table(dt[[10]])
data11<-as.data.table(dt[[11]])
data12<-as.data.table(dt[[12]])
data13<-as.data.table(dt[[13]])
data14<-as.data.table(dt[[14]])
data15<-as.data.table(dt[[15]])
data16<-as.data.table(dt[[16]])
data17<-as.data.table(dt[[17]])
data18<-as.data.table(dt[[18]])
data19<-as.data.table(dt[[19]])
data20<-as.data.table(dt[[20]])
data21<-as.data.table(dt[[21]])
data22<-as.data.table(dt[[22]])
data23<-as.data.table(dt[[23]])
data24<-as.data.table(dt[[24]])
data25<-as.data.table(dt[[25]])

data1_by_dong_qrt<-aggregate(data1$price,by=list(data1$yyyyqrt,data1$dong),mean)


#시계열 회귀분
#