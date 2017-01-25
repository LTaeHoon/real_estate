#문제정의  : 1. 내가 살고 있는 집 혹은 우리 동네 관심을 갖고 있는 집값에 대한 예측
# 2.전체 매매, 전세 추이 및 내가 살고 있는 지역의 매매,전세 추이
data_processing <- function(){
#install.packages("XML")
library(XML)
library(data.table)

#data 수집 과정 (data.go.kr에서 제공하는 API 사용)
service_key <- "EeBjN2xdCzzcqHvefO0rZXaycAim0uGpKxnOX72PY1UpkSZnifzIK1kxLm61XXaQ4pFxhbW%2F%2FZbmQDKFiAFNVA%3D%3D"
#서울시 지역코드
locCode <-c("11110","11140","11170","11200","11215","11230","11260","11290","11305","11320","11350","11380","11410","11440","11470","11500","11530","11545","11560","11590","11620","11650","11680","11710","11740")
locCode_nm <-c("종로구","중구","용산구","성동구","광진구","동대문구","중랑구","성북구","강북구","도봉구","노원구","은평구","서대문구","마포구","양천구","강서구","구로구","금천구","영등포구","동작구","관악구","서초구","강남구","송파구","강동구")
datelist <-c("201601","201602","201603","201604","201605","201606","201607","201608","201609","201610","201611","201612")


#url <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=11110&DEAL_YMD=201601&serviceKey=",service_key)

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
  item_temp_dt<-data.table()
  raw.data <- xmlTreeParse(urllist[i], useInternalNodes = TRUE,encoding = "utf-8")
  rootNode <- xmlRoot(raw.data)
  items <- rootNode[[2]][['items']]
 # price','con_year','year','dong','aptnm','month','dat','area','bungi','loc','floor'
  size <- xmlSize(items)
  for(i in 1:size){
     item_temp <- xmlSApply(items[[i]],xmlValue)
     item_temp_dt <- data.table(price=item_temp[1],
                                con_year=item_temp[2],
                                year=item_temp[3],
                                dong=item_temp[4],
                                aptnm=item_temp[5],
                                month=item_temp[6],
                                dat=item_temp[7],
                                area=item_temp[8],
                                bungi=item_temp[9],
                                loc=item_temp[10],
                                floor=item_temp[11])
     item[[i]]<-list(item_temp_dt)
     #item<-rbind(item,t(as.data.table(item_temp)),fill=TRUE)
  }
  total[[i]]<-rbindlist(item)
}
result_apt_data <- rbindlist(total)
#컬럼명
names(result_apt_data)<-c("거래금액","건축년도","년","법정동","아파트","월","일","전용면적","지번","지역코드","층")
save(result_apt_data, file="apt_item_sales_dt.Rdata")


library(stringr)
library(ggplot2)
load("apt_item_sales_dt3.Rdata")

apt_data<-data.table(item)
#apt_data<-data.table(result_apt_data)
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
lapply(locCode,function(x){apt_data[loc==x,gu:=locCode_nm[which(locCode==x)]]})

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



# 서울시 구/분기별 아파트 매매 가격

apt_data_by_gu_qrt <- aggregate(apt_data$price, apt_data[,c("yyyyqrt","gu")], mean)
names(apt_data_by_gu_qrt) <- c('yyyyqrt','gu','pricemean')

ggplot(apt_data_by_gu_qrt,aes(x=yyyyqrt,y=pricemean,group=gu))+
  geom_line()+
  facet_wrap(~gu,scale='free_y', ncol=3)+
  stat_smooth(method="lm")


#특정구 동별 평균 매매가 추이
# 일단 구별(loc)로 나눈 구별 테이블이 필요
dt<-data.table()
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

data1_by_dong_qrt<-aggregate(data1$price,by=list(data1$yyyym,data1$dong),mean)

#시계열 분석
#시계열에 패턴이 존재한다면 미래에도 이러한 패턴이 계속 될 것이라는 가정하에 예측을 해볼 수 있다.
# 그렇다면 시계열에 패턴이 존재하는지 확인하는 방법은 시계열이 랜덤한지를 알아보는 랜덤성 검증이 있다.
# 랜덤성 검정 -> 런검정 
#
gu_meanprice <- aggregate(apt_data$price,apt_data[,c('yyyym','gu')],mean)
head(gu_meanprice)
names(gu_meanprice)<- c('yyyym','gu','price')
#중복없이 구 추출
gu_list <- unique(gu_meanprice$gu)

# 각 구별 매매가격의 랜덤성 검정 결과를 runs_p변수에 추가
install.packages("lawstat")
library(lawstat)
runs_p<-c()
for(g in gu_list){
  runs_p <- c(runs_p, runs.test(apt_data[gu %in% g,price])$p.value)
}

ggplot(data.table(gu_list, runs_p), aes(x=gu_list, y=runs_p, group=1)) +
  geom_line() + geom_point() +
  ylab('P-value') + xlab('구')

#도봉구를 제외하고 p값이 0.05 보다 작으므로 귀무가설 (랜덤하다)기각 할 수 있다 
#따라서 랜덤하지 않은 어떤 패턴이 있다고 할 수 있다.

# 그럼 이중에서 내가 살고 있는 관악구를 추출하여 시계열 패턴을 그려보겠다.
gwanak_data <- apt_data[gu=="관악구",]
gwanak_price <- aggregate(gwanak_data$price,gwanak_data[,'yyyym'],mean) 
names(gwanak_price)<-c('yyyym','price')
tot_ts <- ts(gwanak_price$price,start=c(2006,1),frequency = 12)
plot(tot_ts)

install.packages("forecast")
library(forecast)
arima_mdl <- auto.arima(tot_ts)
tsdiag(arima_mdl)

accuracy(arima_mdl)
plot(forecast(arima_mdl,h=6))
}

