install.packages("rvest")
library(rvest)
library(data.table)

#data 수집 과정 (data.go.kr에서 제공하는 API 사용)
service_key <- "EeBjN2xdCzzcqHvefO0rZXaycAim0uGpKxnOX72PY1UpkSZnifzIK1kxLm61XXaQ4pFxhbW%2F%2FZbmQDKFiAFNVA%3D%3D"
#서울시 지역코드
locCode <-c("11110","11140","11170","11200","11215","11230","11260","11290","11305","11320","11350","11380","11410","11440","11470","11500","11530","11545","11560","11590","11620","11650","11680","11710","11740")
datelist <-c("201601","201602","201603","201604","201605","201606","201607","201608","201609","201610","201611","201612")

urllist <- list()
cnt <-0
for(i in 1:length(locCode)){
  for(j in 1:length(datelist)){
    cnt=cnt+1
    urllist[cnt] <-paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=",locCode[i],"&DEAL_YMD=",datelist[j],"&serviceKey=",service_key) 
  }
}
#url <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=",locCode[1],"&DEAL_YMD=",datelist[1],"&serviceKey=",service_key)
apt_dt <- data.table()
for(i in 1:length(urllist)){
apt <- read_xml(url, encoding = 'utf-8')
xml_structure(apt)
items = xml_node(apt,'items')
items
#xml_node()에서 한글 사용을 위해 locale 값 변경 
Sys.setlocale("LC_ALL", "C")
#거래금액
price = xml_text(xml_nodes(items,'거래금액'))
#건축년도
con_year = xml_text(xml_nodes(items,"건축년도"))
#년
sale_year = xml_text(xml_nodes(items,"년"))
#법정동
dong = xml_text(xml_nodes(items,"법정동"))

#아파트
aptnm = xml_text(xml_nodes(items,"아파트"))
#월
month = xml_text(xml_nodes(items,"월"))
#일
day = xml_text(xml_nodes(items,"일"))
#전용면적
area = xml_text(xml_nodes(items,"전용면적"))

#지번
address = xml_text(xml_nodes(items,"지번"))

#지역코드
loc = xml_text(xml_nodes(items,"지역코드"))

#층
floor = xml_text(xml_nodes(items,"층"))
Sys.setlocale("LC_ALL", "")
apt_dt <- data.table(price,con_year,sale_year,dong,aptnm,month,day,area,address,loc,floor)

apt_dt<-rbind(apt_dt,apt)


