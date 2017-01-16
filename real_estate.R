#문제정의  : 1. 내가 살고 있는 집 혹은 우리 동네 관심을 갖고 있는 집값에 대한 예측
# 2.전체 매매, 전세 추이 및 내가 살고 있는 지역의 매매,전세 추이

library(XML)
#data 수집 과정 (data.go.kr에서 제공하는 API 사용)
service_key <- "EeBjN2xdCzzcqHvefO0rZXaycAim0uGpKxnOX72PY1UpkSZnifzIK1kxLm61XXaQ4pFxhbW%2F%2FZbmQDKFiAFNVA%3D%3D"
#서울시 지역코드
locCode <-c(11110,11140,11170,11200,11215,11230,11260,11290,11305,11320,11350,11380,11410,11440,11470,11500,11530,11545,11560,11590,11620,11650,11680,11710,11740)
datelist <-c(201601,201602,201603,201604,201605,201606,201607,201608,201609,201610,201611,201612)


#url <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=",locCode,"&DEAL_YMD=",datelist,"&serviceKey=",service_key)

url <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=11110&DEAL_YMD=201601&serviceKey=",service_key)



raw.data <-xmlTreeParse(url, useInternal=TRUE,encoding = "utf-8")
rootNode <- xmlRoot(raw.data)
rootNode['body']
as.list(rootNode)
item<-xpathSApply(rootNode,"//item",xmlValue)
item['거래금액']
xmlName(item)
items[1][1]
items[2][1]
xmlSize(items[5])
names(rootNode/item)
line
listline <- as.list(line)
price <-xpathSApply(listline,"//item/@거래금액",xmlValue)
