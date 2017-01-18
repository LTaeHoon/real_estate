install.packages("rvest")
library(rvest)

#data 수집 과정 (data.go.kr에서 제공하는 API 사용)
service_key <- "EeBjN2xdCzzcqHvefO0rZXaycAim0uGpKxnOX72PY1UpkSZnifzIK1kxLm61XXaQ4pFxhbW%2F%2FZbmQDKFiAFNVA%3D%3D"
#서울시 지역코드
locCode <-c("11110","11140","11170","11200","11215","11230","11260","11290","11305","11320","11350","11380","11410","11440","11470","11500","11530","11545","11560","11590","11620","11650","11680","11710","11740")
datelist <-c("201601","201602","201603","201604","201605","201606","201607","201608","201609","201610","201611","201612")


url <- paste0("http://openapi.molit.go.kr:8081/OpenAPI_ToolInstallPackage/service/rest/RTMSOBJSvc/getRTMSDataSvcAptTrade?LAWD_CD=",locCode[1],"&DEAL_YMD=",datelist[1],"&serviceKey=",service_key)
apt <- read_xml(url, encoding = 'utf-8')
xml_structure(apt)
items = xml_node(apt,'items')
items
Sys.setlocale("LC_ALL", "C")
#거래금액
price = xml_text(xml_nodes(items,'거래금액'))
price
#건축년도
con_year = xml_text(xml_nodes(items,"건축년도"))
#년
sale_year = xml_text(xml_nodes(items,"년"))
#법정동

#아파트

#월

#일

#전용면적

#지번

#지역코드

#층

iconv("嫄곕옒湲덉븸","utf-8","cp949")
Sys.setlocale("LC_ALL", "ko_KR.UTF-8")
Sys.getlocale()
# "嫄곕옒湲덉븸"   "嫄댁텞\xeb뀈\xeb룄"               "\xeb뀈" 
# 
# "踰뺤젙\xeb룞"   "\xec븘\xed뙆\xed듃"               "\xec썡" 
# 
# "\xec씪"   "\xec쟾\xec슜硫댁쟻"       "吏\u0080踰\x88" 
# 
# "吏\u0080\xec뿭肄붾뱶"               "痢\xb5" 