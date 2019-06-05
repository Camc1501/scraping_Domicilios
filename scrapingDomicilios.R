library(rvest)
library(RSelenium)

#Getting name of restaurants, link to get more info and the categorie
a=1
restaurants<-c()
linkn<-c()

repeat{
  print(a)
  #change th page automated to get the hole list of restaurants
  pag<-paste0("https://domicilios.com/restaurantes/bogota?bt=RESTAURANT&page=",a)
  
  web<-read_html(pag)
  
  rest<-html_nodes(web,'.arrivalName')%>%html_text
  l1<-html_nodes(web,'.arrivalName')%>%html_attr('href')

  #verifying if exist more restaurants 
  if(length(rest)==0){
    break
  }
 restaurants<-c(restaurants,rest)
 linkn<-c(linkn,l1)
 a<-a+1
}

df1<-data.frame(restaurants,linkn)

fileName<-paste0("RestaurantsDomicilios_",as.character.Date(Sys.time()),".csv")
write.csv(df1, file = fileName)


remDr <- rsDriver(
  remoteServerAddr = "localhost",
  port=4445L,
  browser = "firefox"
)

Name<-c()
Times<-c()
Shippings<-c()
Deliveries<-c()
Comments<-c()
Dates<-c()
Rankings<-c()
Total_comments<-c()


rd<-remDr[["client"]]

previousl2<-linkn#replace section with all linkn
counter=1

for (co in previousl2) {
  
  l2<-paste0(co,"?t=comentarios")
  
 rd$navigate(l2)
repeat{
  
  rd$findElements(using = "id", value = "profileDetails")
  rd$findElements(using = "id", value = "profileConteiner")
  
  so<-rd$getPageSource()
  
  web<- read_html(as.character(so))
  
  t<-html_nodes(web,'.deliveryTime')%>%html_text
  if (length(t)<1) {
    t<-"N/A"
  }
  
  cost<-html_nodes(web,'.shippingAmount')%>%html_text
  if (length(cost)<1) {
    cost<-"N/A"
  }
  
  minimo<-html_nodes(web,'.deliveryAmount')%>%html_text
  if (length(minimo)<1) {
    minimo<-"N/A"
  }
  
  com<-html_nodes(web,'#reviewList p:nth-child(2)')%>%html_text
  if (length(com)<1) {
    com<-"N/A"
    dat<-"N/A"
    rank<-"N/A"
    t_com<-"N/A"
  }else{
    dat<-html_nodes(web,'#reviewList header p')%>%html_text
    rank<-html_nodes(web,'#reviewList .rating-points')%>%html_text
    t_com<-html_nodes(web,'#profileTabs button:nth-last-child(2)')%>%html_text
  }
  count=1
  for (x in com) {
    Name<-c(Name,restaurants[counter])
    Times<-c(Times,t)
    Shippings<-c(Shippings,cost)
    Deliveries<-c(Deliveries,minimo)
    Comments<-c(Comments,x)
    Dates<-c(Dates,dat[count])
    Rankings<-c(Rankings,rank[count])
    Total_comments<-c(Total_comments,t_com)
    count=count+1
  }
  
  flag<-tryCatch(
    {
      nextpag<-rd$findElement(using = "css", value = ".arrow.next")
    },
    error=function(cond) {
      return("e")
    }
  )
  
 if(typeof(flag)!="character"){
   nextpag$clickElement()
   Sys.sleep(2)
 }else{
    break
  }
}
counter=counter+1  
}
#end of scraping
rd$close()

df<-data.frame(Name,Total_comments,Times,Shippings,Deliveries,Comments,Dates,Rankings)
fileName<-paste0("Restaurants&Comments_",as.character.Date(Sys.time()),".csv")
write.csv(df, file = fileName)

