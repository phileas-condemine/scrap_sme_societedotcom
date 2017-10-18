
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(XML)
library(RCurl)
library(data.table)
library(ggplot2)
library(dplyr)

shinyServer(function(input, output) {
  
#   output$content<-renderText({
#     if (!input$name[1]==""){
#       return()
#     }
#     curl=getCurlHandle()
#     curlSetOpt(cookiejar='cookies.txt',useragent='Mozilla/5.0',followlocation=T,curl=curl,verbose=T,timeout=100)
#     risques=gsub(" ", "+",input$keyword)[1]
#     sinistres<-getURLContent(paste0("https://www.google.fr/search?q=",risques),ssl.verifypeer = FALSE)
#     paste0(unlist(readHTMLList(sinistres)[[10]]),"\n\n")
#   })
  
  
  
  output$var<-renderUI({
    if (input$name[1]=="")
      return()
    name=unlist(strsplit(input$name,split=c(", ","; ","/ ")))
    name=gsub(" ","+",name)
    adresse<-getURLContent(paste0("https://www.google.fr/search?q=","+societe.com+",name[1]),
                           ssl.verifypeer = FALSE)
    sirenpattern <- '[0-9]{9}'
    resultats=readHTMLList(adresse)
    numlist=grep(pattern="societe.com",resultats)[1]
    print(numlist)
    
    resultats=resultats[[numlist]]
    k=grep(pattern = "societe.com",resultats)[1]
    posSiren=unlist(regexpr(text=resultats[k],pattern=sirenpattern))
    siren=substr(x = resultats[k],start=posSiren[1],stop=posSiren[1]+8)
    
    posName=unlist(gregexpr(text=resultats[k],pattern="/"))
    
    firm=substr(x = resultats[k],start=posName[length(posName)]+1,stop=posSiren[1]-1)
    # readHTMLList(adresse)[[10]][1]
    table = input$tab
      url=paste("http://www.societe.com/bilan/",firm,siren,2013,"12311.html",sep="")
      
      test<-getURLContent(url,ssl.verifypeer = FALSE)
      temp<-t(readHTMLTable(test)[[table]])
      header=temp[1,which(!(sapply(temp[2,],FUN=function(x){is.na(as.numeric(x))})))]
      selectizeInput(inputId = "indic",label = "which financial indicator ?",choices = header,
                   selected="",multiple=F,options=list(placeholder="indicator ?"))
  })
  
  
  output$years<-renderUI({
    if (input$name[1]=="")
      return()
    name=unlist(strsplit(input$name,split=c(", ","; ","/ ")))
    name=gsub(" ","+",name)
    adresse<-getURLContent(paste0("https://www.google.fr/search?q=","+societe.com+",name[1]),
                           ssl.verifypeer = FALSE)
    sirenpattern <- '[0-9]{9}'
    resultats=readHTMLList(adresse)
    numlist=grep(pattern="societe.com",resultats)[1]
    print(numlist)
    resultats=resultats[[numlist]]
    k=grep(pattern = "societe.com",resultats)[1]    
    
    posSiren=unlist(regexpr(text=resultats[k],pattern=sirenpattern))
    siren=substr(x = resultats[k],start=posSiren[1],stop=posSiren[1]+8)
    
    posName=unlist(gregexpr(text=resultats[k],pattern="/"))
    
    firm=substr(x = resultats[k],start=posName[length(posName)]+1,stop=posSiren[1]-1)
    # readHTMLList(adresse)[[10]][1]
    table = input$tab
    url=paste("http://www.societe.com/bilan/",firm,siren,2013,"12311.html",sep="")
    
    test<-getURLContent(url,ssl.verifypeer = FALSE)
    temp<-t(readHTMLList(test))
    temp<-temp[[grep("Sociaux",temp)]]
    years=sort(as.numeric(gsub(pattern = "Sociaux ",replacement = "",x = temp)))
    sliderInput(inputId = "years",label = "Quelles années ?",
                   step=1,min=min(years),max=max(years),value=c(min(years),max(years)))
  })
  
  
  output$firm<-renderUI({
    if (input$name[1]=="")
      return()
    if (input$tab[1]=="")
      return()
  
    
    name=unlist(strsplit(input$name,split=c(", ","; ","/ ")))
  
    selectizeInput(inputId = "firm",label = "which firm ?",choices = name,
                   selected="",multiple=F)
    
  })
  
  output$custom<-renderUI({
    if (input$name[1]=="")
      return()
    textInput("vect","custom vector of size 10 for claims or stuff ?","")
  })
  
  output$res<-renderPlot({
    if (input$name[1]=="")
      return()
    if (input$tab[1]=="")
      return()
    if (is.null(input$years))
      return()
    name=unlist(strsplit(input$name,split=c(", ","; ","/ ")))
    name=gsub(" ","+",name)
    gg<-ggplot()
    for (i in 1:length(name)){
      url2<-paste0("https://www.google.fr/search?q=","+societe.com+",name[i])
      print(name)
      print(i)
    adresse<-getURLContent(url2,
                           ssl.verifypeer = FALSE)
    sirenpattern <- '[0-9]{9}'
    resultats=readHTMLList(adresse)
    numlist=grep(pattern="societe.com",resultats)[1]
    print(numlist)
    resultats=resultats[[numlist]]
    k=grep(pattern = "societe.com",resultats)[1]
    
    posSiren=unlist(regexpr(text=resultats[k],pattern=sirenpattern))
    siren=substr(x = resultats[k],start=posSiren[1],stop=posSiren[1]+8)
    posName=unlist(gregexpr(text=resultats[k],pattern="/"))
    
    firm=substr(x = resultats[k],start=posName[length(posName)]+1,stop=posSiren[1]-1)
    # readHTMLList(adresse)[[10]][1]
    table = input$tab
    compteres=NULL
    conform=TRUE
    header=NULL
    for (year in input$years[1]:input$years[2]){
#       Sys.sleep(abs(rnorm(n=1,mean=0.1,sd=.1)))
      url=paste("http://www.societe.com/bilan/",firm,siren,year,"12311.html",sep="")
      test<-getURLContent(url,ssl.verifypeer = FALSE)
      temp<-t(readHTMLTable(test)[[table]])
      if (conform){
        if (year==input$years[1])
          header=temp[1,]
        conform=(min(header==temp[1,])==1)
        header=temp[1,]
      }
      compteres=rbind(compteres,data.frame(x=year,y=temp[2,which(temp[1,]%in%input$indic)]))
    }
    if (max(is.na(compteres$x))==1)
      return()
    compteres=data.table(x=as.numeric(as.character(compteres$x)),
                         y=as.numeric(as.character(compteres$y))/abs(mean(as.numeric(as.character(compteres$y)))),
                         indic=paste(name[i],"-",input$indic[1]))
#     plot(x=compteres$x,y=compteres$y)
    gg<-gg+geom_line(data=compteres,aes(x=x,y=y,color=indic))
    }
    numextractall <- function(string){ # http://stackoverflow.com/questions/19252663/extracting-decimal-numbers-from-a-string
    unlist(regmatches(string,gregexpr("[[:digit:]]+\\.*[[:digit:]]*",string)), use.names=FALSE)} 
    
    if (!input$vect==""){
      compteres=cbind(compteres,custom=as.numeric(numextractall(input$vect)))
      gg<-gg+geom_line(data=compteres,aes(x=x,y=custom,color="custom"))}
    gg

  })


output$tab<-renderTable({
  if (input$name[1]=="")
    return()
  if (input$tab[1]=="")
    return()
  if (is.null(input$years))
    return()
  name=unlist(strsplit(input$name,split=c(", ","; ","/ ")))
  name=gsub(" ","+",name)
  i=input$firm
    url2<-paste0("https://www.google.fr/search?q=","+societe.com+",i)
    print(name)
    print(i)
    adresse<-getURLContent(url2,
                           ssl.verifypeer = FALSE)
    sirenpattern <- '[0-9]{9}'
    resultats=readHTMLList(adresse)
    numlist=grep(pattern="societe.com",resultats)[1]
  print(numlist)
  
    resultats=resultats[[numlist]]
    k=grep(pattern = "societe.com",resultats)[1]
    
    posSiren=unlist(regexpr(text=resultats[k],pattern=sirenpattern))
    siren=substr(x = resultats[k],start=posSiren[1],stop=posSiren[1]+8)
    posName=unlist(gregexpr(text=resultats[k],pattern="/"))
    
    firm=substr(x = resultats[k],start=posName[length(posName)]+1,stop=posSiren[1]-1)
    # readHTMLList(adresse)[[10]][1]
    table = input$tab
    compteres=NULL
    conform=TRUE
    header=NULL
    for (year in input$years[1]:input$years[2]){
      #       Sys.sleep(abs(rnorm(n=1,mean=0.1,sd=.1)))
      url=paste("http://www.societe.com/bilan/",firm,siren,year,"12311.html",sep="")
      test<-getURLContent(url,ssl.verifypeer = FALSE)
      temp<-t(readHTMLTable(test)[[table]])
      if (conform){
        if (year==input$years[1])
          header=temp[1,]
        conform=(min(header==temp[1,])==1)
        header=temp[1,] 
      }
      compteres=rbind(compteres,c(year,temp[2,]))
    }  
  compteres=data.table(compteres)
  setnames(x = compteres,old = colnames(compteres),new = c("annee",header))
  for (c in colnames(compteres)){
    if (max(is.na(as.numeric(compteres[[c]])))==1)
      compteres=compteres[,which(!colnames(compteres)%in%c),with=F]
  }
  compteres
})
  
})
