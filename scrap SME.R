library(XML)
library(RCurl)

curl=getCurlHandle()
curlSetOpt(cookiejar='cookies.txt',useragent='Mozilla/5.0',followlocation=T,curl=curl,verbose=T,timeout=100)

url<-"http://www.societe.com/societe/axa-572093920.html"




risques=c("entreprise accidentel incendie","incendie volontaire entreprise","faillite stocks sinistr?s") #Pourquoi ne pas jouer sur une patte long/short avec un revendeur de produits sinistr?s
risques=gsub(" ", "+",risques)
sinistres<-getURLContent(paste0("https://www.google.fr/search?q=",risques[3]),ssl.verifypeer = FALSE)

readHTMLList(sinistres)[[10]]

firm=c("Colas+BONNEVILLE","Valette+foie+gras","francegrossiste")

adresse<-getURLContent(paste0("https://www.google.fr/search?q=","+societe.com+",firm[1]),
                       ssl.verifypeer = FALSE)
sirenpattern <- '[0-9]{9}'
posSiren=unlist(regexpr(text=readHTMLList(adresse)[[10]][1],pattern=sirenpattern))
siren=substr(x = readHTMLList(adresse)[[10]][1],start=posSiren[1],stop=posSiren[1]+8)

posName=unlist(gregexpr(text=readHTMLList(adresse)[[10]][1],pattern="/"))

Name=substr(x = readHTMLList(adresse)[[10]][1],start=posName[length(posName)]+1,stop=posSiren[1]-1)
# readHTMLList(adresse)[[10]][1]
table = "compteresultat"
compteres=NULL
conform=T
header=NULL
for (i in 2004:2013){
  Sys.sleep(rnorm(n=1,mean=0.5,sd=1)^2)
url=paste("http://www.societe.com/bilan/",Name,siren,i,"12311.html",sep="")

test<-getURLContent(url,ssl.verifypeer = FALSE)
temp<-t(readHTMLTable(test)[[table]])
if (conform){
  if (i==2004)
    header=temp[1,]
  conform=(header==temp[1,])
  header=temp[1,]
}
compteres=rbind(compteres,temp[2,])
}


levels(as.factor(fread("/Users/p-condemine/Documents/projet ENSAE/sample db.csv")$age))
# Faire un shiny qui permet de choisir le mot clef, visualiser les r?sultats, saisir le nom de l'entreprise puis visualiser des bilans selon une liste de variables.
# Mettre aussi un input qui permet d'entrer des donn?es de sinistres ? tracer ? cot? : ann?e/nbsin ou ann?e/d?faut01
