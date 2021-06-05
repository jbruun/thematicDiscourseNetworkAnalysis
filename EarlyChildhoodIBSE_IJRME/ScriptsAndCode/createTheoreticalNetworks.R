#This script creates a lingusitic network based on the theoretical texts corpus
#It will write a comma separated variables file called "listNodesTheoretical.csv" to the working directory
#The end product is allNetTheoretical to be used in furher analyses.

#First load necessary packages
library(tm)
library(igraph)
#Load the files we need
allTheoretical <-Corpus(DirSource("textsTheoretical", encoding="UTF-8"), readerControl = list(language="lat")) 
#Perform basic text-mining preprocessing
allTheoretical<-tm_map(allTheoretical,tolower)
allTheoretical<-tm_map(allTheoretical,removePunctuation)
allTheoretical<-tm_map(allTheoretical,stripWhitespace)


source("EarlyChildhoodIBSE_IJRME/ScriptsAndCode/R-scripts/substitutions.r")

#These words either provide no meaning or are used so many times that they obscure relevant signals.

wordstoremove<-c(stopwords("english"),"can","also","although","may","maybe","moreover","mostly","onto","per se", "really","therefore","though","thus",
                 "xl011","xl021","xl051","xl071","xn011","xn031","xn051","xn071","xq011","xq031","xq041","xq051","xs021","xs091","xs111","xs121",
                 "2013appendixfp10","20132014","1b","p234","p141")


#Cleaning up..

#allFilesx <- tm_map(allFilesx, removeWords, c("endoffile"))
allTheoretical<-tm_map(allTheoretical,stripWhitespace)

source("EarlyChildhoodIBSE_IJRME/ScriptsAndCode/R-scripts/mywordnetwork.r")
#Applying the function to each excerpt

edgelistsTheoretical<-lapply(allTheoretical,myWordNetwork,j.words=wordstoremove)
graphsTheoretical<-list()
for (i in 1:35){
  graphsTheoretical[[i]]<-graph.edgelist(edgelistsTheoretical[[i]],directed=T)
} 

#Bind all excerpts together to a single edge list

allEdgeTheoretical<-rbind(edgelistsTheoretical[[1]],edgelistsTheoretical[[2]],edgelistsTheoretical[[3]],edgelistsTheoretical[[4]],edgelistsTheoretical[[5]],edgelistsTheoretical[[6]],edgelistsTheoretical[[7]],edgelistsTheoretical[[8]],edgelistsTheoretical[[9]],edgelistsTheoretical[[10]],
                        edgelistsTheoretical[[11]],edgelistsTheoretical[[12]],edgelistsTheoretical[[13]],edgelistsTheoretical[[14]],edgelistsTheoretical[[15]],edgelistsTheoretical[[16]],edgelistsTheoretical[[17]],edgelistsTheoretical[[18]],edgelistsTheoretical[[19]],edgelistsTheoretical[[20]],
                        edgelistsTheoretical[[21]],edgelistsTheoretical[[22]],edgelistsTheoretical[[23]],edgelistsTheoretical[[24]],edgelistsTheoretical[[25]],edgelistsTheoretical[[26]],edgelistsTheoretical[[27]],edgelistsTheoretical[[28]],edgelistsTheoretical[[29]],edgelistsTheoretical[[30]],
                        edgelistsTheoretical[[31]],edgelistsTheoretical[[32]],edgelistsTheoretical[[33]],edgelistsTheoretical[[34]],edgelistsTheoretical[[35]])

#make separate networks for each article. Not used in manuscript
articlesT<-lapply(edgelistsTheoretical,graph.edgelist,directed=T)
for (i in 1:length(articlesT)){
  E(articlesT[[i]])$weight<-1
  articlesT[[i]]<-simplify(articlesT[[i]],edge.attr.comb="sum")
  articlesT[[i]]<-delete.vertices(articlesT[[i]],which(V(articlesT[[i]])$name=="break"))
}


#make combined network

allNetTheoretical<-graph.edgelist(allEdgeTheoretical,directed=T)
E(allNetTheoretical)$weight<-1
allNetTheoretical<-delete.vertices(allNetTheoretical,v = which(V(allNetTheoretical)$name=="break"))
allNetTheoretical<-simplify(allNetTheoretical,remove.multiple=T,edge.attr.comb=list(weight="sum"))
prT<-page.rank(allNetTheoretical)
V(allNetTheoretical)$pr<-prT$vector
sort(prT$vector,decreasing = T)[1:10]
fgT<-fastgreedy.community(as.undirected(allNetTheoretical))
write.csv(V(allNetTheoretical)$name,"listNodesTheoretical.csv")

