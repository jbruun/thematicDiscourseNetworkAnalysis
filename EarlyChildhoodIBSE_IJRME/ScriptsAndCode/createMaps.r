#run createTheoreticalNetworks.R and createEmpiricalNetworks.r first
#you should have an object called: allNetTheoretical and an object called allNetEmpirical
#These are graph objects, weighted and directed
#The script creates maps and graphs and write them to graphml files. These may be viewed in software Gephi.

##create bacbone versions of the graphs
source("R_scripts/backboneExtraction.r")
V(allNetTheoretical)$id<-V(allNetTheoretical)$name
freq<-strength(allNetTheoretical,mode = "all")

theoreticalBB<-backboneNetwork(allNetTheoretical,evalFunc = 1, alpha = 0.001)
V(theoreticalBB)$freq<-strength(allNetTheoretical,mode = "all")

####Create a map based on fast-greedy algorithm
fgT<-fastgreedy.community(as.undirected(allNetTheoretical))
fgTBB<-fastgreedy.community(as.undirected(theoreticalBB))

V(theoreticalBB)$fastgreedy<-fgTBB$membership
write.graph(theoreticalBB,"theoretical28052020.graphml",format="graphml")

V(empiricalBB)$fastgreedy<-fgEBB$membership
write.graph(empiricalBB,"empirical28052020.graphml",format="graphml")

##MAP CREA
mapTheoretical<-makemap(fgTBB$membership,theoreticalBB)


mapTBB<-backboneNetwork(mapTheoretical,0.005,1)
V(mapTBB)$n_words<-V(mapTheoretical)$n_words
mapTBB<-decompose.graph(mapTBB)[[1]]
fgMTBB<-fastgreedy.community(as.undirected(mapTBB))
V(mapTBB)$fastgreedy<-fgMTBB$membership
write.graph(mapTBB,"mapTBB21062020.graphml",format="graphml")


mapTheoreticalrem<-delete.vertices(mapTheoretical,1)
mapTremBB<-backboneNetwork(mapTheoreticalrem,0.005,1)
V(mapTremBB)$n_words<-V(mapTheoreticalrem)$n_words
mapTremBB<-decompose.graph(mapTremBB)[[1]]
fgMTremBB<-fastgreedy.community(as.undirected(mapTremBB))
V(mapTremBB)$fastgreedy<-fgMTremBB$membership
write.graph(mapTremBB,"mapTBBrem21062020.graphml",format="graphml")





