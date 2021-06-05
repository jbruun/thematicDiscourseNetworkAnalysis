
#Function to help make linguistic networks
#It returns an list of edges in a directed network
myWordNetwork <- function (txt,j.words) {
  # Clean data
  #remove capitalization
  #corpus.temp          <- tm_map(corpus, tolower)
  #Remove punctuations and other unnecessary things
  #corpus.removed.punc  <- tm_map(corpus.temp, removePunctuation)
  #Remove numbers
  #corpus.removed.numb  <- tm_map(corpus.removed.punc, removeNumbers)    
  #Remove Whitespaces
  #corpus.removed.white <- tm_map(corpus.removed.punc, stripWhitespace)
  #corpus               <- Corpus(DataframeSource(data.frame(data)))
  
  #prepare for stemming (language is english, but TM has a package for other
  #languages)
  #corpus.temp <- corpus.removed.white
  #corpus.copy  <- corpus.temp
  #corpus.temp  <- tm_map(corpus.temp, stemDocument, language = language)
  #corpus.final <- tm_map(corpus.temp, stemCompletion, 
  #                      dictionary = corpus.copy)
  #corpus.final<-corpus.temp
  #Prepare to remove "stopwords" such as "A", "The", e.t.c.
  # keep "declarative words"
  #my.stopwords <- stopwords("english")
  #my.stopwords <- my.stopwords[!(my.stopwords %in% j.words)]
  #Remove stopwrods
  #corpus.crop <- tm_map(corpus.removed.white,removeWords,c(my.stopwords))
  
  # Create edge list for export
  edge.list <-strsplit(as.matrix(txt), split = " ")[[1]]
  #Remove unnecessary 
  #edge.list<-as.matrix(corpus.final[[1]])
  #edge.list <- edge.list[edge.list!=""]
  #edge.list<-strsplit(edge.list,split=" ")
  
  # index of "declarative words" (the -1 is because we're removing first and 
  # last word to make it into a word-network
  node.num <- length(edge.list[!(edge.list %in% j.words)]) - 1
  #Create matrix to be exported
  export.edge.list <- array("",dim=c(node.num,2))
  #Set column names as per Gephi requirements
  #colnames(export.edge.list) <- c("Source","Target","Label")
  # Put source word except the "joining words into the matrix to be exported
  export.edge.list[,1] <- edge.list[!(edge.list %in% j.words)][-length(edge.list[!(edge.list %in% j.words)])]
  # put the target word except the "joining words into the matrix to be exported
  export.edge.list[,2] <- edge.list[!(edge.list %in% j.words)][-1]
  
  #Go through the joining words and put them in
  # for(i in j.words) {
  #  for (j in which(edge.list %in% i)) {
  #  export.edge.list[j - sum(!which(edge.list %in% j.words) > j), 3] <- i
  # }
  #}
  return(export.edge.list)
}