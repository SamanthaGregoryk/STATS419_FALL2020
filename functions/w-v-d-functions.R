# find all costars in each movie and their rankings
costarAndRanking <- function(ttid){
  ttid.legend = ttid;
  rank.legend = subsetDataFrame(imdb.data$all.movies.actors.characters, "ttid", "==", ttid.legend);
}

# deleted Will and Denzel from list
delWllandDenzel <- function(movie){
  movie[-1,]
}

# merge two data frames 
mergeDF <- function(df1, df2){
  merge(df1, df2, by = "nmid")
}

# for correlation table
forCorrelation <- function(mtx){
  nrow = nrow(mtx);
  ncol = ncol(mtx);
  
  myCorr = rcorr(mtx);
  myM = colMeans(mtx, na.rm=TRUE);
  mySD = c();
  
  for(i in 1:ncol) {
    mySD = c(mySD, sd(mtx[,i], na.rm=TRUE));
  }
}

