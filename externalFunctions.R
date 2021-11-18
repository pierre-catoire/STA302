shatterData = function(df,writecsv = T){
  df$NVISITE = 0
  df$AGEVISITE = df$AGE0
  df$CESDTVISITE = df$CESDT0
  
  for(id in sort(df$ID)){
    
    if(id%%100 == 0){print(paste("step ",
                                 id/100,
                                 "on ",
                                 floor(nrow(dfSave)/100)+1,
                                 " ...",
                                 sep=""))}
    
    if(!is.na(df[df$ID==id,]$AGE1)){
      newrow1 = c(df[df$ID == id,])
      newrow1$NVISITE = 1
      newrow1$AGEVISITE = newrow1$AGE1
      newrow1$CESDTVISITE = newrow1$CESDT1
    }
    
    if(!is.na(df[df$ID==id,]$AGE2)){
      newrow2 = c(df[df$ID == id,])
      newrow2$NVISITE = 2
      newrow2$AGEVISITE = newrow2$AGE2
      newrow2$CESDTVISITE = newrow2$CESDT2
    }
    
    if(!is.na(df[df$ID==id,]$AGE4)){
      newrow4 = c(df[df$ID == id,])
      newrow4$NVISITE = 4
      newrow4$AGEVISITE = newrow4$AGE4
      newrow4$CESDTVISITE = newrow4$CESDT4
    }
    
    if(!is.na(df[df$ID==id,]$AGE5)){
      newrow5 = c(df[df$ID == id,])
      newrow5$NVISITE = 5
      newrow5$AGEVISITE = newrow5$AGE5
      newrow5$CESDTVISITE = newrow5$CESDT5
    }
    
    if(!is.na(df[df$ID==id,]$AGE6)){
      newrow6 = c(df[df$ID == id,])
      newrow6$NVISITE = 6
      newrow6$AGEVISITE = newrow6$AGE6
      newrow6$CESDTVISITE = newrow6$CESDT6
    }
    
    if(length(newrow1) != 0){df[nrow(df)+1,] = newrow1}
    if(length(newrow2) != 0){df[nrow(df)+1,] = newrow2}
    if(length(newrow4) != 0){df[nrow(df)+1,] = newrow4}
    if(length(newrow5) != 0){df[nrow(df)+1,] = newrow5}
    if(length(newrow6) != 0){df[nrow(df)+1,] = newrow6}
    
    newrow1 = c()
    newrow2 = c()
    newrow4 = c()
    newrow5 = c()
    newrow6 = c()
  }
  
  df = df[order(df$ID),]
  
  
  write.csv(df, paste(getwd(),"/ShatteredDataFrame.csv",sep = ""))
  return(df)
}