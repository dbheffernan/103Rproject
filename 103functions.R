aggregator <- function(modifer, startx){
  if(startx<40 && startx>1){startx=startx+1}
  state =0
  if(modifer==3)
  {
    modifer = 0
    states = 1
  }
  c = (county.filler(1,startx + modifer,states))
  c
}

basic.filler <- function(basicy, basicx){
  if(is.na(vote_data[basicy, basicx])==FALSE)
  {
    oth=0
    if(basicx > 39)
    {
      if(basicx %% 4 == 0){
        dem = vote_data[basicy, basicx]+vote_data[basicy,basicx+1]
        repub = vote_data[basicy+1, basicx]+vote_data[basicy+1, basicx+1]
        if(is.na(vote_data[basicy+2, basicx])==FALSE){
          oth = vote_data[basicy+2, basicx]+vote_data[basicy+2, basicx+1]}
        gop.percent(dem,repub,oth)
      }
      else if(basicx %% 4 > 0){
        dem = vote_data[basicy, basicx+1]
        repub = vote_data[basicy+1, basicx+1]
        if(is.na(vote_data[basicy+2, basicx+1])==FALSE){
          oth = vote_data[basicy+2, basicx+1]}
        gop.percent(dem,repub,oth)
      }
    }
    else
    {
      dem = vote_data[basicy, basicx]
      repub = vote_data[basicy+1, basicx]
      if(is.na(vote_data[basicy+2, basicx])==FALSE){
        oth = vote_data[basicy+2, basicx]}
      gop.percent(dem,repub,oth)
    }
  }
  else{NA}
}

basic.state.filler <- function(y,x){
  oth=0
  if(is.na(vote_data[y, x])==FALSE)
  {
    if(y>39){
      dem = as.numeric(vote_data[y, x])+as.numeric(vote_data[y,x+1])+as.numeric(vote_data[y,x+2])+as.numeric(vote_data[y,x+3])
      repub = as.numeric(vote_data[y+1, x])+as.numeric(vote_data[y+1, x+1])+as.numeric(vote_data[y+1, x+2])+as.numeric(vote_data[y+1,x+3])
      if(is.na(vote_data[y+2, x])==FALSE){
        oth = as.numeric(vote_data[y+2, x])+as.numeric(vote_data[y+2, x+1])+as.numeric(vote_data[y+2, x+2])+as.numeric(vote_data[y+2,x+3])}
      print(repub)
      gop.percent(dem,repub,oth)
    }
    else{
      dem = as.numeric(vote_data[y, x])+as.numeric(vote_data[y,x+1])+as.numeric(vote_data[y,x+2])
      repub = as.numeric(vote_data[y+1, x])+as.numeric(vote_data[y+1, x+1])+as.numeric(vote_data[y+1, x+2])
      if(is.na(vote_data[y+2, x])==FALSE){
        oth = as.numeric(vote_data[y+2, x])+as.numeric(vote_data[y+2, x+1])+as.numeric(vote_data[y+2, x+2])}
      gop.percent(dem,repub,oth)
    }
  }
  else{NA}
}
census.estimator <- function(parameter, index, year){
  ncc =0
  kent =0
  sussex =0
  state =0
  multiplier = (year %% 10)/10
  if(index > 1) {index <- ((index-1) * 3) + 1}
  if(year %% 10 == 0)
  {
    ncc= as.numeric(census2[parameter,index+1])
    kent=as.numeric(census2[parameter,index])
    sussex = as.numeric(census2[parameter,index+2])
    state = ncc + kent + sussex
  }
  else{
    ncc = as.numeric(census2[parameter,index+1] + (census2[parameter,index+4] - census2[parameter,index+1]) * multiplier)
    kent = as.numeric(census2[parameter,index] + (census2[parameter,index+3] - census2[parameter,index]) * multiplier)
    sussex = as.numeric(census2[parameter,index+2] + (census2[parameter,index+5] - census2[parameter,index+2]) * multiplier)
    state = ncc + kent + sussex
  }
  census_year = data.table(ncc,kent,sussex,state)
}

census.filler <- function(parameter){
  index = 1
  year = 1960
  CPI = 1
  ncc = as.numeric(census2[parameter, index+1])
  kent = as.numeric(census2[parameter, index])
  sussex = as.numeric(census2[parameter, index+2])
  state = ncc + kent + sussex
  parameter_table = data.table(ncc, kent, sussex, state)
  print(parameter_table)
  if(parameter==5){parameter_table <- parameter_table * as.numeric(CPI_inflation[CPI])}
  year <- year +2
  CPI <- CPI +1
  while(year < 2010){
    new_year = census.estimator(parameter, index, year)
    if(parameter==5){new_year <- new_year * as.numeric(CPI_inflation[((year-1960)/2)+1])}
    parameter_table <- rbind(parameter_table, new_year)
    year <- year +2
    CPI <- CPI +1
    if(year %% 10 == 0){index <- index +1}
  }
  parameter_table
}

county.filler <- function(index,indey, state){
  x=c()
  for(i in 1:10){
    if(is.na(vote_data[index,indey])==FALSE)
    {
      if(state < 1) {x[i] = basic.filler(index,indey)}
      else{x[i] = basic.state.filler(index,indey)
      }
    }
    else{i <- i-1}
    index <- index + 3
  }
  x <- x[!sapply(x,is.null)]
  x <- as.numeric(x)
  x
}

final.filler <- function(pvi_compare, raws){
  year=1
  xindex = 1
  ncc_raws = aggregator(raws,xindex)
  if(pvi_compare==1){
    yearcompare =as.numeric(PVI[year])
  }
  else{
    yearcompare =0}
  x = ((year.filler(1,xindex))-yearcompare)
  vote_processed = data.table(x)
  xindex = xindex + 3
  year = year +1
  while (year<26) {
    if(pvi_compare==1){
      yearcompare =as.numeric(PVI[year])
    }
    y = ((year.filler(1,xindex))-yearcompare)
    vote_processed <- rbind(vote_processed, y)
    nz = aggregator(raws,xindex)
    ncc_raws <- c(ncc_raws, nz)
    year = year + 1
    xindex = xindex + 4
  }
  if(raws > -1){
    ncc_raws
  }
  else{
    vote_processed
  }
}
gop.percent <- function(dem,repub,other=0){
  repub/(dem+repub+other)
}
year.filler <- function(starty,startx){
  if(startx<40 && startx>1){startx=startx+1}
  ncc_mean =mean(county.filler(starty,startx,0))
  ncc_min =min(county.filler(starty,startx,0))
  ncc_med =median(county.filler(starty,startx,0))
  kent_mean =mean(county.filler(starty,startx+1,0))
  kent_min =min(county.filler(starty,startx+1,0))
  kent_med =median(county.filler(starty,startx+1,0))
  sussex_mean =mean(county.filler(starty,startx+2,0))
  sussex_min =min(county.filler(starty,startx+2,0))
  sussex_med =median(county.filler(starty,startx+2,0))
  state_mean =mean(county.filler(starty,startx,1),na.rm=TRUE)
  state_min =min(county.filler(starty,startx,1),na.rm=TRUE)
  state_med =median(county.filler(starty,startx,1),na.rm=TRUE)
  the_vote = data.table(ncc_mean,ncc_min,ncc_med,kent_mean,kent_min,kent_med,sussex_mean,sussex_min,sussex_med,state_mean,state_min,state_med)
  the_vote
  
}

