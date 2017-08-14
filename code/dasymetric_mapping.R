###
### Dasymetric mapping: specifying a population to each building
### 
### INPUT:
###      - shapefile of building
###      - shapefile of population by census block
### OUTPUT:
###      - shapefile of buildings with a field for population appended
###
###      
### AUTHOR:
###      Tom Logan
### MODIFIED:
###      November 2016
###
### NOTES:
###      A function for R
### 

####
## Libraries
####
# library(maptools)
library(pbapply)
library(rgdal)
library(raster)
library(rgeos)
library(ggplot2)
library(spdep)
library(pracma)
library(maptools)
library(parallel)
library(httr)
library(FNN)
library(data.table)

# User defined inputs
name.city <- 'phi'

# other variables
kNoCores <<- 4 #floor(detectCores() - 6)  # Calculate the number of cores
filename.bldg <- paste('buildings/',substr(name.city,1,3),'_bldg_clust', sep = "")
filename.censusblock <- paste('demographics/',substr(name.city,1,3),'_pop_censusblock', sep = "")
# output filename
filename.out <- paste(substr(name.city,1,3),'_bldgclust_pop', sep = "")

workdir = paste('F:/UrbanDataProject/green_space_access/data/', name.city, sep = '')

main <- function(){
  
  # changes to work directory
  setwd(workdir)  
  
  # import the data
  the.data <- ImportData()
  list2env(the.data,env = environment())
  
  # for each census blocks
    # how many buildings in block
    # what is total population of block
    # for each building, assign pop/buildings
  bldgs <- ApplyPopAssignment(bldgs,cens.block)    
  
  # write to csv and shapefile
  WriteResults(bldgs)
  
}


ImportData <- function(){
  # import the data

  # open both of the files
    # buildings
    bldg.path <- file.path(workdir,filename.bldg)
    bldg.dsn <-  file.path(workdir,gsub("\\/.*","",filename.bldg))
    bldg.layer <- gsub(".*/","",bldg.path)
    bldgs <- readOGR(dsn = bldg.dsn, layer = bldg.layer, verbose = FALSE)
    # demographics
    block.path <- file.path(workdir,filename.censusblock)
    block.dsn <-  file.path(workdir,gsub("\\/.*","",filename.censusblock))
    block.layer <- gsub(".*/","",block.path)
    cens.block <- readOGR(dsn = block.dsn, layer = block.layer,verbose = FALSE)
  
  # ensure all projections are the same
  if (proj4string(cens.block) != proj4string(bldgs)){
    cens.block = spTransform(cens.block, CRS(proj4string(bldgs)))
    # print(sf)
  }

  # convert population factor to numeric
  cens.block@data[,"POP10"] <- as.numeric(as.character(cens.block@data[,"POP10"]))
  
  # assign bldg ids to all bldgs
  bldgs@data$orig_id <- seq(1,length(bldgs))
  
  # assign zero pop to all bldgs
  # bldgs@data$pop <- 0
  
  the.data <- list(bldgs = bldgs, cens.block = cens.block)
  print('Data import complete')
  return (the.data)
}


ApplyPopAssignment <- function(bldgs,cens.block){
  # loop through the bldgs
  
  num.blocks <- length(cens.block)
  
  # parallel
  if (kNoCores == 1){
    result <- pbsapply(seq(1,num.blocks), function(i) AssignPopulation(i,cens.block,bldgs))
  } else {
    cl <- makeCluster(kNoCores)
    clusterExport(cl, c('AssignPopulation','cens.block','bldgs'), envir=environment())
    clusterEvalQ(cl, c(library(rgeos)))
    # duration and distance for travel
    result <- pbsapply(seq(1,num.blocks), function(i) AssignPopulation(i,cens.block,bldgs), cl = cl)
    parallel::clusterSetRNGStream(cl, iseed = 0L)
    stopCluster(cl)
  }
  
  result <- rbindlist(lapply(result,function(i) as.data.frame(t(i))),fill=T)
  
  order <- match(bldgs@data$orig_id,result$orig_id)
  
  bldgs@data$pop <- result$pop[order]#merge(bldgs@data,result,by = 'orig_id',all=T)

  print('Bldg pop assignments complete')
  return(bldgs)
}


AssignPopulation <- function(i,cens.block,bldgs){
  # assigns a population to each building:
  # for each census blocks
    # how many buildings in block
    # what is total population of block
      # for each building, assign pop/buildings

    block.pop <- cens.block@data[i,"POP10"]
    
    # block polygon
    block.poly <- cens.block[i,]
    
    # buildings in block
    block.bldgs <- bldgs[block.poly,]
    
    # number of residential buildings in the block
    block.bldgs.num <- length(block.bldgs)
    
    if (block.bldgs.num > 0) { 
      # bldg pop
      bldg.pop <- block.pop/block.bldgs.num
      
      
    } else {
      bldg.pop <- 0
    }
    result <- rbind("pop" = bldg.pop, "orig_id" = block.bldgs@data$orig_id)
  return(result)  
}


WriteResults <- function(bldgs){
  # write the result table to csv
  save_dir <- file.path(getwd(),'buildings')
  setwd(save_dir)
  write.csv(bldgs@data,file=paste(filename.out,'.csv', sep = ""), row.names=FALSE)
  
  # write closest to shapefile
  WriteShp(bldgs, save_dir)
}


WriteShp <- function(bldgs, save_dir){
  # write the data to shapefile
  filename.shp <- paste(filename.out, sep = "")
  # we save once with writeOGR so we have .proj file
  writeOGR(bldgs, dsn = save_dir, layer = filename.shp, driver = 'ESRI Shapefile', overwrite_layer = T)
  # we overwrite with writeSpatialShape so the col names aren't truncated
  writeSpatialShape(bldgs, paste(save_dir,'/',filename.shp,sep=''))
}
