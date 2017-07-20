###
### Output origin, destination coordinate pairs for cities
### 
### INPUT:
###      - shapefile of origin points
###      - shapefile of destination points OR polygons
###      - cityName = string, e.g. 'baltimore', or 'detroit'
### OUTPUT:
###      - 
###      - shapefile containing same information
###
###      
### AUTHOR:
###      Tom Logan
###      Andrew Nisbet
###
### NOTES:
###      A function for R 
### 

####
## Libraries
####
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

####
## User defined inputs
####

# city and mode
name.city <- 'sea'
name.orig <- 'bldg'
name.dest <- 'parks'
mode <- 'walk'
workdir <- paste('F:/UrbanDataProject/paper_2/data/', name.city,'/input', sep = '')

# start queries fresh or continue
query.continue <- FALSE # if number >1 then a neartable and results table must already exist. if 1, any existing results will be overwritten
query.groupno <- 10e6 # number of queries to run between saving results

# constants
kNoCores <<- 20 #floor(detectCores() - 6)  # Calculate the number of cores
kPolygonBuffer <- 5  # number of meters to shave off edge of polygon destinations
grid.size <- 50  # meters of grid
kNearestNeighbors <- 50  # closest Euclidean number of destinations to report to origin

# OSRM parameters:
# If you have an OSRM server already running, provide
# osrm.url and that will be used.
osrm.url <- NULL #'http://localhost:5000' # NULL  # e.g. 'https://router.project-osrm.org'
# Otherwise a server can be created locally
# for Windows, just provide the path to an osm.pbf file, the path to a .lua
# routing profile, and the location to the osrm windows binaries included in
# this folder.
osm.pbf.path <- 'f:/UrbanDataProject/green_space_equity/OSRM/walking/washington-latest.osm.pbf'
osrm.profile.path <- 'f:/UrbanDataProject/green_space_equity/analysis/code/lib/osrm/profiles/foot.lua'
osrm.binary.path <- 'f:/UrbanDataProject/green_space_equity/analysis/code/lib/osrm/'

# file names
filename.dest <- paste(substr(name.city,1,3),'_', name.dest, sep = "")
filename.orig <- paste(substr(name.city,1,3),'_',name.orig, sep = "")
filename.out <- paste(substr(name.city,1,3),'_',name.orig,'_',name.dest,'_',mode, sep = "")
filename.full <- paste(filename.out,'_fullresults.csv', sep = "")
filename.neartable <- paste(filename.out,'_neartable.csv',sep="")

main <- function(){

  # Check that not overwriting results
  query.indx <- CheckExistingResults()
  
  # Start OSRM if needed.
  started.osrm.server <- FALSE
  if (is.null(osrm.url)) {
    print('No OSRM url provided, starting a server.')
    osrm.url <- StartOSRMServer(osm.pbf.path, osrm.profile.path, osrm.binary.path)
    started.osrm.server <- TRUE
  }
  
  # only run this if not continuing
  if (query.indx == 1){
    # import data
    the.data <- ImportData()
    orig.shp <- the.data[[1]]
    dest.shp <- the.data[[2]]
    
    # if destination or origin is a polygon, process to a series of points
    orig.pts <- ConvertOrigPolyToPoint(orig.shp)
    dest.pts <- ProcessPolygon(dest.shp,grid.size)
    
    # determine the nearest destinations to each origin
    near.table <- FindNearestEuclidean(orig.pts,dest.pts)
  } else {
    near.table <- fread(filename.neartable)
  }
  
  # query OSRM to return time and distance
  result.table <- DoQuerying(near.table, osrm.url, query.indx)
  
  # write to csv
  WriteResults(result.table)
  
  # Cleanup server.
  if (started.osrm.server) {
    KillOSRMServer()
  }
}


CheckExistingResults <- function(){
  # changes to work directory
  setwd(workdir)  
  # does the filename exist
  results.exist <- file.exists(filename.full)
  # if overwriting, confirm
  if (!query.continue){
    if (results.exist){
      x <- readline("* \n CAUTION: You want to overwrite existing results. Please confirm by typing Y \n* \n")  
      if (x != 'Y'){
        stop('overwriting cancelled by user')
      }
    }
    query.indx <- 1
  } else {
    # if continuing, does the neartable exist
    neartable.exist <- file.exists(filename.neartable)
    if (!neartable.exist){
      stop('neartable doesnt exist, cannot continue')
    }
    # determine the index to continue from
    # scan the results file
    query.indx <- nrow(fread(filename.full, select = 1L))
  }
  
  return(query.indx)
}


StartOSRMServer <- function (osm.pbf.path, osrm.profile.path, osrm.binary.path) {

  # Only works on windows.
  platform <- .Platform$OS.type
  if (platform != 'windows') {
    print('Wanring: starting OSRM may only work on Windows, provide osrm.url for other platforms.')
  }

  # Create requried temp directory.
  dir.create('c:/temp', showWarnings = FALSE)

  # Extract the data with the required profile.
  print('Extracting the map data.')
  extract.path <- file.path(osrm.binary.path, 'osrm-extract.exe')
  cmd <- paste(extract.path, osm.pbf.path, '-p', osrm.profile.path, sep = " ")
  system2('cmd.exe', input = cmd)

  # Contract the graph.
  print('Contracting the route graph.')
  osrm.file.path <- gsub('\\.osm\\.pbf$', '.osrm', osm.pbf.path)
  contract.path <- file.path(osrm.binary.path, 'osrm-contract.exe')
  cmd <- paste(contract.path, osrm.file.path, sep = " ")
  system2('cmd.exe', input = cmd)

  # Start the server.
  print('Starting the OSRM server.')
  routed.path <- file.path(osrm.binary.path, 'osrm-routed.exe')
  cmd <- paste(routed.path, osrm.file.path, '--port', '5000', sep = " ")
  system2('cmd.exe', input = cmd, wait = FALSE)

  # Return the url
  url <- 'http://localhost:5000'
  print(paste('OSRM server running at', url, sep = " "))
  return(url)

}


KillOSRMServer <- function() {
  cmd <- paste('Taskkill', '/IM', 'osrm-routed.exe', '/F')
  system2('cmd.exe', input = cmd)
}


ImportData <- function(){
  # import the data

  # open both of the files
  orig.shp = readOGR(dsn = workdir, layer = filename.orig,verbose = FALSE)
  dest.shp = readOGR(dsn = workdir, layer = filename.dest,verbose = FALSE)
  
  the.data <- c(orig.shp, dest.shp)
  print('Data import complete')
  return (the.data)
}


ConvertOrigPolyToPoint <- function(orig.shp){
  # if the origin is a polygon, convert it to a point at the centroid.
  
  # assign an id to each origin
  orig.shp$orig_id <- seq(0,length(orig.shp)-1)
  
  # add the city code to the data
  orig.shp$city <- name.city
  
  sf.class <- class(orig.shp)[1]
  if (sf.class == "SpatialPolygonsDataFrame"){
    
    # get the centroid for all of the polygons 
    orig.pts <- gCentroid(orig.shp,byid=TRUE)
    
    # relate the polygon data with the centroid
    orig.pts <- SpatialPointsDataFrame(orig.pts,data=orig.shp@data)
  } else {
    # rename
    orig.pts <- orig.shp
  }
  # output filename
  filename.shp <- paste(substr(name.city,1,3),'_',name.orig,'_pts', sep = "")
  
  WriteShp(orig.pts,filename.shp)
  return(orig.pts)
}


ProcessPolygon <- function(dest.shp,grid.size){
  # if the destination is a polygon, then we need to process it
  
  # assign an id to each destination
  dest.shp$dest_id <- seq(1,length(dest.shp))
  
  dest.class <- class(dest.shp)[1]
  if (dest.class == "SpatialPolygonsDataFrame"){
    
    # create a negative buffer into the polygon
    dest.shp.buf <- gBuffer(dest.shp,width=-kPolygonBuffer,byid=TRUE)
    
    # dest_ids are excluded 
    excld_dest <- setdiff(seq(1,length(dest.shp)),dest.shp.buf@data$dest_id)
    
    # convert the polygon into the boundary line
    dest.line <- as(dest.shp.buf,"SpatialLinesDataFrame")
    
    # regularly spaced points along lines
    dest.pts <- CreateGrid(grid.size,dest.line)
    
    # if there are any polygons which disappear because of buffer
    if (! length(excld_dest)==0){
      # get the centroid for all excluded polygons 
      dest.cntr <- gCentroid(dest.shp[excld_dest,],byid=TRUE)
      # relate the polygon data with the centroid
      dest.cntr <- SpatialPointsDataFrame(dest.cntr,data=dest.shp[excld_dest,]@data)
      # merge boundaries with centroids
      dest.pts <- do.call("rbind", c(dest.pts,dest.cntr))
    }
    
    
    
    
  } else {
    dest.pts <- dest.shp
  }
  # output filename
  filename.shp <- paste(substr(name.city,1,3),'_',name.dest,'_pts', sep = "")
  # write a .shp for the pts
  WriteShp(dest.pts,filename.shp)
  print('Destination processing complete')
  
  return(dest.pts)
}


CreateGrid = function(grid.size,dest.line){
  ## takes the grid size input
  ## returns the polygons with gridded boundaries
  print('Boundary gridding begun')
  
  # return the spatial point dataframes
  
  # split sf into kNoCores subsets
  sf.nums <- seq(1,length(dest.line))
  sf.subindx <- split(sf.nums, ceiling(sf.nums/ceiling(length(dest.line)/20)))
  
  # for each park, turn the boundary line to series of points
  cl <- makeCluster(kNoCores,outfile="")
  clusterExport(cl, c("grid.size","dest.line", "GridLine"), envir=environment())
  clusterEvalQ(cl, c(library(raster), library(rgeos), library(sp)))
  spdf_list = parLapply(cl,sf.nums,function(j) GridLine(j))
  stopCluster(cl)
  
  # merge the spatialpointdataframes
  dest.pts <- do.call("rbind", spdf_list)
  
  #plot(sg); lines(sf)
  return(dest.pts)
}


GridLine <- function(j){
  # intersect a single park polygon
  
  # park line
  p1 <- dest.line[j,]
  
  # number of points on line
  line.pts <- sum(SpatialLinesLengths(p1))/grid.size
  if(line.pts <= 2){line.pts = 2}
  
  # line to points
  dest.pt <- spsample(p1,line.pts,'regular')
  
  # data to add
  df <- p1@data[rep(1,length(dest.pt)),]
  # convert to spatialPointsDataFrame
  dest.pt <- SpatialPointsDataFrame(dest.pt,data=df)
  
  return(dest.pt)
}


FindNearestEuclidean <- function(orig.pts,dest.pts){
  # finds the nearest neighbors to the origin
  nearest.neighbor <- get.knnx(dest.pts@coords[,1:2],orig.pts@coords,k=kNearestNeighbors)
  
  # get decimal degree lat and lon for both
  CRS.new <- CRS("+init=epsg:4326") # WGS 84
  orig.pts <- spTransform(orig.pts,CRS.new)
  dest.pts <- spTransform(dest.pts,CRS.new)
  
  # compiles a table in preparation for queries
  # orig_id | dest_id | orig_lon | orig_lat | dest_lon | dest_lat | euclidean
  gdest.ids = c(t(nearest.neighbor$nn.index))
  orig.ids = rep(seq(dim(nearest.neighbor$nn.index)[1]),each = kNearestNeighbors)
  dest.rank = rep(seq(kNearestNeighbors),times = dim(nearest.neighbor$nn.index)[1])
  near.table <- data.table('orig_id' = orig.ids,
                               'dest_id' = dest.pts@data$dest_id[gdest.ids],
                               'gdest_id' = gdest.ids,
                               'rank' = dest.rank,
                               'orig_lon' = orig.pts@coords[orig.ids,1],
                               'orig_lat' = orig.pts@coords[orig.ids,2],
                               'dest_lon' = dest.pts@coords[gdest.ids,1],
                               'dest_lat' = dest.pts@coords[gdest.ids,2],
                               'euclid' = c(t(nearest.neighbor$nn.dist)))
  
  print('Near table created')
  fwrite(near.table,file = filename.neartable)
  return(near.table)
}


DoQuerying <- function(near.table,osrm.url,query.indx){
  
  if (kNoCores > 1) {
    # if it's parallel
    print('OSRM parallel querying begun')
    outfile.progress <- paste(filename.out,'_progress.txt',sep="")
    num.splits <- 100
    
    # divide into groups
    near.table[,'query_group'] <- near.table[query.indx:.N,.(ceiling(query.indx:.N/(.N/num.splits)))] 

    # parallelise queries
    cl <- makeCluster(kNoCores,outfile=outfile.progress)
    clusterExport(cl, c('near.table',"osrm.url","GetSingleTravelInfo","QueryOSRM"), envir=environment())
    clusterEvalQ(cl, c(library(httr),library(pbapply),library(data.table)))
    # duration and distance for travel
    travel.queries = pbsapply(seq(1,num.splits),function(j) QueryOSRM(j, osrm.url), cl = cl)
    parallel::clusterSetRNGStream(cl, iseed = 0L)
    stopCluster(cl)
    

      
  } else {
    print('OSRM querying begun')
    near.table <- near.table[query.indx:.N,]
    travel.queries <- QueryOSRM(near.table, osrm.url)
  }
  
  # unlist results
  travel.info = t(matrix(unlist(travel.queries),nrow=2))
  
  # add results to the near.table
  near.table[,'duration'] = travel.info[,1]
  near.table[,'distance'] = travel.info[,2]
  
  return(near.table)
}
  

QueryOSRM <- function(j, osrm.url){
  # query the routing algorithm to determine the travel distance and duration
  
  near.table <- near.table[query_group==j,]
  # number of queries
  query.no <- dim(near.table)[1]
  
  # do queries
  travel.queries = sapply(seq(1,query.no),function(i) GetSingleTravelInfo(near.table[i,], osrm.url))

  return(travel.queries)
}


GetSingleTravelInfo <- function(route, osrm.url){
  # query OSRM once and return the travel distance and duration
  # route <- near.table[j,]
  orig.lat <- route[,'orig_lat']
  orig.lon <- route[,'orig_lon']
  dest.lon <- route[,'dest_lon']
  dest.lat <- route[,'dest_lat']

  # get the url  
  query.url <- sprintf('%s/route/v1/walking/%.6f,%.6f;%.6f,%.6f?overview=false',osrm.url, orig.lon, orig.lat, dest.lon, dest.lat)
  
  # return the parsed json  
  result <- content(GET(query.url),"parsed")
  
  # extract time and distance
  duration <- result$routes[[1]]$legs[[1]]$duration
  distance <- result$routes[[1]]$legs[[1]]$distance
  
  result <- c('duration' = duration, 'distance' = distance)
  return(result)
}


WriteResults <- function(result.table){
  
  
  # write the full result table to csv
  fwrite(result.table, file = filename.full, row.names=FALSE)
  
  # determine closest
  closest.table <- DetermineClosest(result.table)
  
  # write closest to csv
  fwrite(closest.table,file=paste(filename.out,'_results.csv', sep = ""), row.names=FALSE)
  
  # convert closest table to spatialdataframe
  # get the coordinates
  coords <- closest.table[,c('orig_lon','orig_lat')]
  # create the spatial points dataframe
  closest.spdf <- SpatialPointsDataFrame(coords = coords, data = closest.table, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
  
  # write closest to shapefile
  filename.shp <- paste(filename.out,'_results', sep = "")
  WriteShp(closest.spdf,filename.shp)
}


DetermineClosest <- function(result.table){
  # determines the closest destination by OSRM distance for each origin
  
  # convert the routed distance into a matrix
  distance.matrix <- matrix(result.table$distance, ncol = kNearestNeighbors,byrow = TRUE)
  
  # determine the column index for the minimum for each origin (row)
  distance.indx <- apply( distance.matrix, 1, which.min)
  
  # subset the neartable to take only these indices
  # first, get the indices for the neartable
  resulttable.indx <- sapply(seq(distance.indx), function(i) distance.indx[i] + kNearestNeighbors*(i - 1))
  # get the rows from the neartable
  closest.table <- result.table[resulttable.indx,]
  
  return(closest.table)
}


WriteShp <- function(spatial.dataframe,filename.shp){
  # write the data to shapefile
  
  # get the saving directory and filename
  save_dir <- getwd()
  
  # write the shapefile. we use both methods because one doesn't write 
  # the projection file and the other modfies the colnames
  writeOGR(spatial.dataframe,save_dir, filename.shp, driver = 'ESRI Shapefile')
  writeSpatialShape(spatial.dataframe, paste(save_dir,'/',filename.shp,sep=''))
}
