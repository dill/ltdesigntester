### simple simulation...
library(magrittr)
library(sp)
library(rgdal)
library(designtester)

## make a zigzag
n_segs <- 10
zz <- data.frame(x   = c(seq(0, 0.5, len=n_segs),
                         seq(0.5, 1, len=n_segs)),
                 y   = c(seq(0, 1, len=n_segs),
                         seq(1, 0, len=n_segs)),
                 leg = c(rep("1", n_segs),
                         rep("2", n_segs)))

##plot(zz)
write_transects(zz, "shapes/zig")


# many zigzags

mzz <- rbind(zz,zz,zz)

mzz$x <- mzz$x/3
ind <- 1:nrow(zz)
mzz$x[ind+nrow(zz)] <- mzz$x[ind+nrow(zz)]+1/3
mzz$x[ind+2*nrow(zz)] <- mzz$x[ind+2*nrow(zz)]+2/3


mzz$leg <- as.numeric(mzz$leg)
mzz$leg[ind+nrow(zz)] <- mzz$leg[ind+nrow(zz)]+2
mzz$leg[ind+2*nrow(zz)] <- mzz$leg[ind+2*nrow(zz)]+4
mzz$leg <- as.character(mzz$leg)

# stretch out over 0,3
mzz2 <- mzz
mzz2$x <- mzz$x*3

#plot(mzz[,c("x","y")], type="l", asp=1)
write_transects(mzz2, "shapes/manyzigzags")

# lots of effort in the west
l <- data.frame(x = c(rep(0.1, 5),
                      rep(0.25, 5),
                      rep(0.4, 5)),
                y = rep(seq(0.2, 0.8, len=5),3),
                leg = c(rep("1", 5),
                        rep("2", 5),
                        rep("3", 5)))

#plot(l)
write_transects(l, "shapes/leftie")


## many zig-zags then a lone transect /\/\/    |

zzl <- rbind.data.frame(mzz,
                        data.frame(x   = rep(2.8, 10),
                                   y   = seq(0, 1, len=10),
                                   leg = rep(as.character(max(as.numeric(mzz$leg))+1), 10)))
write_transects(zzl, "shapes/zzl")

#                _
# IWC    | \/\/ |_



i <- data.frame(x   = rep(0.25, 10),
                y   = seq(0, 1, len=10),
                leg = rep(1, 10))

w <- data.frame(x   = c(zz$x*0.5, zz$x*0.5+0.5)+0.75,
                y   = 1-c(zz$y, zz$y),
                leg = rep(2, 2*length(zz$x)))

ce <- data.frame(x   = c(seq(2.8, 2.5, len=5),
                         seq(2.5, 2.3, len=5), seq(2.3, 2.5, len=5),
                         seq(2.5, 2.8, len=5)),
                 y   = c(rep(0.02, 5),
                         seq(0.02, 0.98, len=10),
                         rep(0.98, 5)),
                 leg = rep(3, 20))

iwc <- rbind.data.frame(i, w, ce)
iwc <- unique(iwc)

write_transects(iwc, "shapes/iwc")


### simple region shapefile
region <- data.frame(x=c(0,0,1,1,0),
                     y=c(0,1,1,0,0))

region2shp <- function(region, file){
  region <- region %>%
              Polygon %>% list %>%
              Polygons(ID="1") %>% list %>%
              SpatialPolygons %>%
              SpatialPolygonsDataFrame(data=data.frame(z=1))
  writeOGR(region, file, "data", "ESRI Shapefile" )
}

region2shp(region, "shapes/region")

### simple region shapefile
region2 <- data.frame(x=c(0,0,3,3,0),
                      y=c(0,1,1,0,0))
region2shp(region2, "shapes/region2")


