### simple simulation...
library(magrittr)
library(sp)
library(rgdal)
library(ltdesigntester)

### simple region shapefile
region_big <- data.frame(x=c(0,0,3,3,0),
                         y=c(0,2.5,2.5,0,0))
R <- matrix(c(cos(pi/2), sin(pi/2), -sin(pi/2), cos(pi/2)),2,2)
region_big[,c("x","y")] <- t(R %*% t(region_big[,c("x","y")]))


#plot(region_big, asp=1, type="l")

region2shp <- function(region, file){
  region <- region %>%
              Polygon %>% list %>%
              Polygons(ID="1") %>% list %>%
              SpatialPolygons %>%
              SpatialPolygonsDataFrame(data=data.frame(z=1))
  writeOGR(region, file, "data", "ESRI Shapefile" )
}
region2shp(region_big, "shapes/region_big")


## make a zigzag
n_segs <- 10
zz <- data.frame(x   = c(seq(0, 0.5, len=n_segs),
                         seq(0.5, 1, len=n_segs)),
                 y   = c(seq(0, 1, len=n_segs),
                         seq(1, 0, len=n_segs)),
                 leg = c(rep("1", n_segs),
                         rep("2", n_segs)))
# many zigzags
mzz <- rbind(zz,zz,zz)
mzz$x <- mzz$x/3
ind <- 1:nrow(zz)
mzz$x[ind+nrow(zz)] <- mzz$x[ind+nrow(zz)]+1/3
mzz$x[ind+2*nrow(zz)] <- mzz$x[ind+2*nrow(zz)]+2/3
# labels
mzz$leg <- as.numeric(mzz$leg)
mzz$leg[ind+nrow(zz)] <- mzz$leg[ind+nrow(zz)]+2
mzz$leg[ind+2*nrow(zz)] <- mzz$leg[ind+2*nrow(zz)]+4
mzz$leg <- as.character(mzz$leg)
# stretch out over 0,3
mzz2 <- mzz
mzz2$x <- mzz$x*3
mzz2$y <- mzz2$y + 1.5

# add bottom zig(zag)
zzll <- zz
zzll$x <- zzll$x*3
zzll$leg <- c(rep("7", n_segs),
              rep("8", n_segs))
tb <- rbind.data.frame(mzz2, zzll)
tb[,c("x","y")] <- t(R %*% t(tb[,c("x","y")]))

#lines(tb[,c("x","y")], type="l", asp=1)
write_transects(tb, "shapes/tb")



### now the corner
region_sq <- data.frame(x=c(0,0,3,3,0),
                        y=c(0,3,3,0,0))

plot(region_sq, asp=1, type="l")

n_segs <- 3
zz <- data.frame(y   = c(seq(0, 0.25, len=n_segs),
                         seq(0.25, 0.5, len=n_segs)),
                 x   = c(seq(0, 0.5, len=n_segs),
                         seq(0.5, 0, len=n_segs)))
mzz <- rbind(zz,zz,zz,zz,zz)
mzz$y <- mzz$y + sort(rep(seq(0.5,2.5, by=0.5), 2*n_segs))
mzz$leg <- sort(rep(seq(1,10, by=1), 3))
mzz$x <- mzz$x +0.25
lines(mzz[,c("x","y")])


zzu <- data.frame(x   = c(seq(0, 0.25, len=n_segs),
                          seq(0.25, 0.5, len=n_segs)),
                  y   = c(seq(0.5, 0, len=n_segs),
                          seq(0, 0.5, len=n_segs)))

mzzu <- rbind(zzu, zzu, zzu, zzu, zzu)
mzzu$x <- mzzu$x + sort(rep(seq(0.25,2.25, by=0.5), 2*n_segs))
mzzu$leg <- 10+sort(rep(seq(1,10, by=1), 3))
lines(mzzu[,c("x","y")])
corner <- rbind.data.frame(mzz, mzzu)

region2shp(region_sq, "shapes/region_sq")

write_transects(corner, "shapes/corner")
