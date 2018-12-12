library("GWmodel")
library("robustbase")
library('sp')
library('spgwr')
library('raster')
library('ggplot')

houses = read.csv("/Users/gentydaku/Downloads/stat139project-master 2/DC_DATA.csv")

model1 = lm(PRICE~FAGI_Total, houses)

resids = residuals(model1)
colors = c('dark blue','blue','red','dark red')
map.resids = SpatialPointsDataFrame(data=data.frame(resids),coords=cbind(houses$X,houses$Y))
spplot(map.resids,cuts=quantile(resids),col.regions=colors, cex=1)

GWRbandwidth = gwr.sel(PRICE ~ Crime, data=houses, coords=cbind(houses$X,houses$Y), adapt=T)

# gwr.model = gwr(PRICE ~ Crime, data=houses, coords=cbind(houses$X,houses$Y), adapt = GWRbandwidth, hatmatrix=TRUE, se.fit=TRUE)
gwr.model = gwr(PRICE~Crime, data = houses, coords = cbind(houses$X, houses$Y), bandwidth = 0.01315562)

results = as.data.frame(gwr.model$SDF)

house$data = results$Crime

dc_shapefile = readOGR("/Users/gentydaku/Downloads/tl_2016_11_cousub/tl_2016_11_cousub.shp")
line = fortify(dc_shapefile, region="name")

gwr.point1 = ggplot(house, aes(x=X,y=Y) + geom_point(aes(colour=house$data)) + 
                      scale_colour_gradient2(low = 'red', mid = 'white', high = 'blue', midpoint = 0, space = 'rgb',
                                             na.value = 'grey50',guide='colourbar',guide_legend(title='Coefs')))

gwr.point1+geom_path(data=line,aes(long,lat,group=id),colour='grey')+coord_equal()
