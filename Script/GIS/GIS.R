library(ggmap)
library(ggplot2)
library(raster)
library(patchwork)

# Data Load
korea <- shapefile('/Users/jch/Documents/github/P3K-HB_Demography/Lecture/MA1-1 Report/GIS/Vector/KOR_adm/KOR_adm1.shp')
test_before <- read.csv('/Users/jch/Desktop/before.csv')
test_after <- read.csv('/Users/jch/Desktop/after.csv')
StrongSettle <- read.csv('/Users/jch/Documents/github/P3K-HB_Demography/Data/StrongholdsSettlement.csv')
StrongSettle_before <- subset(StrongSettle, type == "before")
StrongSettle_after <- subset(StrongSettle, type == "after")
StrongSettle_central <- subset(StrongSettle, Site == "서울 풍납토성")

# Point Plot
# Before A.D. 250
before_point <- ggplot() +
    geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black') +
    coord_map(xlim=c(126, 129.5), ylim=c(36.8, 38.525)) +
    geom_point(data=test_before, aes(x=test_before$X, y=test_before$Y), color='red') +
    ggtitle('Before A.D. 300')
# After A.D. 250
after_point <- ggplot() +
    geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black') +
    coord_map(xlim=c(126, 129.5), ylim=c(36.8, 38.525)) +
    geom_point(data=test_after, aes(x=test_after$X, test_after$Y), color='blue') +
    ggtitle('After A.D. 300')

before_point + after_point
ggsave("/Users/jch/Documents/github/P3K-HB_Demography/Graph/GIS_point.tiff", dpi=750, dev='tiff', height=5, width=11, units="in")

# KDE
# Before A.D. 250
before_kde <- ggplot() +
    geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black') +  
    coord_map(xlim=c(126, 129.5), ylim=c(36.8, 38.525)) +
    stat_density_2d_filled(data=test_before, aes(x=X, y=Y, alpha=..level..), n=150) +
    geom_point(data=StrongSettle_before, aes(x=StrongSettle_before$X, y=StrongSettle_before$Y), fill='black', color='black', shape=22, size=2.5) +
    ggtitle('KDE(Before A.D. 300)') +
    labs(alpha="level", fill="level")
# After A.D. 250
after_kde <- ggplot() +
    geom_polygon(data=korea, aes(x=long, y=lat, group=group), fill='white', color='black') +  
    coord_map(xlim=c(126, 129.5), ylim=c(36.8, 38.525)) +
    stat_density2d_filled(data=test_after, aes(x=test_after$X, y=test_after$Y, alpha=..level..), n=150) +
    geom_point(data=StrongSettle_after, aes(x=StrongSettle_after$X, y=StrongSettle_after$Y), fill='black', color='black', shape=22, size=2.5) +
    geom_point(data=StrongSettle_central, aes(x=StrongSettle_central$X, y=StrongSettle_central$Y), fill='red', color='black', shape=22, size=3) +
    ggtitle('KDE(After A.D. 300)') +
    labs(alpha="level", fill="level")
before_kde + after_kde
ggsave("/Users/jch/Documents/github/P3K-HB_Demography/Graph/GIS_KDE.tiff", dpi=750, dev='tiff', height=5, width=11, units="in")
