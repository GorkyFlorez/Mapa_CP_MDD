

library(sf)
library(ggplot2)
library(cartomisc)
library(ggspatial)
library(raster)
library(ggsn)
library(ggmap)
library(ggspatial)
Via   = read_sf("Shp/Via.geojson")
Re   = read_sf("Shp/Red_vial.geojson")
CPP   = read_sf("Shp/CP.geojson")
MD   = read_sf("Shp/MDD.geojson")

Vias <- st_transform(Via  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
Red <- st_transform(Re   ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
CP  <- st_transform(CPP  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))
MDD <- st_transform(MD ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurAmerica = st_read("SHP/SurAme.geojson")  
SurAmeric <- st_transform(SurAmerica  ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

Peru          <- getData('GADM', country='Peru', level=0) %>% st_as_sf()

Vias_bufe<- Vias %>% 
  st_buffer(units::set_units(10, km))

ggplot()+
  geom_sf(data = Vias_bufe)

Vias_merge =st_union(Vias_bufe)  
write_sf(Vias_merge, "Shp/Via_merge.shp")
write_sf(Red, "Shp/ViasDF.shp")
CPXY <- cbind(CP, st_coordinates(st_centroid(CP$geometry)))
library(maptools)
Via_merge = readShapePoly("Shp/Via_merge.shp")
ViasDF = readShapeLines("Shp/ViasDF.shp")

MDDgra  =ggplot()+
  geom_sf(data = MDD, fill="black", color="black")+
  theme_void()
MDDgra
MDDgra.grob  <- ggplotGrob(MDDgra) 

SurAmericaa=  ggplot()+
  geom_sf(data = SurAmeric, fill=NA, color="black", size=0.6)+
  geom_sf(data = Peru , fill="gray", color="black")+
  geom_sf(data = MDD, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        panel.border = element_rect( color = "grey20", fill = NA))+
  annotation_custom(MDDgra.grob, xmin = -50, xmax = -40, ymin =-55, ymax=-40)+
  geom_segment(aes(x=-70, xend=-49,y=-14, yend=-50), 
               linetype = "dashed", color = "black", size = 0.6)+
  geom_segment(aes(x=-70, xend=-46,y=-14, yend=-43), 
               linetype = "dashed", color = "black", size = 0.6)+
  annotate(geom = "text", x = -46, y = -38, hjust = 0, vjust = 1, 
           label = "Madre de Dios",size = 2, family="serif", color = 
             "black",  fontface="italic")
SurAmericaa
SurAmericaa.grob  <- ggplotGrob(SurAmericaa)  


ca1 <- get_map(location = c(-69.70, -12.8 ,-69.15, -12.25), 
               maptype = "terrain", source = "google", force = T)

Mapa =ggmap(ca1) +
  geom_point(data = CPXY ,aes(X, Y),size = 2, pch=21, color="black", fill="red")+
  geom_path(data=ViasDF,size=1, color="#1d3557",
            aes(x=long,y=lat,group=group))+
  labs(title = "", subtitle="",
        caption="Gorky Florez",x="Longitud",y="Latitud",tag="")+
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  theme(legend.position = "none",
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a9def9"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  annotate(geom = "text", x = -69.6, y = -12.7, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo                       Gorky Florez Castillo                        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)+
  annotate(geom = "text", x = -69.6, y = -12.4, hjust = 0, vjust = 1,
           label = "Mapa de la los centros poblados de la Carretera \n          Interoceánica suroeste de la Amazonia",
           size = 4, family="serif", color = "black",  face = "bold", fontface="italic")
  


ggsave(plot=Mapa,"Mapa/Mapa de CP2.png",units = "cm",width = 29, #alto
       height = 29, #ancho
       dpi=1200)
















































