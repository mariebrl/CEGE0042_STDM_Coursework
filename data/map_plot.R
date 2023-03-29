# This funtction takes 3 main arguments (st_object, required attribute for plotting, title for the legend)
# The rest of the arguments are optional which are (style: change the breaks, palette, out_path, savemap)
# if save map is TRUE map will save in a file that is appropriate for the word report
# the functions returns a tm object for tmap ready to visualise or save
mapReport <- function(st, att, maintitle, legendtitle = "" ,style="quantile",palette = "PuBu", out_path="map.png", savemap=FALSE) {
  tm<-tm_shape(st) +
    tm_borders(col = "black",lwd = 0.25) +
    tm_polygons(col=att,
                style=style,
                palette = palette) +
    tm_scale_bar(position=c("RIGHT", "bottom")) +
    tm_compass(position=c(0.9, 0.9), size=1.5) +
    tm_layout(legend.position=c("left", "top"),
              scale = 1,
              legend.width = 1,
              outer.margins=c(0.06,0,0.06,0), # bottom, left, top, and right margin
              inner.margins=c(0.0,0,0.1,0),
              between.margin=0,
              frame = FALSE,
              title.snap.to.legend = TRUE,
              title = legendtitle,
              main.title = maintitle,
              main.title.size = 1.5,
              main.title.position = c("center", "top")
    )
  if (savemap){
    tmap_save(tm, filename = out_path ,width = 1495,dpi=500)
  }
  return(tm)
}