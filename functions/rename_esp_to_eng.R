
recode_spanish_provinces <- function(old){
  new <- old
  new[new=="Ciudad Autónoma de Melilla"] <- 'Ciudad Autonoma de Melilla'
  new[new=="Andalucía"] <- 'Andalucia'
  new[new=='Canarias'] <- 'Canarias'
  new[new=='Castilla-La Mancha'] <- 'Castilla-la Mancha'
  new[new=="Cataluña"] <- 'Cataluna'
  new[new=='Galicia'] <- 'Galicia'
  new[new=='La Rioja'] <- 'La Rioja'
  new[new=='Illes Balears'] <- 'Illes Balears'
  new[new=="País Vasco"] <- 'Pais Vasco'
  new[new=="Castilla y León"] <- 'Castilla y Leon'
  new[new=='Extremadura'] <- 'Extremadura'
  new[new=="Ciudad Autónoma de Ceuta"] <- 'Ciudad Autonoma de Ceuta'
  new[new=='Comunidad Foral de Navarra'] <- 'Comunidad Foral de Navarra'
  new[new=="Aragón"] <- 'Aragon'
  new[new=='Comunitat Valenciana'] <- 'Comunidad Valenciana'
  new[new=="Territorio no asociado a ninguna autonomía"] <- ''
  new[new=='Comunidad de Madrid'] <- 'Comunidad de Madrid'
  new[new=='Cantabria'] <- 'Cantabria'
  new[new=='Principado de Asturias'] <- 'Principado de Asturias'
  new[new=="Región de Murcia"] <- 'Region de Murcia'
  
  return(new)
}

### END #######################################################################