.onAttach <- function(libname, pkgname) {
  # to show a startup message
  inicio<- c("

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                            Tlamatini - R package                         ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ \n\n")

  insight::print_color(inicio, "cyan")
  #insight::print_color("  ", "cyan")


  texto<- paste("El paquete Tlamatini se ha cargado con \u00e9xito. \u00a1Espero que lo disfrutes!\n", "Actualizaciones y m\u00e1s informaci\u00f3n en:
              \n  https://mariosandovalmx.github.io/tlamatini-website/\n\n", "Email de contacto: sandoval.m@hotmail.com \n", sep=" ")
  insight::print_color(texto, "cyan")

  texto2<- paste("Para citar este paquete: Mario A. Sandoval-Molina (2021). tlamatini: Funciones utiles \npara biologxs y ecologxs confundidos con los modelos lineales. R package. https://doi.org/10.5281/zenodo.7765347", sep=" ")
  insight::print_color(texto2, "green")


}

.onLoad <- function(libname, pkgname) {
  # something to run
}
