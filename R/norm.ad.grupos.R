#' Prueba de normalidad de Anderson-Darling por grupos.
#'
#' Realiza la prueba de Anderson-Darling para probar la normalidad de una
#' variable por grupos.
#'
#' @param formula Formula iniciando por la variable numerica de nuestro interes.
#' Puede tomar forma de y ~ x+z. Se pueden usar 2 o mÃ¡s grupos.
#' @param datos Base de datos o dataframe.
#'
#' @return Una prueba Anderson-Darling por grupos, un valor de p y estadistico asociado a
#' la prueba.
#' @export
#'
#' @examples
#' #data(iris)
#' #norm.ad.grupos(Petal.Length~ Species, iris)
#' #iris$site <- c(rep("A", 75), rep("B", 75))
#' #norm.ad.grupos(Petal.Length~ Species+ site, iris)
#' @encoding UTF-8
#' @importFrom stats aggregate
#' @importFrom stats as.formula
#' @importFrom nortest ad.test
#' @importFrom ggpubr ggqqplot

norm.ad.grupos <- function(formula, datos){

  datos<- datos
  form <- as.formula(formula)

  fac<- c(form[[3]])
  fac2 <-fac[[1]]


  if(length(fac2)<= 1){
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                            una variables                           ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



    resp.var<- as.character(form)[2]
    fac<- c(form[[3]])
    fac2 <-fac[[1]]
    fac2
    cols <- stringr::str_split(fac2,",")
    cols<- cols[[1]]
    cols
    # create a new column `x` with the three columns collapsed together
    #datos$grps <- apply(datos[ , cols ] , 1 , paste , collapse = "-" )

    datos$grps <- datos[ , cols ]

    #####  estadisticos
    #  calcular el valor de W
    res<- aggregate(form, data = datos, FUN =
                      function(x) ad.test(x)$statistic)
    res
    names(res)[names(res) == resp.var] <- "statistic"
    res
    #calcular el valor de p
    resb<- aggregate(form, data = datos, FUN =
                       function(x) ad.test(x)$p.value)
    names(resb)[names(resb) == resp.var] <- "p.value"
    resb
    #unir los dos resultados en una tabla
    results<- merge(x = res, y = resb, all.x = TRUE)
    insight::print_color("Prueba de normalidad de Anderson-Darling por grupos. ", "green")


    ####### grafico
    auto_mfrow(nrow(res), setup = TRUE)
    # graficar qqplot
    pl<-ggpubr::ggqqplot(datos, x = resp.var,
                     facet.by =  "grps")
    print(pl)


    # tabla resultados
    insight::export_table(results, align= "right", title = "--", format = "markdown")



  } else {

    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                            dos o mas variables                           ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    resp.var<- as.character(form)[2]
    fac<- c(form[[3]])
    fac2 <-fac[[1]]
    fac2
    fac3<-paste(fac2[-1], collapse = ",")
    fac4<- fac3
    fac4
    cols <- stringr::str_split(fac4,",")[[1]]
    cols

    # crear nueva columna con los factores agrupados
    datos$grps <- apply(datos[ , cols ] , 1 , paste , collapse = "-" )



    #####  estadisticos
    #  calcular el valor de W
    res<- aggregate(form, data = datos, FUN =
                      function(x) ad.test(x)$statistic)
    res
    names(res)[names(res) == resp.var] <- "statistic"
    res
    #calcular el valor de p
    resb<- aggregate(form, data = datos, FUN =
                       function(x) ad.test(x)$p.value)
    names(resb)[names(resb) == resp.var] <- "p.value"
    resb
    #unir los dos resultados en una tabla
    results<- merge(x = res, y = resb, all.x = TRUE)
    insight::print_color("Prueba de normalidad de Anderson-Darling por grupos. ", "green")


    ####### grafico
    auto_mfrow(nrow(res), setup = TRUE)
    # graficar qqplot
    pl<- ggpubr::ggqqplot(datos, x = resp.var,
                     facet.by =  "grps")
    print(pl)


    # tabla resultados
    insight::export_table(results, align= "right", title = "--", format = "markdown")




  }

}

