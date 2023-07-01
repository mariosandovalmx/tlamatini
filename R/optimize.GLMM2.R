#' Probar optimizadores en modelo glmmTMB
#'
#' Esta es una función que permite ajustar GLMM usando una fomula y tus datos probando con diferentes optimizadores cuando existen problemas de convergencia. A veces al ajustar modelos pueden arrojar errores. Por ejemplo, una advertencia ‘iteration limit reached without convergence’, se soluciona aumentando el número de iteraciones: 'glmmTMBControl(optCtrl=list(iter.max=1e3,eval.max=1e3))'. Vea la ayuda de la función glmmTMBControl para mas detalles.
#'
#' @param formula Formula del modelo que queremos ajustar, incluyendo el componente aleatorio si es que hay. Por ejemplo: "y ~ x+z"
#' @param data Base de datos que se usa para ajustar el modelo.
#' @return Lista con los diferentes optimizadores y su ajuste. Si es FALSE, el modelo no converge.Si es TRUE, podemos ajustar ese modelo con ese optimizador.
#' @export
#'
#' @examples
#' # do not run
#' #datos <- ChickWeight
#' #library(glmmTMB)
#' #modelo <- glmmTMB(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' # optimizeGLMM2(formula = p_azucar ~ tratamiento * dia, data = datos)
#' @encoding UTF-8
#' @importFrom performance check_convergence
#' @importFrom insight print_color
#' @import glmmTMB
optimizeGLMM2 <- function(formula, data){
  #if(missing(optimizer)) optimizer <- "Unspecified"
  datos<- datos
  form <- as.formula(formula)

  optmod <- c("optim","nlminb")
  optmethod <- c("BFGS", "L-BFGS-B", "Nelder-Mead", "CG", "SANN")
  # create vectors for convergence  output
  output <- c()
  output2 <- c()

  pb <- utils::txtProgressBar(min = 0, max = 5, style = 3)

  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                                    optim                                 ----
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  for(i in 1:length(optmethod)) {

    utils::setTxtProgressBar(pb, i)
    # detectar errores y pasal al siguiente optimizador
    skip_to_next <- FALSE
    tryCatch( suppressWarnings( op.mod <- glmmTMB::glmmTMB(form, data = datos, control=  glmmTMBControl(optimizer=optmod[1], optArgs=list(method=optmethod[i]))) )
              , error = function(e) { skip_to_next <<- TRUE})

    if(skip_to_next) {
      mt <- optmethod[i]
      tf <- "FALSE"
      convtext <- paste0(mt,":", "Convergence ", tf ," ")
      #insight::print_color(convtext, "green")
      output <- c(output, convtext)
      next } else {


        #suppressWarnings(
        # op.mod <- glmmTMB::glmmTMB(form, data = datos, control=  glmmTMBControl(optimizer=optmod[1], optArgs=list(method=optmethod[i])))
        #)


        ##################################
        mt <- optmethod[i]
        tf <- performance::check_convergence(op.mod)
        convtext <- paste0(mt,":", "Convergence ", tf ," ")
        #insight::print_color(convtext, "green")
        output <- c(output, convtext)
      }



    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ##                                   nlminb                                 ----
    ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    #suppressWarnings(
    #  op.mod2 <- glmmTMB::glmmTMB(form, data = datos, control=  glmmTMBControl(optimizer=optmod[2], optArgs=list(method=optmethod[i])))
    #)
    # detectar errores y pasal al siguiente optimizador
    skip_to_next <- FALSE
    tryCatch( suppressWarnings( op.mod2 <- glmmTMB::glmmTMB(form, data = datos, control=  glmmTMBControl(optimizer=optmod[1], optArgs=list(method=optmethod[i]))) )
              , error = function(e) { skip_to_next <<- TRUE})

    if(skip_to_next) {
      mt <- optmethod[i]
      tf <- "FALSE"
      convtext <- paste0(mt,":", "Convergence ", tf ," ")
      #insight::print_color(convtext, "green")
      output <- c(output, convtext)
      next } else {


        ##################################
        mt2 <- optmethod[i]
        tf2 <- performance::check_convergence(op.mod2)
        convtext2 <- paste0(mt2,":", "Convergence ", tf2 ," ")
        #insight::print_color(convtext, "green")
        output2 <- c(output2, convtext2)
      }

  }
  my_bind <- function(x, y){
    if(length(x = x) > length(x = y)){
      len_diff <- length(x) - length(y)
      y <- c(y, rep(NA, len_diff))
    }else if(length(x = x) < length(x = y)){
      len_diff <- length(y) - length(x)
      x <- c(x, rep(NA, len_diff))
    }
    cbind(x, y)
  }

  df.out <- my_bind(output,output2)
  df.out <- as.data.frame(df.out)
  names(df.out) <- c("optim", "nlminb")
  cat("\n")
  insight::print_color("Optimizadores del modelo:", "green")
  print.data.frame(df.out, row.names = FALSE, right=FALSE)

  #opts <- options(knitr.kable.NA = "")
  #df.out <- knitr::kable(df.out, "rst")
  #return(df.out)

}

