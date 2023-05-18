#' Probar optimizadores en modelo glmmTMB
#'
#' Esta es una función que permite ajustar GLMM con diferentes optimizadores cuando existen problemas
#' de convergencia.
#'
#' @param model Modelo GLMM ajustado con la paqueteria glmmTMB.
#' @param optimizer Optimizador que se usa en el modelo, puede ser "optim" o "nlminb".
#' @return Lista con los diferentes optimizadores y su ajuste. Si es FALSE, el modelo no converge.
#' @export
#'
#' @examples
#' # do not run
#' #datos <- datasets::ChickWeight
#' #library(glmmTMB)
#' #modelo <- glmmTMB(weight ~ Diet +(1|Chick), family=gaussian("log"), data = datos)
#' # optimizeGLMM(modelo, optimizer = "optim")
#' # optimizeGLMM(modelo, optimizer = "nlminb")
#' @encoding UTF-8
#' @importFrom performance check_convergence
optimizeGLMM <- function(model, optimizer= c("optim","nlminb") ){
  if(missing(optimizer)) optimizer <- "Unspecified"
  mod <- model
  optmod <- c("optim","nlminb")
  optmethod <- c("BFGS", "L-BFGS-B", "Nelder-Mead", "CG", "SANN")


  if(optimizer == "optim"){
    insight::print_color("Optimizer = optim", "red")

    for(i in 1:length(optmethod)) {
      suppressWarnings(tryCatch(
        expr = {
          message( op.mod <- stats::update(mod, control=  glmmTMB::glmmTMBControl(optimizer=optmod[1], optArgs=list(method=optmethod[i]))))
          message("Model OK")
        },
        error = function(e){

        },
        warning = function(w){
        },
        finally = {
          message('.')
        }
      )    )


      ##################################
      mt <- optmethod[i]
      tf <- performance::check_convergence(op.mod)
      convtext <- paste0(mt,":", "Convergence ", tf ," ")
      insight::print_color(convtext, "green")

    }




  } else if(optimizer == "nlminb"){
    insight::print_color("Optimizer = nlminb", "red")

    for(i in 1:length(optmethod)) {
      suppressWarnings(tryCatch(
        expr = {
          message( op.mod <- stats::update(mod, control= glmmTMB::glmmTMBControl(optimizer=optmod[2], optArgs=list(method=optmethod[i]))))
          message("Model OK")
        },
        error = function(e){
        },
        warning = function(w){
        },
        finally = {
          message('.')
        }
      )    )


      ##################################
      mt <- optmethod[i]
      tf <- performance::check_convergence(op.mod)
      convtext <- paste0(mt,":", "Convergence ", tf ," ")
      insight::print_color(convtext, "green")

    }


  } else{
    insight::print_color("You should specify an optimizer.", "green")
  }


}
