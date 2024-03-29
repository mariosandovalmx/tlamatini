#' Función para determinar la familia de distribución y la función de enlace en el objeto 'mer'
#' @description
#' función de utilidad para determinar la familia de distribución y la función de enlace en el objeto de la clase 'mer'.
#' @param mod Modelo
#' @note Función tomada de la paquetería AICcmodavg.
#' @return familia de distribución y link function
#' @keywords internal
#' @export

fam_link_mer <- function(mod) {

  if(identical(paste(class(mod)), "mer")) {

    call.mod <- mod@call

    ##determine type of call:  lmer( ) vs glmer( )
    fun.call <- call.mod[1]

    ##supported link
    supp.link <- "yes"

    ##for lmer
    if(identical(as.character(fun.call), "lmer")) {
      fam.type <- "gaussian"
      link.type <- "identity"
    }

    ##for glmer
    if(identical(as.character(fun.call), "glmer")) {
      fam.call <- call.mod$family

      ##determine family of glmm and set to canonical link
      if(!is.na(charmatch(x = "binomial", table = fam.call))) {
        fam.type <- "binomial"
        link.type <- "logit"
      } else {
        if(!is.na(charmatch(x = "poisson", table = fam.call))) {
          fam.type <- "poisson"
          link.type <- "log"
        } else {
          if(!is.na(charmatch(x = "Negative Binomial", table = fam.call))) {
            fam.type <- "Negative.Binomial"
            link.type <- "log"
          } else {
            if(!is.na(charmatch(x = "gaussian", table = fam.call))) {
              fam.type <- "gaussian"
              link.type <- "identity"
            } else {
              if(!is.na(charmatch(x = "Gamma", table = fam.call))) {
                fam.type <- "Gamma"
                link.type <- "log"
              } else {fam.type <- "other"}
            }
          }
        }

        ##check for family type other than binomial, Poisson, normal, negative binomial, or Gamma
        if(identical(fam.type, "other")) stop("\nThis distribution family is not yet supported\n")

        ##determine if canonical link was used
        if(length(fam.call) > 1){
          link.type <- as.character(fam.call$link)
        }


        ##check for links supported by this function
        if(identical(fam.type, "binomial")) {
          if(!identical(link.type, "logit")) supp.link <- "no"
        }

        if(identical(fam.type, "poisson")) {
          if(!identical(link.type, "log") && !identical(link.type, "identity")) supp.link <- "no"
        }

        if(identical(fam.type, "Negative.Binomial")) {
          if(!identical(link.type, "log") && !identical(link.type, "identity")) supp.link <- "no"
        }

        if(identical(fam.type, "gaussian")) {
          if(!identical(link.type, "log") && !identical(link.type, "identity")) supp.link <- "no"
        }

        if(identical(fam.type, "Gamma")) {
          if(!identical(link.type, "log")) supp.link <- "no"
        }

        ##if(identical(supp.link, "no")) stop("\nOnly canonical link is supported with current version of function\n")

        ##if(identical(link.type, "other")) stop("\nThis function is not yet defined for the specified link function\n")
      }
    }

    out.link <- list("family" = fam.type, "link" = link.type, "supp.link" = supp.link)

  }


  if(identical(paste(class(mod)), "lmerMod") || identical(paste(class(mod)), "glmerMod")) {

    call.mod <- mod@call

    ##determine type of call:  lmer( ) vs glmer( )
    fun.call <- call.mod[1]

    ##supported link
    supp.link <- "yes"

    if(identical(as.character(fun.call), "lmer")) {
      fam.type <- "gaussian"
      link.type <- "identity"
    }

    if(identical(as.character(fun.call), "glmer")) {
      fam.call <- mod@resp$family
      fam.type <- fam.call$family
      link.type <- fam.call$link

      ##check for links supported by this function
      if(identical(fam.type, "binomial")) {
        if(!identical(link.type, "logit")) supp.link <- "no"
      }

      if(identical(fam.type, "poisson")) {
        if(!identical(link.type, "log") && !identical(link.type, "identity")) supp.link <- "no"
      }

      if(!is.na(charmatch(x = "Negative Binomial", table = fam.type))) {
        ##modify fam.type
        fam.type <- "Negative.Binomial"
        if(!identical(link.type, "log") && !identical(link.type, "identity")) supp.link <- "no"
      }

      if(identical(fam.type, "gaussian")) {
        if(!identical(link.type, "log") && !identical(link.type, "identity")) supp.link <- "no"
      }

      if(identical(fam.type, "Gamma")) {
        if(!identical(link.type, "log")) supp.link <- "no"
      }
    }


    out.link <- list("family" = fam.type, "link" = link.type, "supp.link" = supp.link)
  }

  return(out.link)
}
