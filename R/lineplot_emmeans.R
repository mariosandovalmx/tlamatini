#' Lineplot con pvalues obtenidos con emmeans
#'
#' Lineplot con los valores de p obtenidos con emmeans de un LM, LMM, GLM, GLMM, etc. Gráficos generados con paqueteria "ggplot", por ahora solo permite una variable explicativa sin interacciones.
#' @param formula Formula de los contrastes que nos interesan obtener.
#' @param modelo Modelo LM, GLM, GLMM, etc.
#' @param data Base de datos con que se ajusto el modelo.
#' @param p.adjust.method método para ajustar los valores p para comparaciones múltiples. Se utiliza cuando se realizan comparaciones por pares. Los valores permitidos incluyen "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". Si no desea ajustar el valor p (no recomendado), utilice p.adjust.method = "none".
#'
#' @return Lineplot de los contrastes posthoc en el modelo todas las variables explicativas del modelo.
#' @export
#'
#' @examples
#' data(iris)
#' mod <- glm(Petal.Width ~ Species, family = gaussian("log"), data=iris)
#' lineplot_emmeans(formula = Petal.Width ~ Species, modelo=mod, data= iris)
#' @encoding UTF-8
#' @import rstatix
#' @import ggplot2


lineplot_emmeans <- function(formula, modelo, data, p.adjust.method=NULL){

  if(is.null(p.adjust.method)){

    data<- data
    form <- as.formula(formula)
    mod <- modelo

    var <- as.character(form[3])
    respvar <- as.character(form[2])

    stat.tests <- data %>%
      rstatix::emmeans_test(form, model = mod, p.adjust.method = "bonferroni")
    stat.tests
    stat.tests <- stat.tests %>% rstatix::add_xy_position(x = var)
    stat.tests
    stat.tests2 <- stat.tests[,c(3:9)]


    bxp <- ggpubr::ggerrorplot(data, x = var, y = respvar, error.plot = "errorbar" ,
                               desc_stat = c("mean_se","mean"), color = "black",
                               add = "jitter", add.params = list(color = "darkgray", alpha=0.7)) +theme(legend.position = "none")

    bxp2 <-  bxp + ggpubr::stat_pvalue_manual(stat.tests,
                                              y.position = stat.tests$y.position,
                                              color = "black",
                                              label = "p.adj.signif")
    print(bxp2)
    return(stat.tests2)



  }else if(isTRUE(is.character(p.adjust.method))){
    data<- data
    form <- formula(model, fixed.only = TRUE)
    mod <- model
    padj <-  p.adjust.method

    var <- as.character(form[3])
    respvar <- as.character(form[2])

    stat.tests <- data %>%
      rstatix::emmeans_test(form, model = mod, p.adjust.method = padj)
    stat.tests
    stat.tests <- stat.tests %>% rstatix::add_xy_position(x = var)
    stat.tests
    stat.tests2 <- stat.tests[,c(3:9)]

    bxp <- ggpubr::ggerrorplot(data, x = var, y = respvar, error.plot = "errorbar" ,
                               desc_stat = c("mean_se","mean"), color = "black",
                               add = "jitter", add.params = list(color = "darkgray", alpha=0.7))
    bxp2 <-  bxp + ggpubr::stat_pvalue_manual(stat.tests,
                                              y.position = stat.tests$y.position,
                                              color = "black",
                                              label = "p.adj.signif")
    print(bxp2)
    return(stat.tests2)}
}

