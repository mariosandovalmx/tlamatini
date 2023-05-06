#' Violinplot con pvalues obtenidos con emmeans
#'
#' Violinplot con los valores de p obtenidos con emmeans de un LM, LMM, GLM, GLMM, etc. Gráficos generados con paqueteria "ggplot", por ahora solo permite una variable explicativa sin interacciones. Proporciona informacion sobre la distribución de cada grupo, así como la media y los quantiles.
#' @param formula Formula de los contrastes que nos interesan obtener.
#' @param modelo Modelo LM, GLM, GLMM, etc.
#' @param data Base de datos con que se ajusto el modelo.
#' @param p.adjust.method método para ajustar los valores p para comparaciones múltiples. Se utiliza cuando se realizan comparaciones por pares. Los valores permitidos incluyen "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". Si no desea ajustar el valor p (no recomendado), utilice p.adjust.method = "none".
#'
#' @return Violinplot de los contrastes posthoc en el modelo todas las variables explicativas del modelo.
#' @export
#'
#' @examples
#' library(tlamatini)
#' data(iris)
#' iris$Species <- as.factor(iris$Species)
#' mod <- glm(Petal.Width ~ Species, family = gaussian("log"), data=iris)
#' violinplot_emmeans(formula = Petal.Width ~ Species, modelo=mod, data= iris)
#' @encoding UTF-8
#' @import rstatix
#' @import ggplot2

violinplot_emmeans <- function(formula, modelo, data, p.adjust.method=NULL){

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

    suppressWarnings(
      viol <-  ggpubr::ggboxplot(data, x = var, y = respvar, fill = var, bxp.errorbar=T,width = 0.3,
                                 bxp.errorbar.width = 0.2, ,add = c("jitter","mean"),add.params =
                                   list(color = "black", alpha=0.3)) +
        ggplot2::stat_boxplot(geom = "errorbar",width = 0.2, lwd=0.8) +
        geom_split_violin(aes(x = factor(var), y = respvar, fill = var),alpha = .4, trim = FALSE) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_brewer(palette = "Dark2"))


    viol2 <- viol + ggpubr::stat_pvalue_manual(stat.tests,
                                               y.position = stat.tests$y.position,
                                               color = "black",
                                               label = "p.adj.signif",
                                               linetype =7,
                                               label.size = 4,
                                               bracket.size =0.8)

    print(viol2)

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

    suppressWarnings(
      viol <-  ggpubr::ggboxplot(data, x = var, y = respvar, fill = var, bxp.errorbar=T,width = 0.3, bxp.errorbar.width = 0.2, ,add = c("jitter","mean"),add.params = list(color = "black", alpha=0.3)) + ggplot2::stat_boxplot(geom = "errorbar",width = 0.2, lwd=0.8) +
        geom_split_violin(aes(x = var, y = respvar, fill = var),alpha = .4, trim = FALSE) +
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_brewer(palette = "Dark2"))


    viol2 <- viol + ggpubr::stat_pvalue_manual(stat.tests,
                                               y.position = stat.tests$y.position,
                                               color = "black",
                                               label = "p.adj.signif",
                                               linetype =7,
                                               label.size = 4,
                                               bracket.size =0.8)

    print(viol2)

    return(stat.tests2)}
}
