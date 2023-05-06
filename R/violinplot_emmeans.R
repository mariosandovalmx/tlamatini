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

    df<- data
    form <- as.formula(formula)
    mod <- modelo

    var <- as.character(form[3])
    respvar <- as.character(form[2])


    var2 <- sym(as.character(form[3]))
    respvar2 <- sym(as.character(form[2]))


    stat.tests <- data %>%
      rstatix::emmeans_test(form, model = mod, p.adjust.method = "bonferroni")
    stat.tests <- stat.tests %>% rstatix::add_xy_position(x = var)
    stat.tests2 <- stat.tests[,c(3:9)]


      viol <-  ggplot(df, aes(x = !!var2, y = !!respvar2, fill=!!var2)) +
        geom_boxplot(outlier.shape=NA,  alpha=0.5) +
        stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="darkred", fill="darkred") +
        geom_jitter(aes(colour = !!var2),width=0.4, alpha=0.3)+
        geom_split_violin(ggplot2::aes(x = !!var2, y = !!respvar2, fill = !!var2),alpha = .4, trim = FALSE, scale = "count")+
        ggplot2::theme(legend.position = "none") +
        ggplot2::scale_fill_brewer(palette = "Dark2")


    #viol2 <- viol + ggpubr::stat_pvalue_manual(stat.tests,
    #                                           y.position = stat.tests$y.position,
    #                                          color = "black",
    #                                           label = "p.adj.signif",
    #                                           linetype =7,
    #                                           label.size = 4,
    #                                           bracket.size =0.8)

    print(viol)
    insight::print_color("Funci\u00f3n en desarrollo. Agregar valores de p manualmente.", "red")
    return(stat.tests2)




  }else if(isTRUE(is.character(p.adjust.method))){
    df<- data
    form <- as.formula(formula)
    mod <- modelo
    padj <-  p.adjust.method

    var <- as.character(form[3])
    respvar <- as.character(form[2])


    var2 <- sym(as.character(form[3]))
    respvar2 <- sym(as.character(form[2]))


    stat.tests <- data %>%
      rstatix::emmeans_test(form, model = mod, p.adjust.method = padj)
    stat.tests <- stat.tests %>% rstatix::add_xy_position(x = var)
    stat.tests2 <- stat.tests[,c(3:9)]


    viol <-  ggplot(df, aes(x = !!var2, y = !!respvar2, fill=!!var2)) +
      geom_boxplot(outlier.shape=NA,  alpha=0.5) +
      stat_summary(fun.y=mean, geom="point", shape=20, size=6, color="darkred", fill="darkred") +
      geom_jitter(aes(colour = !!var2),width=0.4, alpha=0.3)+
      geom_split_violin(ggplot2::aes(x = !!var2, y = !!respvar2, fill = !!var2),alpha = .4, trim = FALSE, scale = "count")+
      ggplot2::theme(legend.position = "none") +
      ggplot2::scale_fill_brewer(palette = "Dark2")


    #viol2 <- viol + ggpubr::stat_pvalue_manual(stat.tests,
    #                                           y.position = stat.tests$y.position,
    #                                          color = "black",
    #                                           label = "p.adj.signif",
    #                                           linetype =7,
    #                                           label.size = 4,
    #                                           bracket.size =0.8)

    print(viol)
    insight::print_color("Funci\u00f3n en desarrollo. Agregar valores de p manualmente.", "red")
    return(stat.tests2)}
}
