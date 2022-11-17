#' ryx
#'
#' Correlation Function
#'
#' The ryx package intends to make calculating and visualizing correlations easier.
#'
#' @param data is a data frame
#' @param y is the name of a numeric variable in the data frame
#' @param x is a character vector with the names of one or more numeric variables in the data frame
#'
#' @return An object that displays the variables chosen, the correlations of each variable, the p-value of each correlation, and the significance of each p-value
#' @export
#'
#' @examples
#' \dontrun{
#' ryx(mtcars, y = "mpg", x = c("hp", "wt", "disp", "cyl", "am", "gear"))
#' }
ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}

#' print.ryx
#'
#' The print function prints out an object displaying the correlation, the p-values and the significance of the p-values of all the selected outcome variables against the response variable.
#'
#' @param x: an object of class ryx
#'
#' @return Prints out an object displaying the variables chosen, the correlations of each variable, the p-value of each correlation, and the significance of each p-value
#' @export
#'
#' @examples
#' \dontrun{
#' print(ryx(mtcars, y = "mpg", x = c("hp", "wt", "disp", "cyl", "am", "gear")))
#' }
#'
print.ryx = function(x){
  if(!inherits(x, "ryx")){
    stop("This function requires an object of type 'ryx'")
  }
  variable = x$df$variable
  r = round(x$df$r, digits = 3)
  p = format.pval(x$df$p, digits = 3)
  sigif = x$df$sigif
  pf = data.frame(variable  = variable,
                  r = r,
                  p = p,
                  sigif = sigif)
  row.names(pf) = NULL
  cat("Correlations of",
      x$y,
      "with
")
  print(pf)
}



#' summary.ryx
#'
#' The summary function summarized in only a few sentences what variables your a correlating with your response variable. It displays the median, the range, the amount of statistically significant correlations, and all the variables involved the variables used.
#'
#' @param x: an object of class ryx
#'
#' @return A summary of the statistics calculated in ryx.
#' @export
#'
#' @examples
#' \dontrun{
#' summary(ryx(mtcars, y = "mpg", x = c("hp", "wt", "disp", "cyl", "am", "gear")))
#' }
summary.ryx = function(x){
  if(!inherits(x, "ryx")){
    stop("This function requires an object of type 'ryx'")
  }
  med = median(abs(x$df$r))
  min = min(x$df$r)
  max = max(x$df$r)
  sig = ifelse(x$df$p < 0.05, 1,0)
  s = length(which(sig == 1))
  total = length(x$df$p)

  cat("Correlating ", x$y , "with ", x$x,
      "The median absolute correlation was", med , "with a range from", min , "to" , max,
      s, "out of" , total , "where significant at the p < 0.05 level.")
}



#' plot.ryx
#'
#' The plot function displays the correlation of all the outcome variables against the response variable.
#'
#' @param x: an object of class ryx
#'
#' @return A plot of the correlations of each variable.
#' @export
#'
#' @examples
#' \dontrun{
#' plot(ryx(mtcars, y = "mpg", x = c("hp", "wt", "disp", "cyl", "am", "gear")))
#' }
plot.ryx = function(x){
  if(!inherits(x, "ryx")){
    stop("This function requires an object of type 'ryx'")
  }
  library(ggplot2)
  title = paste("Correlations with", x$y)
  xvar = x$df$r
  yvar = x$df$variable
  posneg = ifelse(x$df$r >= 0, "positive","negative")
  ggplot(data = x$df,
         aes(x = abs(xvar),
             y = reorder(yvar, abs(xvar)),
             color = posneg))+
    geom_point()+
    geom_segment(aes(yend = reorder(yvar, abs(xvar)), xend = 0),
                 color = "gray")+
    labs(title = title,
         x = "Correlation (absolute values)",
         y = "Variables")+
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.1))+
    theme_minimal()+
    scale_color_manual(values =
                         c("negative" = "red",
                           "positive" = "blue")) +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(color = "gray", linetype = "dashed"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.background = element_rect(color = "black"))

}
