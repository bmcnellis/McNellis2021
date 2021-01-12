#' @title Miscellaneous small functions
#'
#' @description Small functions for McNellis2021.
#'
#' @name misc
#' @rdname misc
NULL
#' @rdname misc
#' @export
trunc_dec <- function(x, type, level = 1) {
    if (type == 'floor') {
        y <- round(x - 5*10^(-level-1), level)
    } else if (type == 'ceiling') {
        y <- round(x + 5*10^(-level-1), level)
    } else {
        stop('bad type')
    }
    y
}
#' @rdname misc
#' @export
lm_eqn <- function(df){
    m <- lm(y ~ x, df)
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
                     list(a = format(coef(m)[1], digits = 2),
                          b = format(coef(m)[2], digits = 2),
                          r2 = format(summary(m)$r.squared, digits = 3)))
    as.character(as.expression(eq))
}
#' @rdname misc
#' @export
std_err <- function(x) {
    y <- sd(x) / sqrt(length(x))
    return(y)
}
#' @rdname misc
#' @export
mod_10_time <- function(y) {
    z <- unlist(lapply(strsplit(y, ' '), function(x) x[length(x)]))
    zz <- unlist(lapply(strsplit(z, '\\('), function(x) x[2]))
    z0 <- unlist(strsplit(zz, ')'))
    z0
}
#' @rdname misc
#' @export
get_McNellis_data <- function() {
    message("Figures data is loaded by running 'data(figure_X_df)' where X is a number from 1-8, e.g. 'data(figure_1_df)'.")
    message("You can also use '?figure_1_df' for a description of the data object.")
    cat('\n')
    message("Available figure dataframes:")
    cat('\nfigure_1_df\nfigure_2_df\nfigure_3_df\nfigure_4_df\nfigure_5_df\nfigure_6_df\nfigure_7_df\nfigure_8_df\n\n')
    message("Tables can be loaded the same way.")
    cat('\n')
    message("Availabletable dataframes:")
    cat('\ntable_1\ntable_2\ntable_3\ntable_A1S1\n\n')
}
