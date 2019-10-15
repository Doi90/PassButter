#' @title spoof_legend
#'
#' @description Make a fake legend using a colour palette
#'
#' @usage spoof_legend(cs)
#'
#' @param colours a colour palette character string
#'
#' @param labels a logical indicating if you want labels on the legend
#'
#' @param label_text a character string of labels for each element in the colour palette. If NULL, defaults to colour names
#'
#' @param borders a logical indicating if you want borders around each colour element
#'
#' @param ncol a numeric indicating the number of columns for the legend
#'
#' @param nrow a numeric indicating the number of rows for the legend
#'
#' @param cex_label a numeric indicating the relative text size of the labels
#'
#' @param label_padding_x a numeric indicating spacing of the labels on the x-axis relative to the starting point
#'
#' @param label_padding_y a numeric indicating spacing of the labels on the y-axis relative to the starting point
#'
#' @param ... additional arguments can be passed to the text() function controlling the labels
#'
#' @author David Wilkinson \email{davidpw@student.unimelb.edu.au}
#'
#' @section Date submitted: 2019-10-15
#'
#' @section Last Modified: 2019-10-15
#'
#' @examples spoof_legend(rainbow(10), ncol = 5, nrow = 2)
#'
#' @export

## Credit: Modified from scales::show_col

spoof_legend <- function(colours = NULL,
                         labels = TRUE,
                         label_text = NULL,
                         borders = NULL,
                         ncol = 5,
                         nrow = 2,
                         cex_label = 1,
                         label_padding_x = -0.5,
                         label_padding_y = 0.5,
                         ...){

  if(is.null(colours)){

    stop("Colour palette not supplied")

  }

  ## Build matrix of colours

  n <- length(colours)

  colours <- c(colours,
               rep(NA, nrow * ncol - length(colours)))

  colours <- matrix(colours,
                    ncol = ncol,
                    byrow = TRUE)

  ## Return to old par settings when function finishes

  old <- par(pty = "s",
             mar = c(0, 0, 0, 0))
  on.exit(par(old))


  size <- max(dim(colours))

  ## Plot

  plot(c(0, size),
       c(0, -size),
       type = "n",
       xlab = "",
       ylab = "",
       axes = FALSE)

  rect(col(colours) - 1,
       -row(colours) + 1,
       col(colours),
       -row(colours),
       col = colours,
       border = borders)

  if(labels){

    if(is.null(label_text)){

      labs <- colours

    }

    text(x = col(colours) + label_padding_x,
         y = -row(colours) + label_padding_y,
         labels = labs,
         cex = cex_label,
         ...)

  }
}
