#' mapa_chr
#'
#' Crea un plot con grupos de anclaje.
#'
#' @param cr vector Tiene que recibir una vector con valores numericos.
#' @param col_cr color
#' @param unidad undidad de los grupos (cM o Bp)
#'
#' @return
#' @export
#'
mapa_chr <- function(cr = c(30.4, 19.7, 23.5, 18.6, 27),
                     col_cr = "red", unidad = "bp") {
  rango <- 1:length(cr)
  plot(x = rango, # cuantas cromosomas
       y = cr, # longitud de cada uno
       ylim = range(0, max(cr)),
       type = "h", # tipo: histograma
       xaxt = "n", # no poner las etiquetas en x
       yaxt = "n",
       bty = "n", # sin caja
       col = "white",
       xlab = "Cromosomas",
       ylab = unidad
  )
  axis(side = 2,
       at = pretty(0:max(cr)),
       labels = rev(pretty(0:max(cr))),
       lty = 1
  )
  axis(side = 3,
       at = pretty(rango),
       labels = paste0("LG", rango),
       lty = 0
  )
  segments(x0 = rango,
           y0 = abs(max(cr) - cr),
           x1 = rango,
           y1 = max(cr),
           lwd = 10,
           col = col_cr
  )
}
