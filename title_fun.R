range_norm <- function (x) 
{
  (x - min(x))/(max(x) - min(x))
}



get_palette <- function (palette) 
{
  if (is.null(palette)) {
    palette = c("skyblue", "dodgerblue")
  }
  if (!is.null(names(palette))) {
    pal.colours <- names(palette)
    pal.values <- unname(palette)
    pal.norm <- range_norm(pal.values)
  }
  else {
    pal.colours <- palette
    pal.norm <- seq(0, 1, length.out = length(pal.colours))
    pal.values <- seq(0, 1, length.out = length(pal.colours))
  }
  pal.colours = if (length(palette) == 1) {
    data.frame(values = c(pal.values, pal.values + 1), norm = c(0, 
                                                                1), orig = c(pal.colours, pal.colours), stringsAsFactors = F)
  }
  else {
    data.frame(values = pal.values, norm = pal.norm, orig = pal.colours, 
               stringsAsFactors = F)
  }
  pal.colours$hex <- gradient_n_pal(colours = pal.colours$orig, 
                                    values = pal.colours$values, space = "Lab")(pal.colours$values)
  pal.colours
}
