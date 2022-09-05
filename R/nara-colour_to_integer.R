# Copyright 2022 mikefc <mikefc@coolbutuseless.com>
#
# This function has been extracted from the nara package, available
# under the MIT license at:
# https://github.com/coolbutuseless/nara
#
# It is here re-distributed with the same terms.
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#' Convert a colour as a character string  into an integer value representing RGBA
#' @noRd
#' @param colour character vector of R colour names (including hex colours)
#'
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
colour_to_integer <- function(colour) {
  .Call(colour_to_integer_, colour)
}


# Copyright 2022 Sergio Oller <sergioller@gmail.com>
# This function is part of the ggmatrix package and it is distributed under
# the ggmatrix license terms.
#
# My own implementation, in R, written to better understand the performance impact
# of not using C for this conversion:
# colour_to_integer <- function(x) {
#   # this function vs nara::colour_to_integer(x) gives the same output
#   # for x of length 256 (typical colormap):
#   #   this takes 400ms, nara takes 200ms
#   #   this takes 36KB RAM, nara takes 1KB
#   # for x of length 1E7 (e.g. to process a 10000*1000 matrix)
#   #   this function takes 1.17s, nara takes 400ms
#   #   this function uses 1.3GB RAM, nara uses 38MB (!!!)
#
#   if (is.numeric(x)) {
#     return(x)
#   }
#
#   # grDevices::col2rgb takes the RAM
#   rgba <- grDevices::col2rgb(x, alpha = TRUE)
#   col_as_int <- rgba[1,] + 256L*rgba[2,] + 256L*256L*rgba[3,] + 256*256*256*rgba[4,]
#
#   my_ints <- as.integer(
#     ifelse(
#       rgba[4,] > 127L,
#       -2^32 + col_as_int,
#       col_as_int
#     )
#   )
#   my_ints
# }
