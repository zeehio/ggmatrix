# Copyright 2022 Sergio Oller Moreno <sergioller@gmail.com>
# This file is part of the ggmatrix package and it is distributed under the MIT license terms.
# Check the ggmatrix package license information for further details.

#' Raster a matrix as a rectangle, efficiently
#'
#'
#' @param matrix The matrix we want to render in the plot
#' @param xmin,xmax,ymin,ymax Coordinates where the corners of the matrix will be positioned.
#' @param interpolate If `TRUE`, interpolate linearly, if `FALSE` (the default) don't interpolate.
#' @param flip_cols,flip_rows Flip the rows and columns of the matrix. By default we flip the columns.
#' @inheritParams ggplot2::geom_raster
#'
#' @export
geom_matrix_raster <- function(matrix, xmin, xmax, ymin, ymax,
                               interpolate = FALSE,
                               flip_cols = TRUE,
                               flip_rows = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE)
{
  data <- data.frame(values = c(matrix))
  mapping <- aes(fill = .data$values)

  if (nrow(matrix) > 1L) {
    x_step <- (xmax - xmin)/(nrow(matrix) - 1L)
  } else {
    x_step <- 1
  }
  if (ncol(matrix) > 1L) {
    y_step <- (ymax - ymin)/(ncol(matrix) - 1L)
  } else {
    y_step <- 1
  }


  # we return two layers, one blank to create the axes and handle limits, another
  # rastering the matrix.
  corners <- data.frame(
    x = c(xmin - x_step/2, xmax + x_step/2),
    y = c(ymin - y_step/2, ymax + y_step/2)
  )
  list(
    layer(
      data = corners, mapping = aes(x=.data$x, y=.data$y), stat = StatIdentity, geom = GeomBlank,
      position = PositionIdentity, show.legend = show.legend, inherit.aes = inherit.aes,
      params = list(), check.aes = FALSE
    ),
    layer(
      data = data,
      mapping = mapping,
      stat = StatIdentity,
      geom = GeomMatrixRaster,
      position = PositionIdentity,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list2(
        mat = matrix,
        matrix_nrows = nrow(matrix),
        matrix_ncols = ncol(matrix),
        corners = corners,
        flip_cols = flip_cols,
        flip_rows = flip_rows,
        interpolate = interpolate
      )
    )
  )
}

GeomMatrixRaster <- ggproto(
  "GeomMatrixRaster", Geom,
  non_missing_aes = c("fill"),
  required_aes = c("fill"),
  default_aes = aes(fill = "grey35"),

  draw_panel = function(self, data, panel_params, coord, mat, matrix_nrows, matrix_ncols,
                        corners, flip_cols, flip_rows, interpolate) {
    if (!inherits(coord, "CoordCartesian")) {
      rlang::abort(c(
        "GeomMatrixRaster only works with coord_cartesian"
      ))
    }
    corners <- coord$transform(corners, panel_params)
    if (inherits(coord, "CoordFlip")) {
      byrow <- TRUE
      mat_nr <- matrix_ncols
      mat_nc <- matrix_nrows
      nr_dim <- c(matrix_nrows, matrix_ncols)
    } else {
      byrow <- FALSE
      mat_nr <- matrix_nrows
      mat_nc <- matrix_ncols
      nr_dim <- c(matrix_ncols, matrix_nrows)
    }
    x_rng <- range(corners$x, na.rm = TRUE)
    y_rng <- range(corners$y, na.rm = TRUE)

    mat <- matrix(
      farver::encode_native(data$fill),
      nrow = mat_nr,
      ncol = mat_nc,
      byrow = byrow
    )

    if (flip_cols) {
      rev_cols <- seq.int(mat_nc, 1L, by = -1L)
      mat <- mat[, rev_cols, drop = FALSE]
    }
    if (flip_rows) {
      rev_rows <- seq.int(mat_nr, 1L, by = -1L)
      mat <- mat[rev_rows, drop = FALSE]
    }

    nr <- structure(
      mat,
      dim = nr_dim,
      class = "nativeRaster",
      channels = 4L
    )

    rasterGrob(nr, x_rng[1], y_rng[1],
               diff(x_rng), diff(y_rng), default.units = "native",
               just = c("left","bottom"), interpolate = interpolate)
  },
  draw_key = draw_key_rect
)
