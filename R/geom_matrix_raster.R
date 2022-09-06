# Copyright 2022 Sergio Oller Moreno <sergioller@gmail.com>
# This file is part of the ggmatrix package and it is distributed under the MIT license terms.
# Check the ggmatrix package license information for further details.

geom_matrix_raster_internal <- function(matrix, xmin, xmax, ymin, ymax,
                               fill_nlevels = 256L,
                               fill_min = NA_real_, fill_max = NA_real_,
                               interpolate = FALSE,
                               flip_cols = TRUE,
                               flip_rows = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE,
                               fast_mode = TRUE)
{

  if (fast_mode) {
    if (is.na(fill_min) || is.na(fill_max)) {
      frange <- range(matrix)
      if (is.na(fill_min)) {
        fill_min <- frange[1]
      }
      if (is.na(fill_max)) {
        fill_max <- frange[2]
      }
    }
    data <- data.frame(
      values = seq(from = fill_min, to = fill_max, length.out = fill_nlevels)
    )
  } else {
    data <- data.frame(values = c(matrix))
  }
  mapping <- aes(fill = .data$values)
  # we return two layers, one blank to create the axes and handle limits, another
  # rastering the matrix.
  corners <- data.frame(
    x = c(xmin, xmax),
    y = c(ymin, ymax)
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
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        flip_cols = flip_cols,
        flip_rows = flip_rows,
        interpolate = interpolate,
        fill_range = c(fill_min, fill_max),
        fast_mode = fast_mode
      )
    )
  )
}

#' Raster a matrix as a rectangle, efficiently
#'
#' The goal of this function is to be fast rather than supporting all transformations.
#'
#' @param matrix The matrix we want to render in the plot
#' @param xmin,xmax,ymin,ymax Coordinates where the corners of the matrix will be positioned.
#' @param interpolate If `TRUE`, interpolate linearly, if `FALSE` (the default) don't interpolate.
#' @param flip_cols,flip_rows Flip the rows and columns of the matrix. By default we flip the columns.
#' @param fill_nlevels Instead of passing all matrix values through the scale mapping, we take a shortcut
#' and divide the matrix range into `fill_nlevels` equally spaced in the matrix range of values. Only those
#' are converted to colours. Unused in the slow mode.
#' @param fill_min,fill_max Typically the `range(matrix)`, but you can set limits here as you set them on your scale
#' to get better definition in the range. Unused in the slow mode.
#' @inheritParams ggplot2::geom_raster
#'
#' @export
geom_matrix_raster <- function(matrix, xmin, xmax, ymin, ymax,
                               fill_nlevels = 4096L,
                               fill_min = NA_real_, fill_max = NA_real_,
                               interpolate = FALSE,
                               flip_cols = TRUE,
                               flip_rows = FALSE,
                               show.legend = NA,
                               inherit.aes = TRUE)
{
  geom_matrix_raster_internal(
    matrix = matrix,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    fill_min = fill_min,
    fill_max = fill_max,
    fill_nlevels = fill_nlevels,
    interpolate = interpolate,
    flip_cols = flip_cols,
    flip_rows = flip_rows,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fast_mode = TRUE
  )
}


#' @describeIn geom_matrix_raster A slower version, that accepts scale transformations
#' @export
geom_matrix_raster_slow <- function(matrix, xmin, xmax, ymin, ymax,
                                    interpolate = FALSE,
                                    flip_cols = TRUE,
                                    flip_rows = FALSE,
                                    show.legend = NA,
                                    inherit.aes = TRUE) {
  geom_matrix_raster_internal(
    matrix = matrix,
    xmin = xmin,
    xmax = xmax,
    ymin = ymin,
    ymax = ymax,
    interpolate = interpolate,
    flip_cols = flip_cols,
    flip_rows = flip_rows,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    fast_mode = FALSE
  )
}


GeomMatrixRaster <- ggproto("GeomMatrixRaster", Geom,
                      non_missing_aes = c("xmin", "xmax", "ymin", "ymax", "fill"),
                      required_aes = c("xmin", "xmax", "ymin", "ymax", "fill"),
                      default_aes = aes(fill = "grey35", alpha = NA),

                      draw_panel = function(self, data, panel_params, coord, mat, matrix_nrows, matrix_ncols,
                                            xmin, xmax, ymin, ymax, flip_cols, flip_rows, interpolate, fill_range, fast_mode) {
                        if (!inherits(coord, "CoordCartesian")) {
                          rlang::abort(c(
                            "GeomMatrixRaster only works with coord_cartesian"
                          ))
                        }
                        corners <- data.frame(x = c(xmin, xmax), y = c(ymin, ymax))
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

                        if (fast_mode) {
                          colormap <- farver::encode_native(data$fill)
                          breaks <- seq(from = fill_range[1], to = fill_range[2], length.out = length(colormap))
                          mat <- findInterval(mat, breaks, rightmost.closed = TRUE)
                          mat <- colormap[mat]
                          mat <- matrix(mat, nrow = mat_nr, ncol = mat_nc, byrow = byrow)
                        } else {
                          colours <- farver::encode_native(data$fill)
                          mat <- matrix(colours, nrow = mat_nr, ncol = mat_nc, byrow = byrow)
                        }

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

