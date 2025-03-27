Tissot <- ggproto("Tissot", Stat,
                  compute_layer = function(self, data, params, layout) {
                    # add coord to the params, so it can be forwarded to compute_group()
                    params$coord <- layout$coord
                    ggproto_parent(Stat, self)$compute_layer(data, params, layout)
                  },

                  compute_panel = function(data, scales, coord) {

                    browser()
                    geometry_data <- data[[ ggplot2:::geom_column(data) ]]
                    geometry_crs <- sf::st_crs(geometry_data)

                    bbox <- sf::st_bbox(geometry_data)

                    if (inherits(coord, "CoordSf")) {
                      # if the coord derives from CoordSf, then it
                      # needs to know about bounding boxes of geometry data
                      coord$record_bbox(
                        xmin = bbox[["xmin"]], xmax = bbox[["xmax"]],
                        ymin = bbox[["ymin"]], ymax = bbox[["ymax"]]
                      )

                      # to represent the location of the geometry in default coordinates,
                      # we take the mid-point along each side of the bounding box and
                      # backtransform
                      bbox_trans <- sf_transform_xy(
                        list(
                          x = c(rep(0.5*(bbox[["xmin"]] + bbox[["xmax"]]), 2), bbox[["xmin"]], bbox[["xmax"]]),
                          y = c(bbox[["ymin"]], bbox[["ymax"]], rep(0.5*(bbox[["ymin"]] + bbox[["ymax"]]), 2))
                        ),
                        coord$get_default_crs(),
                        geometry_crs
                      )

                      # record as xmin, xmax, ymin, ymax so regular scales
                      # have some indication of where shapes lie
                      data$xmin <- min(bbox_trans$x)
                      data$xmax <- max(bbox_trans$x)
                      data$ymin <- min(bbox_trans$y)
                      data$ymax <- max(bbox_trans$y)
                    } else {
                      # for all other coords, we record the full extent of the
                      # geometry object
                      data$xmin <- bbox[["xmin"]]
                      data$xmax <- bbox[["xmax"]]
                      data$ymin <- bbox[["ymin"]]
                      data$ymax <- bbox[["ymax"]]
                    }
                    data
                  },
                  setup_params = function(data, params){
                    # do stuff to params
                    
                    
                    
                    return(params)
                  },

                  required_aes = c("geometry")
)

#' @export
#' @rdname ggsf
#' @inheritParams stat_identity
stat_tissot <- function(mapping = NULL, data = NULL, geom = "sf",
                    position = "identity", na.rm = FALSE, show.legend = NA,
                    inherit.aes = TRUE, centers = NULL,
                    radius = NULL, ...) {
  browser()
  layer_sf(
    stat = Tissot,
    data = data,
    mapping = mapping,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm, centers = centers,
      radius = radius,
      ...
    ),
    check.param = FALSE
  )
}
