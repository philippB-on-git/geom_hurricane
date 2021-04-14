#' Read hurricane data
#' 
#' Used to read hurricane observation dataset "ebtrk_atlc_1988_2015.txt".
#' 
#' @details The file "ebtrk_atlc_1988_2015.txt" must exist in the working directory.
#' 
#' @return a tibble (tbl_df) containing hurricane observation data.
#' 
#' @references \href{https://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}{https://rammb.cira.colostate.edu/research/tropical_cyclones/tc_extended_best_track_dataset/}
#' @importFrom readr read_fwf fwf_widths
read_hurricane_data <- function() {
    ext_tracks_widths <- c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                           4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
    ext_tracks_colnames <- c("storm_id", "storm_name", "month", "day",
                             "hour", "year", "latitude", "longitude",
                             "max_wind", "min_pressure", "rad_max_wind",
                             "eye_diameter", "pressure_1", "pressure_2",
                             paste("radius_34", c("ne", "se", "sw", "nw"), 
                                   sep = "_"),
                             paste("radius_50", c("ne", "se", "sw", "nw"), 
                                   sep = "_"),
                             paste("radius_64", c("ne", "se", "sw", "nw"), 
                                   sep = "_"),
                             "storm_type", "distance_to_land", "final")
    
    ext_tracks <- readr::read_fwf("ebtrk_atlc_1988_2015.txt", 
                                  readr::fwf_widths(ext_tracks_widths, 
                                                    ext_tracks_colnames),
                                  na = "-99")
}

#' Tidy hurricane data
#' 
#' Used to create a tidy dataset in long format from the hurricane observation dataset
#' derived from \code{read_hurricane_data}.
#' 
#' @param data a tibble (tbl_df) derived from \code{read_hurricane_data()} function
#' 
#' @return the tidy tibble (tbl_df) in the long format
#' 
#' @importFrom dplyr select mutate
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_to_title
#' @importFrom rlang sym
tidy_hurricane_data <- function(data) {
    data %>%
        dplyr::mutate(date = as.POSIXct(paste0(!!rlang::sym("year"), "-", 
                                               !!rlang::sym("month"), "-", 
                                               !!rlang::sym("day"), " ", 
                                               !!rlang::sym("hour"), ":", "00:00"),
                                        format = "%Y-%m-%d %H:%M",
                                        tz = "UTC"),
                      storm_id = paste(
                          stringr::str_to_title(!!rlang::sym("storm_name")),
                          !!rlang::sym("year"), 
                          sep = "-"),
                      longitude = -!!rlang::sym("longitude")) %>%
        dplyr::select(!!rlang::sym("storm_id"), 
                      !!rlang::sym("date"), 
                      !!rlang::sym("latitude"), 
                      !!rlang::sym("longitude"), 
                      dplyr::starts_with("radius")) %>%
        tidyr::pivot_longer(cols = dplyr::starts_with("radius"), 
                            names_pattern = "radius_([0-9]{2})_([a-z]{2})",
                            names_to = c("wind_speed", ".value")) %>%
        dplyr::select(-c(!!rlang::sym("ne"), !!rlang::sym("nw"), 
                         !!rlang::sym("se"), !!rlang::sym("sw")),
                      !!rlang::sym("ne"), !!rlang::sym("nw"), 
                      !!rlang::sym("se"), !!rlang::sym("sw"))
}

#' Filter one hurricane observation
#' 
#' Filters an observation for a given point in time in the tidy hurricane observation 
#' derived from \code{tidy_hurricane_data()}.
#' 
#' @param data a tidy dataset (tbl_df) derived from \code{tidy_hurricane_data()}
#' @param storm_name a string specifying the hurricane storm name
#' @param year an integer (or character) specifying the year
#' @param datetime a datetime string following the format %Y-%m-%d %H:%M or the string "last" to use
#' the last observation of a given storm
#' @param tz a string specifying the timezone (default: "UTC")
#'
#' @return the filtered tidy tibble (tbl_df) ready to use with \code{geom_hurricane()}
#' 
#' @examples 
#' \dontrun{
#' data %>%
#'     filter_hurricane_observation(storm_name = "Ike", year = 2008, datetime = "2008-09-13 06:00:00")
#' data %>%
#'     filter_hurricane_observation(storm_name = "Ike", year = 2008, datetime = "last")
#' }
#' 
#' @importFrom dplyr filter
#' @importFrom rlang sym
filter_hurricane_observation <- function(data, 
                                         storm_name, 
                                         year, 
                                         datetime = "last",
                                         tz = "UTC")
{
    # using ifelse approach
    data %>%
        dplyr::filter(!!rlang::sym("storm_id") == paste0(storm_name, "-", year)) %>%
        dplyr::filter(!!rlang::sym("date") == ifelse(
            datetime == "last",
            sort(unique(!!rlang::sym("date")), decreasing = T)[1],
            as.POSIXct(datetime, tz = "UTC")
        ))
    
    # but this would also work:
    # if (datetime == "last") {
    #     cond <- expression(date == sort(unique(date), decrease = T)[1])
    # } else {
    #     cond <- expression(as.POSIXct(datetime, tz = tz))
    # }
    # data <- data %>%
    #     dplyr::filter(UQ(sym("storm_id")) == paste0(storm_name, "-", year)) %>%
    #     dplyr::filter(eval(cond))
}

#' Plot hurricane wind speed
#' 
#' The hurricane geom is used to visualize the areas of wind speeds any given hurricane observation
#' data derived from the \code{filter_hurricane_data()} function.
#' 
#' @param mapping set of aesthetic mappings created by aes() as used in the ggplot2 package:  
#' \itemize{
#'    \item{\code{x} longitude}
#'    \item{\code{y} latitude}
#'    \item{\code{r_nw} radius in northwest direction}
#'    \item{\code{r_sw} radius in southwest direction}
#'    \item{\code{r_ne} radius in northeast direction}
#'    \item{\code{r_se} radius in southeast direction}
#'    \item{\code{scale_radii} (default = 1) used to scale the wind speed radii}
#' }
#' @param data data of a hurricane observation as derived from \code{filter_hurricane_observation()}
#' @param stat statistical transformation to use on the data for this layer, as a string
#' @param inherit.aes If true, mapping is combined with the default mapping at the top level of the 
#' plot; if false, default aesthetics are overwritten.
#' @param show.legend logical. Should this layer be included in the legends? NA (default) includes 
#' if any aesthetics are mapped. FALSE never includes, TRUE always includes.
#' @param na.rm If false (default) missing values are removed with a warning. If true, missing 
#' values are silently removed.
#' @param ... other arguements passed on to \code{ggplot2::layer()}
#' 
#' @details Use the aesthetic \code{scale_radii} to scale the size of the hurricane visualization.
#' \code{geom_hurricane()} can visualize wind speed areas of several hurricane observations
#' at once. The distinct draws of the wind speed segments are separated via the location of the 
#' given hurricane observation using \code{x = longitude} and \code{y = latitude} in the dataset
#' provided via \code{data}.
#' 
#' @importFrom ggplot2 layer
geom_hurricane <- function(mapping = NULL, 
                           data = NULL, 
                           stat ="identity",
                           position = "identity",
                           inherit.aes = TRUE, 
                           show.legend = NA,
                           na.rm = FALSE,
                           ...)
{
    ggplot2::layer(
        geom = ggproto_hurricane, 
        stat = stat,
        data = data,
        mapping = mapping, 
        position = position,
        params = list(na.rm = FALSE, ...),
        inherit.aes = inherit.aes,
        show.legend = NA)
}

#' Create quadrant data from wind speeds
#' 
#' This is a helper function to construct points for the polygon draw derived from 
#' hurricane observation data.
#' 
#' @param data.wind_speed data passed to \code{geom_hurricane()} including x, y, r_ne, r_se, r_nw, 
#' r_sw
#' @param quadrant a string specifying the quadrant, one of c("ne", "se", "nw", "sw").
#' 
#' @details The function uses the \code{destPoint} function from the geosphere package
#' to calculate the points on a quarter circle for given quadrants with radius provided in
#' the \code{data.wind_speed} dataset.
#' 
#' @return a data.frame containing the points to draw a polygon
#' @importFrom geosphere destPoint
wind_speeds_to_quadrant <- function(data.wind_speed, quadrant = c("ne", "se", "nw", "sw")) {
    degr <- switch(quadrant, 
                   nw = 270:360,
                   ne = 0:90,
                   sw = 180:270,
                   se = 90:180)
    
    quadr <- paste0("r_", quadrant)
    data.frame(colour = as.character(data.wind_speed$colour),
               fill = as.character(data.wind_speed$fill),
               geosphere::destPoint(p = c(data.wind_speed$x, 
                                          data.wind_speed$y),
                                    b = degr,
                                    d = data.wind_speed[, quadr]),
               group = data.wind_speed$group,
               PANEL = data.wind_speed$PANEL,
               alpha = data.wind_speed$alpha,
               draw.id = paste(data.wind_speed$x,
                               data.wind_speed$y, 
                               sep = "_"))
}

#' Convert nautic miles to meters
#' 
#' This is a helper function for ggproto_hurricane to convert nautic miles into
#' meters with the possibility to scale the outcome by \code{scale}.
#' 
#' @param nautic (numeric) nautic miles to be converted
#' @param scale (numeric) scaling factor
#' 
#' @details Conversion of input with forumla \eqn{nautic * 1852 * scale}
#' 
#' @return (numeric) converted to meters and scaled by factor \code{scale}
nautic_to_meters <- function(nautic, scale = 1) {
    nautic * 1852 * scale
}

# Constructor for ggproto_hurricane object
# Transforms provided data into points on a quarter circles using wind_speeds_to_quadrant() and
# draws a polygon
ggproto_hurricane <- ggplot2::ggproto("ggproto_hurricane", Geom,
                                         required_aes = c("x", "y",
                                                          "r_ne", "r_se", 
                                                          "r_nw", "r_sw"),
                                         default_aes = aes(fill = 1, 
                                                           colour = 1, 
                                                           alpha = 0.5, 
                                                           scale_radii = 1),
                                         draw_key = draw_key_polygon,
                                         draw_group = function(data, 
                                                               panel_scales, 
                                                               coord) 
{
    # transforming and converting data 
    coords <- coord$transform(data, panel_scales)
    data <- data %>% 
        dplyr::mutate(r_nw = nautic_to_meters(!!rlang::sym("r_nw"), scale_radii),
                      r_ne = nautic_to_meters(!!rlang::sym("r_ne"), scale_radii),
                      r_se = nautic_to_meters(!!rlang::sym("r_se"), scale_radii),
                      r_sw = nautic_to_meters(!!rlang::sym("r_sw"), scale_radii))
    
    # loop through data rows, in case more than one hurricane observation has been passed to
    # geom_hurricane; different observations will be separated in the polygon draw call using the
    # 'id' argument of 'polygonGrob'; wind_speeds_to_quadrant() creates the dataset accordingly
    draw_data <- NULL
    for (i in 1:nrow(data)) {
        NW <- wind_speeds_to_quadrant(data[i, ], quadrant = "nw")
        NE <- wind_speeds_to_quadrant(data[i, ], quadrant = "ne")
        SE <- wind_speeds_to_quadrant(data[i, ], quadrant = "se")
        SW <- wind_speeds_to_quadrant(data[i, ], quadrant = "sw")
        draw_data <-  do.call("rbind", list(draw_data, NW, NE, SE, SW))
    }
    
    # 'id' argument of 'polygonGrob' expects numeric values
    draw_data <- draw_data %>% 
        mutate("draw.id" = as.numeric(as.factor(!!rlang::sym("draw.id"))))
    
    # rename and transform created quadrant data
    base::names(draw_data) <- base::sub("lon", "x", base::names(draw_data))
    base::names(draw_data) <- base::sub("lat", "y", base::names(draw_data))
    draw_data <- coord$transform(draw_data, panel_scales)
   
    # draw grob polygon using created quadrant data
    grid::polygonGrob(
        x = draw_data$x,
        y = draw_data$y,
        id = draw_data$draw.id, # this is necessary to separately draw two hurricane observations
        gp = grid::gpar(col = draw_data$colour, 
                        fill = draw_data$fill, 
                        alpha = draw_data$alpha)
    )
    
})

#---------------------------------------------------------------------------------------------------
library(stringr)
library(tidyr)
library(dplyr)
library(readr)
library(ggmap)

hur_data <- read_hurricane_data()
storm_observation <- tidy_hurricane_data(hur_data) %>%
    filter_hurricane_observation(storm_name = "Ike", year = 2008, datetime = "2008-09-13 06:00:00")

get_map("Louisiana", zoom = 6, maptype = "toner-background") %>%
    ggmap(extent = "device") +
    geom_hurricane(data = storm_observation,
                   aes(x = longitude, y = latitude,
                       r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                       fill = wind_speed, color = wind_speed),
                   scale_radii = 0.9) +
    scale_color_manual(name = "Wind speed (kts)", 
                       values = c("red", "orange", "yellow")) + 
    scale_fill_manual(name = "Wind speed (kts)", 
                      values = c("red", "orange", "yellow"))
