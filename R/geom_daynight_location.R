#' Add Day/Night Pattern to ggplot for a location
#'
#' Adds a day/night pattern to a ggplot object that is calculated based on a longitude and latitude. Daytime is represented by
#' rectangles filled with the specified `day_fill` color and nighttime by
#' rectangles filled with the specified `night_fill` color.
#' The pattern is created along the x-axis, which must be a datetime variable.
#'
#' @param day_fill The fill color for daytime rectangles. Defaults to "white".
#' @param night_fill The fill color for nighttime rectangles. Defaults to "grey30".
#' @param longitude Longitude of the location for which to calculate sunrise and sunset times.
#' @param latitude Latitude of the location for which to calculate sunrise and sunset times.
#' @param solarDep A numerical value representing the solar depression angle. See [suntools::crepuscule]
#' @inheritParams ggplot2::geom_rect
#' @param ... Additional arguments passed to `geom_rect`.
#' @return A ggplot2 layer representing the day/night pattern.
#' @examples
#' # Basic usage with default parameters
#' library(ggplot2)
#' ggplot(daynight_temperature, aes(datetime, temperature)) +
#'   geom_daynight_location() +
#'   geom_point()
#'
#' # Basic usage with faceting by sensor
#' ggplot(daynight_temperature, aes(datetime, temperature)) +
#'   geom_daynight_location() +
#'   geom_point() +
#'   facet_wrap(vars(sensor))
#'
#' # Custom day and night fill colors, custom longitude and latitude times, and adjusted alpha
#' ggplot(daynight_temperature, aes(datetime, temperature, color = sensor)) +
#'   geom_daynight_location(
#'     day_fill = "yellow", night_fill = "blue",
#'     longitude = 5, latitude = 20, alpha = 0.5
#'   ) +
#'   geom_line(linewidth = 1)
#' @export
geom_daynight_location <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", na.rm = FALSE,
                          show.legend = NA, inherit.aes = TRUE,
                          day_fill = "white", night_fill = "grey30",
                          longitude = 6, latitude = 18, solarDep=0, ...) {
  layer <- ggplot2::layer(
    geom = GeomDayNightLocation,
    mapping = mapping,
    data = data,
    stat = stat,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      day_fill = day_fill,
      night_fill = night_fill,
      longitude = longitude,
      latitude = latitude,
      solarDep= solarDep,
      ...
    )
  )
  return(layer)
}

#' Create Day/Night Pattern Data
#'
#' Generates a data frame representing daytime and nighttime periods
#' based on a sequence of datetime values and a specific location.
#'
#' @param min_datetime The starting datetime value (POSIXct format).
#' @param max_datetime The ending datetime value (POSIXct format).
#' @param longitude Longitude of the location for which to calculate sunrise and sunset times.
#' @param latitude Latitude of the location for which to calculate sunrise and sunset times.
#' @param solarDep A numerical value representing the solar depression angle. See [suntools::crepuscule]
#' @return A data frame with columns \code{datetime} and \code{daytime},
#'   where \code{datetime} represents the datetime values and \code{daytime}
#'   is a logical indicating whether the time is during the day.
#' @keywords internal
daynight_table <- function(min_datetime, max_datetime, longitude, latitude, solarDep) {
  rlang::check_installed("suntools", "to calculate sunrise times")
  datetime_sequence <- seq(from = min_datetime-3600*24, to = max_datetime+3600*24, by = "day")
  sunrise<-suntools::crepuscule(cbind(longitude, latitude), datetime_sequence,   POSIXct.out = TRUE, direction="dawn",solarDep=solarDep
)$time
  sunset<-suntools::crepuscule(cbind(longitude, latitude), datetime_sequence,   POSIXct.out = TRUE, direction="dusk",solarDep=solarDep
  )$time
  # Internal function to check if a time is during the day
  is_daytime <- function(datetime) {
    hour <- as.numeric(format(datetime, "%H"))
    return(hour >= longitude & hour < latitude)
  }

  datetime_sequence <- seq(from = min_datetime, to = max_datetime, by = "hour")

  daynight <- rbind(data.frame(
    starttime = as.numeric(sunrise),
    endtime = as.numeric(sunset),
    daytime = TRUE
  ),
  data.frame(
    starttime = as.numeric(sunset)[-length(sunset)],
    endtime = as.numeric(sunrise)[-1],
    daytime = FALSE
  )
)
  return(daynight)
}

#' Draw Day/Night Pattern on Panel
#'
#' Internal function to draw the day/night pattern on the ggplot2 panel.
#'
#' @param data The data to be displayed.
#' @param panel_params The parameters of the panel.
#' @param coord The coordinate system.
#' @param day_fill The fill color for daytime rectangles.
#' @param night_fill The fill color for nighttime rectangles.
#' @param longitude Longitude of the location for which to calculate sunrise and sunset times.
#' @param latitude Latitude of the location for which to calculate sunrise and sunset times.
#' @param solarDep A numerical value representing the solar depression angle. See [suntools::crepuscule]
#' @return A gList object containing the grobs for the day/night pattern.
#' @keywords internal
draw_panel_daynight_location <- function(data, panel_params, coord, day_fill,
                                night_fill, longitude, latitude,solarDep) {
  # Check if 'x' is a continuous datetime scale
  if (!inherits(panel_params$x$scale, "ScaleContinuousDatetime")) {
    warning("In geom_daynight_location(): 'x' must be a datetime, ignoring output.",
      call. = FALSE
    )
    return(grid::nullGrob())
  }

  # Get the x-axis limits
  datetime_range <- panel_params$x$get_limits()
  tz <- panel_params$x$scale$timezone
  datetime_range <- as.POSIXct(datetime_range, tz = tz)

  # Generate the day/night table within the datetime range
  daynight <- daynight_table(datetime_range[1], datetime_range[2], longitude, latitude,solarDep=solarDep)

  # Check if the 'fill' parameter was used and warn that it will be ignored
  if (!is.na(unique(data[["fill"]]))) {
    message("Ignoring argument 'fill' in geom_daynight_location, use day_fill and night_fill.")
  }

  # Define the common aesthetics
  common_aes <- c("PANEL", "linewidth", "linetype", "alpha")

  # Create a dataframe with the common aesthetics
  common <- unique(data[, common_aes])
  common$colour <- NA
  rownames(common) <- NULL

  # Create the data for the daytime rectangles
  day_subset <- daynight[daynight$daytime == TRUE, ]
  data_day <- tryCatch(
    {
      merge(
        data.frame(
          xmin = day_subset$starttime,
          xmax = day_subset$endtime, # One rectangle per hour
          ymin = -Inf,
          ymax = Inf,
          fill = day_fill
        ),
        common
      )
    },
    error = function(e) {
      merge(
        data.frame(
          xmin = NA,
          xmax = NA, # One rectangle per hour
          ymin = -Inf,
          ymax = Inf,
          fill = day_fill
        ),
        common
      )
    }
  )

  # Create the data for the nighttime rectangles
  night_subset <- daynight[daynight$daytime == FALSE, ]
  data_night <- tryCatch(
    {
      merge(
        data.frame(
          xmin = night_subset$starttime,
          xmax = night_subset$endtime, # One rectangle per hour
          ymin = -Inf,
          ymax = Inf,
          fill = night_fill
        ),
        common
      )
    },
    error = function(e) {
      merge(
        data.frame(
          xmin = NA,
          xmax = NA, # One rectangle per hour
          ymin = -Inf,
          ymax = Inf,
          fill = night_fill
        ),
        common
      )
    }
  )
  # Draw the daytime and nighttime rectangles on the panel
  grid::gList(
    ggplot2::GeomRect$draw_panel(data_day, panel_params, coord),
    ggplot2::GeomRect$draw_panel(data_night, panel_params, coord)
  )
}

#' GeomDayNightLocation
#'
#' A ggproto object for creating a day/night pattern geom in ggplot2.
#'
#' This geom creates a pattern along the x-axis of a ggplot2 plot,
#' distinguishing between daytime and nighttime using rectangles filled
#' with specified colors.
#'
#' @format An object of class \code{GeomDayNightLocation} (inherits from \code{Geom}, \code{ggproto}).
#' @keywords internal
GeomDayNightLocation <- ggplot2::ggproto(
  "GeomDayNightLocation", ggplot2::Geom,
  required_aes = "x",
  default_aes = ggplot2::aes(
    colour = NA,
    fill = NA,
    linewidth = 0,
    linetype = 1,
    alpha = 0.3
  ),
  draw_key = ggplot2::draw_key_rect,
  draw_panel = draw_panel_daynight_location
)
