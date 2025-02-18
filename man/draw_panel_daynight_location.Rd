% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom_daynight_location.R
\name{draw_panel_daynight_location}
\alias{draw_panel_daynight_location}
\title{Draw Day/Night Pattern on Panel}
\usage{
draw_panel_daynight_location(
  data,
  panel_params,
  coord,
  day_fill,
  night_fill,
  longitude,
  latitude,
  solarDep
)
}
\arguments{
\item{data}{The data to be displayed.}

\item{panel_params}{The parameters of the panel.}

\item{coord}{The coordinate system.}

\item{day_fill}{The fill color for daytime rectangles.}

\item{night_fill}{The fill color for nighttime rectangles.}

\item{longitude}{Longitude of the location for which to calculate sunrise and sunset times.}

\item{latitude}{Latitude of the location for which to calculate sunrise and sunset times.}

\item{solarDep}{A numerical value representing the solar depression angle. See [suntools::crepuscule]}
}
\value{
A gList object containing the grobs for the day/night pattern.
}
\description{
Internal function to draw the day/night pattern on the ggplot2 panel.
}
\keyword{internal}
