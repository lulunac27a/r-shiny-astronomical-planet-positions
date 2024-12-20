# import Shiny library
library(shiny)
# define UI component
ui <- fluidPage(titlePanel("Astronomical Planet Positions with Rise and Set Times and Sun"),
    sidebarLayout(sidebarPanel(sliderInput("date", "Day of Year",
        min = 1, max = 365, value = 1, step = 1)  #date input
,
        sliderInput("time", "Time of Day", min = 0, max = 24,
            value = 12, step = 0.01)  #time input
, sliderInput("latitude",
            "Latitude", min = -90, max = 90, value = 0, step = 0.01)  #latitude input
, sliderInput("elongation", "Elongation",
            min = -180, max = 180, value = 0, step = 0.01)  #planet elongation angle input
), mainPanel(textOutput("dateTime")  #date and time output
, textOutput("sunPosition")  #sun position output
, textOutput("sunRiseSet")  #sun rise and set time output
, textOutput("planetPosition")  #planet position output
, textOutput("planetRiseSet")  #planet rise and set time output
, textOutput("sunAltitudeAzimuth")  #sun altitude and azimuth angle output
, textOutput("planetAltitudeAzimuth")  #planet altitude and azimuth angle output
)))
# define server component
server <- function(input, output) {
    day_to_month_day <- function(day) {
        # convert day of year to month and day
        days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30,
            31, 30, 31)
        # number of days in a month
        month_names <- c("January", "February", "March", "April",
            "May", "June", "July", "August", "September", "October",
            "November", "December")
        # full month names
        month <- 1
        # set initial month to 1 (January)
        while (day > days_in_month[month]) {
            # while day is greater than days in a specified
            # month
            day <- day - days_in_month[month]
            # subtract days by days in a specified month
            month <- month + 1
            # increase month by 1
        }
        return(list(month = month_names[month], day = day))
        # return list of month and day number
    }

    sun_longitude <- function(date) {
        # get sun longitude based on day of year (date)
        return((date - 80) * 360/365)
        # in degrees
    }

    longitude_to_right_ascension <- function(longitude) {
        # convert ecliptic longitude to right ascension
        return(atan2(sin(longitude * pi/180) * cos(23.45 * pi/180),
            cos(longitude * pi/180)) * 180/pi)
        # in degrees
    }

    sun_declination <- function(date) {
        return(asin(sin(23.44 * pi/180) * sin(sun_longitude(date) *
            pi/180)) * 180/pi)
        # in degrees
    }

    sun_rise_set <- function(latitude, declination) {
        # get sunrise and sunset times
        hour_angle <- acos(-tan(latitude * pi/180) * tan(declination *
            pi/180)) * 180/pi
        # in degrees
        sunrise <- 12 - hour_angle/15
        sunset <- 12 + hour_angle/15
        return(list(sunrise = sunrise, sunset = sunset))
    }

    planet_longitude <- function(date, elongation) {
        # get planet longitude based on day of year (date)
        # and planet elongation
        return((date - 80) * 360/365 + elongation)
        # in degrees
    }

    planet_declination <- function(date, elongation) {
        # get planet declination based on planet longitude
        return(asin(sin(23.44 * pi/180) * sin(planet_longitude(date,
            elongation) * pi/180)) * 180/pi)
        # in degrees
    }

    planet_rise_set <- function(latitude, declination, elongation) {
        # get planet rise and set times based on latitude,
        # declination and planet elongation
        sun_right_ascension <- longitude_to_right_ascension(sun_longitude(input$date))
        # in degrees
        planet_right_ascension <- longitude_to_right_ascension(planet_longitude(input$date,
            elongation))
        # in degrees
        hour_angle <- acos(-tan(latitude * pi/180) * tan(declination *
            pi/180)) * 180/pi
        # in degrees
        rise <- (12 - hour_angle/15 - (sun_right_ascension -
            planet_right_ascension)/360 * 24)%%24
        # in hours
        set <- (12 + hour_angle/15 - (sun_right_ascension - planet_right_ascension)/360 *
            24)%%24
        # in hours
        return(list(rise = rise, set = set))
    }

    altitude_azimuth <- function(latitude, declination, time) {
        # get altitude and azimuth based on latitude,
        # declination and time (hour angle)
        hour_angle <- (time - 12) * 15
        # in hours
        altitude <- asin(sin(latitude * pi/180) * sin(declination *
            pi/180) + cos(latitude * pi/180) * cos(declination *
            pi/180) * cos(hour_angle * pi/180)) * 180/pi
        # in degrees
        azimuth <- acos(min(1, max(-1, (sin(declination * pi/180) -
            sin(altitude * pi/180) * sin(latitude * pi/180))/(cos(altitude *
            pi/180) * cos(latitude * pi/180))))) * 180/pi
        # in degrees
        if (hour_angle > 0) {
            azimuth <- 360 - azimuth
        }
        return(list(altitude = altitude, azimuth = azimuth))
    }

    time_to_hh_mm <- function(time) {
        # convert time to hh:mm.mm format
        sign <- ifelse(time < 0, "-", "")
        # add a minus sign if time is negative
        time <- abs(time)
        # use absolute value of time
        hours <- floor(time)
        minutes <- (time - hours) * 60
        return(sprintf("%s%02d:%05.2f", sign, hours, minutes))
    }

    output$dateTime <- renderText({
        # date and time output
        date <- input$date
        # date input
        time <- input$time
        # time input
        month_day <- day_to_month_day(date)
        paste("Date: ", month_day$month, " ", month_day$day,
            "\nTime: ", time_to_hh_mm(time))
    })

    output$sunRiseSet <- renderText({
        # sun rise and set time output
        date <- input$date
        latitude <- input$latitude
        # latitude input
        declination <- sun_declination(date)
        rise_set <- sun_rise_set(latitude, declination)
        paste("Sunrise: ", time_to_hh_mm(rise_set$sunrise), "\nSunset: ",
            time_to_hh_mm(rise_set$sunset))
    })

    output$sunPosition <- renderText({
        # sun position output
        date <- input$date
        longitude <- sun_longitude(date)
        right_ascension <- longitude_to_right_ascension(longitude)
        declination <- sun_declination(date)
        paste("Sun Right Ascension: ", round(right_ascension,
            2), "\nSun Declination: ", round(declination, 2))
    })

    output$sunAltitudeAzimuth <- renderText({
        # sun altitude and azimuth output
        date <- input$date
        time <- input$time
        latitude <- input$latitude
        declination <- sun_declination(date)
        alt_az <- altitude_azimuth(latitude, declination, time)
        paste("Sun Altitude: ", round(alt_az$altitude, 2), "\nSun Azimuth: ",
            round(alt_az$azimuth, 2))
    })

    output$planetPosition <- renderText({
        # planet position output
        date <- input$date
        elongation <- input$elongation
        # planet elongation angle input
        longitude <- planet_longitude(date, elongation)
        declination <- planet_declination(date, elongation)
        right_ascension <- longitude_to_right_ascension(longitude)
        paste("Planet Right Ascension: ", round(right_ascension,
            2), "\nPlanet Declination: ", round(declination,
            2))
    })

    output$planetRiseSet <- renderText({
        # planet rise and set time output
        date <- input$date
        latitude <- input$latitude
        elongation <- input$elongation
        declination <- planet_declination(date, elongation)
        rise_set <- planet_rise_set(latitude, declination, elongation)
        paste("Planet Rise: ", time_to_hh_mm(rise_set$rise),
            "\nPlanet Set: ", time_to_hh_mm(rise_set$set))
    })

    output$planetAltitudeAzimuth <- renderText({
        # planet altitude and azimuth output
        date <- input$date
        time <- input$time + 24/360 * (longitude_to_right_ascension(planet_longitude(date,
            input$elongation)) - longitude_to_right_ascension(sun_longitude(date)))
        # adjust time based on planet right ascension and
        # sun right ascension
        latitude <- input$latitude
        elongation <- input$elongation
        declination <- planet_declination(date, elongation)
        alt_az <- altitude_azimuth(latitude, declination, time)
        paste("Planet Altitude: ", round(alt_az$altitude, 2),
            "\nPlanet Azimuth: ", round(alt_az$azimuth, 2))
    })
}
# run the Shiny web app server
shinyApp(ui = ui, server = server)

