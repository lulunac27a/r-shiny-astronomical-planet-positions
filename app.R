library(shiny)

ui <- fluidPage(
    titlePanel("Astronomical Planet Positions with Rise and Set Times and Sun"),
    sidebarLayout(
        sidebarPanel(
            sliderInput("date", "Day of Year", min = 1, max = 365, value = 1, step = 1),
            sliderInput("time", "Time of Day", min = 0, max = 24, value = 12, step = 0.01),
            sliderInput("latitude", "Latitude", min = -90, max = 90, value = 0, step = 0.01),
            sliderInput("elongation", "Elongation", min = -180, max = 180, value = 0, step = 0.01)
        ),
        mainPanel(
            textOutput("dateTime"),
            textOutput("sunPosition"),
            textOutput("sunRiseSet"),
            textOutput("planetPosition"),
            textOutput("planetRiseSet"),
            textOutput("sunAltitudeAzimuth"),
            textOutput("planetAltitudeAzimuth")
        )
    )
)

server <- function(input, output) {
    day_to_month_day <- function(day) {
        days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
        month_names <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
        month <- 1
        while (day > days_in_month[month]) {
            day <- day - days_in_month[month]
            month <- month + 1
        }
        return(list(month = month_names[month], day = day))
    }
    
    sun_longitude <- function(date) {
        return ((date - 80) * 360 / 365)
    }
    
    longitude_to_right_ascension <- function(longitude) {
        return (atan2(sin(longitude * pi / 180) * cos(23.45 * pi / 180), cos(longitude * pi / 180)) * 180 / pi)
    }
    
    sun_declination <- function(date) {
        return (asin(sin(23.44 * pi / 180) * sin(sun_longitude(date) * pi / 180)) * 180 / pi)
    }
    
    sun_rise_set <- function(latitude, declination) {
        hour_angle <- acos(-tan(latitude * pi / 180) * tan(declination * pi / 180)) * 180 / pi
        sunrise <- 12 - hour_angle / 15
        sunset <- 12 + hour_angle / 15
        return(list(sunrise = sunrise, sunset = sunset))
    }
    
    planet_longitude <- function(date, elongation) {
        return ((date - 80) * 360 / 365 + elongation)
    }
    
    planet_declination <- function(date, elongation) {
        return (asin(sin(23.44 * pi / 180) * sin(planet_longitude(date, elongation) * pi / 180)) * 180 / pi)
    }
    
    planet_rise_set <- function(latitude, declination, elongation) {
        sun_right_ascension <- longitude_to_right_ascension(sun_longitude(input$date))
        planet_right_ascension <- longitude_to_right_ascension(planet_longitude(input$date, elongation))
        hour_angle <- acos(-tan(latitude * pi / 180) * tan(declination * pi / 180)) * 180 / pi
        rise <- (12 - hour_angle / 15 - (sun_right_ascension - planet_right_ascension) / 360 * 24) %% 24
        set <- (12 + hour_angle / 15 - (sun_right_ascension - planet_right_ascension) / 360 * 24) %% 24
        return(list(rise = rise, set = set))
    }

    altitude_azimuth <- function(latitude, declination, time) {
        hour_angle <- (time - 12) * 15
        altitude <- asin(sin(latitude * pi / 180) * sin(declination * pi / 180) + cos(latitude * pi / 180) * cos(declination * pi / 180) * cos(hour_angle * pi / 180)) * 180 / pi
        azimuth <- acos(min(1, max(-1, (sin(declination * pi / 180) - sin(altitude * pi / 180) * sin(latitude * pi / 180)) / (cos(altitude * pi / 180) * cos(latitude * pi / 180))))) * 180 / pi
        if(hour_angle > 0) {
            azimuth <- 360 - azimuth
        }
        return(list(altitude = altitude, azimuth = azimuth))
    }
    
    time_to_hh_mm <- function(time) {
        sign <- ifelse(time < 0, "-", "")
        time <- abs(time)
        hours <- floor(time)
        minutes <- (time - hours) * 60
        return(paste(sign, formatC(hours, width = 2, flag = 0), ":", formatC(minutes, width = 2, flag = 0), sep = ""))
    }
    
    output$dateTime <- renderText({
        date <- input$date
        time <- input$time
        month_day <- day_to_month_day(date)
        paste("Date: ", month_day$month, " ", month_day$day, "\nTime: ", time_to_hh_mm(time))
    })
    
    output$sunRiseSet <- renderText({
        date <- input$date
        latitude <- input$latitude
        declination <- sun_declination(date)
        rise_set <- sun_rise_set(latitude, declination)
        paste("Sunrise: ", time_to_hh_mm(rise_set$sunrise), "\nSunset: ", time_to_hh_mm(rise_set$sunset))
    })

    output$sunPosition <- renderText({
        date <- input$date
        longitude <- sun_longitude(date)
        right_ascension <- longitude_to_right_ascension(longitude)
        declination <- sun_declination(date)
        paste("Sun Right Ascension: ", round(right_ascension, 2), "\nSun Declination: ", round(declination, 2))
    })

    output$sunAltitudeAzimuth <- renderText({
        date <- input$date
        time <- input$time
        latitude <- input$latitude
        declination <- sun_declination(date)
        alt_az <- altitude_azimuth(latitude, declination, time)
        paste("Sun Altitude: ", round(alt_az$altitude, 2), "\nSun Azimuth: ", round(alt_az$azimuth, 2))
    })

    output$planetPosition <- renderText({
        date <- input$date
        elongation <- input$elongation
        longitude <- planet_longitude(date, elongation)
        declination <- planet_declination(date, elongation)
        right_ascension <- longitude_to_right_ascension(longitude)
        paste("Planet Right Ascension: ", round(right_ascension, 2), "\nPlanet Declination: ", round(declination, 2))
    })

    output$planetRiseSet <- renderText({
        date <- input$date
        latitude <- input$latitude
        elongation <- input$elongation
        declination <- planet_declination(date, elongation)
        rise_set <- planet_rise_set(latitude, declination, elongation)
        paste("Planet Rise: ", time_to_hh_mm(rise_set$rise), "\nPlanet Set: ", time_to_hh_mm(rise_set$set))
    })

    output$planetAltitudeAzimuth <- renderText({
        date <- input$date
        time <- input$time + 24 / 360 * (longitude_to_right_ascension(planet_longitude(date, input$elongation)) - longitude_to_right_ascension(sun_longitude(date)))
        latitude <- input$latitude
        elongation <- input$elongation
        declination <- planet_declination(date, elongation)
        alt_az <- altitude_azimuth(latitude, declination, time)
        paste("Planet Altitude: ", round(alt_az$altitude, 2), "\nPlanet Azimuth: ", round(alt_az$azimuth, 2))
    })
}

shinyApp(ui = ui, server = server)

