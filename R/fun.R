#' Helper for gg_cal_plot
#' @noRd
get_week_month<- function(x, y){
  d <-  as.numeric(format(x, "%U"))
  d2 <-  as.numeric(format(y, "%U"))
  (d - d2) + 1
}

#' Helper for gg_cal_plot
#' @noRd
strava_athlete_activities_api <- function(oauth_token, page_number, page_size = 50) {
  print(paste0("Querying Strava V1 API v3/athlete/activities [page ", page_number, "]"))
  resp <- GET(
    url = "https://www.strava.com/api/v3/athlete/activities",
    config = oauth_token,
    query = list(per_page = page_size, page = page_number))

  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
    break
  }

  # Strava API returns data in JSON format, so parse it and return as a data.frame
  jsonlite::fromJSON(content(resp, "text"), flatten = FALSE)
}


#' Helper for gg_cal_plot
#' @noRd
get_strava_data <- function(token){
  # A list to store the returned data into
  data_pages <- list()

  # Start with page #1
  page_number <- 1

  repeat {
    strava_data_page <- strava_athlete_activities_api(token, page_number)
    if(length(strava_data_page)==0) break

    # Query API
    data_pages[[page_number]] <- strava_data_page

    # Count number of records returned
    records_returrned <- data_pages[[page_number]] %>% nrow()
    print(paste0('Retrieved ', records_returrned, ' records'))

    # Continue to the next page if the number of returned records matches the page size
    if (records_returrned < 50) {
      print("All records returned, exiting")
      break
    } else {
      print("checking for more records ..")
      page_number <- page_number + 1
    }
  }

  data <- rbind_pages(data_pages)

}

#' Plot running calendar from Strava
#'
#' @param data a dataframe from Strava API
#' @param year current year to plot
#' @param current_year logical. Is year the current year.
#'
#' @return a ggplot
#' @export
gg_run_cal <- function(data, year, current_year){
  # Combine the list of data frames into a single dataframe of all collected pages
  data_year <-
    data %>%
    dplyr::filter(year(start_date_local) == year & type == 'Run')

  completed <- round(sum(data_year$distance*0.000621371), 2)
  to_go <- year-completed

  # day_of_year <- yday(with_tz(Sys.Date(), 'America/New_York'))
  # goal <- round(year/365*day_of_year, 2)

  run_data <- data_year %>%
    tidyr::separate(start_date_local, c('date', 'time'), sep = 'T') %>%
    dplyr::mutate(date = ymd(date),
                  #week = isoweek(date),
                  #week_of_month = get_week_of_month(date),
                  weekday = wday(date, label = T),
                  day_of_month = mday(date),
                  month = month(date, label = T),
                  year = year(date),
                  first_day = '01') %>%
    tidyr::unite(c(month, year), col = 'month_year', sep = ' ', remove = F) %>%
    tidyr::unite(c(year, month, first_day), col = 'first_day_of_month', sep = '-', remove = F) %>%
    dplyr::mutate(first_day_of_month = ymd(first_day_of_month),
           week_of_month = get_week_month(date, first_day_of_month)) %>%
    # week_of_first_day = isoweek(first_day_of_month),
    # week_of_month = (week - week_of_first_day) + 1 )%>%
    dplyr::filter(type == 'Run')


  date_from <- paste(year, "01", "01", sep = "-")
  if(current_year){
    date_to <- lubridate::today()
  } else {
    date_to <- paste(year, "12", "31", sep = "-")
  }


  all_dates <- tibble::tibble(date = seq(ymd(date_from), ymd(date_to) , by="day")) %>%
    dplyr::mutate(date = ymd(date),
                  weekday = wday(date, label = T),
                  day_of_month = mday(date),
                  month = month(date, label = T),
                  year = year(date),
                  first_day = '01') %>%
    tidyr::unite(c(month, year), col = 'month_year', sep = ' ', remove = F) %>%
    tidyr::unite(c(year, month, first_day), col = 'first_day_of_month', sep = '-', remove = F) %>%
    dplyr::mutate(first_day_of_month = ymd(first_day_of_month),
           week_of_month = get_week_month(date, first_day_of_month))


  run_data %<>% dplyr::full_join(all_dates) %>%
    dplyr::mutate(mileage =  round((distance*0.000621371), 2)) %>%
    dplyr::group_by(month_year, date, weekday, week_of_month)

  month_years <- unlist(purrr::map(c(seq(2019, 2100, by = 1)), ~paste(month.abb, .)))

  run_data$month_year <- factor(run_data$month_year, levels = month_years)

  cal_data <-
    run_data %>%
    dplyr::summarize(Mileage = sum(mileage))


  with_data <- run_data %>%
    dplyr::mutate(With =
             case_when(
               str_detect(tolower(name), 'myo') ~ "Dog",
               str_detect(tolower(name), "stroller|girl|mar|mol|anna|baby") ~ "Daughter(s)"
             )) %>%
    dplyr::group_by(month_year, date, weekday, week_of_month, With) %>%
    dplyr::filter(!is.na(With)) %>%
    dplyr::summarize(Mileage = sum(mileage))


  ggplot2::ggplot(cal_data, aes(weekday, -week_of_month)) +
    geom_tile(aes(fill = Mileage),
              color = "grey40")+
    geom_tile(data = with_data,
              aes(x = weekday, y=  -week_of_month, fill = Mileage, color = With), size = 1)+
    geom_text(aes(label = Mileage), size = 3)+
    facet_wrap(~month_year, ncol = 3) +
    scale_fill_gradient(low = "yellow", high = "red", na.value = 'grey20')+
    scale_y_continuous(name = '',
                       labels = NULL,
                       breaks = NULL)+
    xlab('')+
    theme_linedraw(base_size = 14)+
    scale_color_manual(values = c("hotpink", 'steelblue'))+
    theme(panel.grid = element_blank(),
          axis.ticks = element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "#2C2B2B", color = "#2C2B2B"),
          plot.background = element_rect(fill = "#2C2B2B", color = "#2C2B2B"),
          legend.background = element_rect(fill = "#2C2B2B", color = "#2C2B2B"),
          text = element_text(color = "white"),
          axis.text.x  = element_text(color = "white"),
          strip.text = element_text(color = "#2B82E1"),
          strip.background =  element_rect(fill = "#2C2B2B", color = "#2C2B2B")
    )

}
