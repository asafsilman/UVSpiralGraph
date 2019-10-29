###### Load

library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(viridis)

dataSource <- read_csv("dataSources.csv")
framesToAnimate = 500

for (j in c(1:nrow(dataSource))) {
  dataURL <- paste0("https://data.gov.au/api/3/action/package_show?id=", dataSource$dataID[j])
  locationName <- dataSource$location[j]

  r <- GET(dataURL)
  metaData <- content(r, "parsed")
  dataResources <- length(metaData$result$resources)
  
  
  plotTitle <- paste("Daily Maximum", metaData$result$title)
  columnNames <- c("timestamp", "latitude", "longitude", "uv_index")
  
  df <- data.frame(matrix(ncol = length(columnNames), nrow = 0))
  colnames(df) <- columnNames
  
  for (sourceId in c(1:dataResources)){
    resource <- metaData$result$resources[[sourceId]]
    destFile <- paste0("./data/", resource$id, ".csv")
    fileUrl <- resource$url
    if (!file.exists(destFile)) {
      download.file(fileUrl, destFile, method="auto")
    }
    data_ <- read_csv(destFile)
    colnames(data_) <- columnNames
    df <- rbind(df, data_)
    rm(data_, fileUrl, destFile, resource)
  }
  df <- type_convert(df, cols(uv_index = col_double()))
  
  df_summary <- df %>% 
    filter(timestamp > as.Date("2009-01-01")) %>%
    filter(uv_index > 0) %>%
    mutate(date_col = date(timestamp)) %>%
    group_by(date_col) %>%
    summarize(value = max(uv_index)) %>%
    mutate(year_month_week_value=paste(format(date_col, format="%Y-%m"), week(date_col))) %>%
    mutate(year_month=format(date_col, format="%Y-%m")) %>%
    mutate(day_of_year=yday(date_col)) %>%
    mutate(angle=day_of_year/366 * 2*pi) %>%
    arrange(date_col)%>%
    group_by(year_month_week_value) %>%
    mutate(group_id=group_indices()) %>% ungroup() %>%
    mutate(position = 1:n())
  
  anim <- df_summary %>%
    ggplot(aes(x=angle, y=value, color=value, group=position)) +
    geom_point() +
    scale_x_continuous(limits=c(0,12)/12*2*pi, breaks=0:11/12*2*pi, minor_breaks=0:12/12*2*pi,
                       labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    scale_y_continuous(breaks=seq(0, 50, 3)) +
    coord_polar(theta="x") +
    # scale_color_viridis(option = "C") +
    scale_colour_gradientn(
      colours = c("#85DE77", "#FFF49C", "#FFB347", "#FF756D", "#D291BC", "#CC6BB1", "#9C1A87"),
      values = c(0, 3, 5, 8, 11, 15, 50),
      rescaler = function(x, ...) x
    ) +
    theme_linedraw() +
    theme(plot.caption = element_text(hjust = 0)) +
    
    labs(
      title=paste(plotTitle, "- {(df_summary %>% filter(group_id==frame_time) %>% select(year_month))[[1]][1]}"),
      x="Month",
      y="UV Index",
      color="UV Index",
      caption = "Data Source: Australian Radiation Protection and Nuclear Safety Agency (ARPANSA)\nCreated by: Asaf Silman (t: @AsafSilman)"
    ) +
    transition_components(group_id, exit_length=as.integer(20), range = c(as.integer(1), max(df_summary$group_id))) +
    
    exit_fade(alpha=0.3) +
    exit_shrink(size=0.5) +
    shadow_mark(alpha=0.3, color='gray', size=0.5)
  
  
  anim_save(
    paste0("output/",locationName, ".mp4"),
    animate(anim, nframes=framesToAnimate, fps=25, renderer = ffmpeg_renderer())
  )
}







  











