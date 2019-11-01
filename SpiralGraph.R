# Generate Spiral Graph of UV Index data from ARPANSA
# Maximum UV for a given day is used as the feature point

# Author: Asaf Silman (Twitter: @AsafSilman)

library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
library(gganimate)
library(viridis)

# dataSources contains a list of all datasets provided by ARPANSA
dataSource <- read_csv("dataSources.csv") %>% filter(active==1)
framesToAnimate = 500

# for (j in c(1:1)) { # Used for testing
for (j in c(1:nrow(dataSource))) {
  # Create the data url from the ID in dataSource
  dataURL <- paste0("https://data.gov.au/api/3/action/package_show?id=", dataSource$dataID[j])
  locationName <- dataSource$location[j]

  
  # Fetch the dataset meta data, which includes all the dataset resources.
  # the original data is a minute resolution of each day for a given year of
  # the UV index
  r <- GET(dataURL)
  metaData <- content(r, "parsed")
  dataResources <- length(metaData$result$resources)
  
  # Create a dataframe with required columns. Then iterate through all
  # dataset resources, adding data from each year to dataframe
  
  # Initialise column names
  columnNames <- c("timestamp", "latitude", "longitude", "uv_index")
  
  # create empty dataframe and column names
  df <- data.frame(matrix(ncol = length(columnNames), nrow = 0))
  colnames(df) <- columnNames
  
  # Load data resource from cache, if it doesn't exist download it
  for (sourceId in c(1:dataResources)){
    resource <- metaData$result$resources[[sourceId]]
    destFile <- paste0("./data/", resource$id, ".csv")
    fileUrl <- resource$url
    if (!file.exists(destFile)) {
      download.file(fileUrl, destFile, method="auto")
    }
    
    data_ <- read_csv(destFile)
    colnames(data_) <- columnNames # rename columns
    df <- rbind(df, data_) # add data to master dataframe
    rm(data_, fileUrl, destFile, resource) # remove redundant variables
  }
  df <- type_convert(df, cols(uv_index = col_double())) # convert uv_index to double
  
  # Summarise the dataframe, this will calculate the maximum uv for a given day.
  # Then add some other values to summary which are required for animation.
  # The data is grouped by year-weekNum for each key-frame
  df_summary <- df %>% 
    filter(timestamp > as.Date("2009-01-01")) %>%
    filter(uv_index > 0) %>%
    mutate(date_col = date(timestamp)) %>%
    group_by(date_col) %>%
    summarize(value = max(uv_index)) %>%
    mutate(year_week_value=sprintf("%d-Wk%02d", year(date_col), week(date_col))) %>% # This is used for grouping data
    mutate(year_month=format(date_col, format="%Y-%m")) %>%
    mutate(day_of_year=yday(date_col)) %>% # calculate day in year
    mutate(angle=day_of_year/366 * 2*pi) %>% # calculate angle as fraction of year
    arrange(date_col) %>% # make sure the data is sorted by date
    group_by(year_week_value) %>% # Generate groups
    mutate(group_id=group_indices()) %>% ungroup() %>% # Generate group ids then ungroup
    mutate(position = 1:n()) # this is needed for gganimate
  
  
  plotTitle <- paste("Daily Maximum", metaData$result$title)
  
  # generate animation
  anim <- df_summary %>%
    ggplot(aes(x=angle, y=value, color=value, group=position)) + # as per docstrings group needs to be assigned
    geom_point() +
    
    # Add ticks for months
    scale_x_continuous(limits=c(0,12)/12*2*pi, breaks=0:11/12*2*pi, minor_breaks=0:12/12*2*pi,
                       labels=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
    # Settings hard limit of 50 for UV
    scale_y_continuous(breaks=seq(0, 50, 3)) + # Maximum UV ever recorded was 43.3 https://www.livescience.com/46701-andes-highest-uv-index-measured.html
    coord_polar(theta="x") +
    
    # Color scale using UV Index standard colors. Modified a bit for asthetics
    scale_colour_gradientn(
      colours = c("#85DE77", "#FFF49C", "#FFB347", "#FF756D", "#D291BC", "#CC6BB1", "#9C1A87"),
      values = c(0, 3, 5, 8, 11, 15, 50),
      rescaler = function(x, ...) x
    ) +
    theme_linedraw() +
    theme(plot.caption = element_text(hjust = 0)) +
    
    # Add labels and captions
    labs(
      title=paste(plotTitle, "- {(df_summary %>% filter(group_id==frame_time) %>% select(year_month))[[1]][1]}"),
      x="Month",
      y="UV Index",
      color="UV Index",
      caption = "Data Source: Australian Radiation Protection and Nuclear Safety Agency (ARPANSA)\nCreated by: Asaf Silman (t: @AsafSilman)"
    ) +
    
    # Transition through groups (grouped by year-weekNum), and add exit_lenngth for animation. Cut off animation when data ends
    transition_components(group_id, exit_length=as.integer(20), range = c(as.integer(1), max(df_summary$group_id))) +
    
    # Add fade + shrink on data
    exit_fade(alpha=0.3) +
    exit_shrink(size=0.5) +
    shadow_mark(alpha=0.3, color='gray', size=0.5) # Show mark of past data
  
  # Save animation to file using ffmpeg, but any renderer is fine
  anim_save(
    paste0("output/", locationName, ".mp4"),
    animate(anim, nframes=framesToAnimate, fps=25, renderer = ffmpeg_renderer())
  )
}
