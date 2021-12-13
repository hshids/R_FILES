# 1. import the package and the path of directory
########
library(RCurl)
library(RJSONIO)
library(XML)
library(plotly)
library(reshape2)

# the data used to draw the plot saved here
data_dir <- "D:/study/R_plot/task/data"
# all results will be saved here
work_dir <- "D:/study/R_plot/task/Results"
# set working directory
setwd(work_dir)
######## 

test <- readRDS("ca.counties.covid.rds")


#2. data processing
######## 
# generate the path of data
tt = paste(data_dir, "USA.json", sep = "/")
# import data
us = fromJSON(tt)
length(us$data)

# got the county and state names in data
geoid <- lapply(us$data, '[[', c('geoid'))
display_name <- lapply(us$data, '[[', c('display_name'))
CA_counties_index_in_data <- which(substr(geoid, 5, 6) == "06")
# check the names and remove "California" and "Unknown" 
unlist(display_name[CA_counties_index_in_data])
CA_counties_index_in_data <- CA_counties_index_in_data[-1]
CA_counties_index_in_data <- CA_counties_index_in_data[-length(CA_counties_index_in_data)]
unlist(display_name[CA_counties_index_in_data])
length(CA_counties_index_in_data)

# subset the data of counties CA
CA_counties_data <- us
CA_counties_data$data <- us$data[CA_counties_index_in_data]
# generate the date sequence
daterange <- seq.Date(from = as.Date("2020-03-01",format = "%Y-%m-%d"), by = "day", length.out = 282)
# generate the cases data frame
case.df <- data.frame("date" = daterange)
for (i in 1:length(CA_counties_data$data)) {
  case.df[ ,CA_counties_data$data[[i]]$display_name] <- CA_counties_data$data[[i]]$cases
}
# melt data frame
case.df.melted <- melt(case.df,id.vars = "date", variable.name="county",value.name="cases")


# generate the counties data frame
info.df <- data.frame()
for (i in 1:length(CA_counties_data$data)) {
  info.df[i,"county"] <- CA_counties_data$data[[i]]$display_name
  info.df[i,"latest cases"] <- (
    CA_counties_data$data[[i]]$cases[length(CA_counties_data$data[[i]]$cases)]
    - CA_counties_data$data[[i]]$cases[length(CA_counties_data$data[[i]]$cases)-1]
  )
  info.df[i,"latest deaths"] <- (
    CA_counties_data$data[[i]]$deaths[length(CA_counties_data$data[[i]]$deaths)]
    - CA_counties_data$data[[i]]$deaths[length(CA_counties_data$data[[i]]$deaths)-1]
  )
  info.df[i,"total cases"] <- CA_counties_data$data[[i]]$latest$cases
  info.df[i,"total deaths"] <- CA_counties_data$data[[i]]$latest$deaths
  info.df[i,"population"] <- CA_counties_data$data[[i]]$population
}

info.df[, "latest cases proportion"] <- paste(round(info.df$`latest cases` / info.df$population, 4)*100, "%", sep='')
info.df[, "latest deaths proportion"] <- paste(round(info.df$`latest deaths` / info.df$population, 4)*100, "%", sep='')

info.df[, "label"] <- paste("latest cases: ", info.df$`latest cases`, 
                            " (", info.df$`latest cases proportion`, ")",
                            "\nlatest deaths: ", info.df$`latest deaths`, 
                            " (", info.df$`latest deaths proportion`, ")",
                            "\ntotal cases: ", info.df$`total cases`,
                            "\ntotal deaths: ", info.df$`total deaths`, sep = "")

info.df.label <- info.df[, c("county", "label")]
# add county information to case.df.melted
final.df <- merge(case.df.melted, info.df.label, by.x = "county", by.y = "county")
# set the highlight key
final.df.hl <- highlight_key(final.df, key=~county)

#3.1 draw the time series plot
######## 
p <- plot_ly(final.df.hl, x = ~date, y = ~cases, color = ~county, text = ~label, type = "scatter", mode = "lines") %>%
  highlight(on='plotly_hover', off='plotly_doubleclick', dynamic=TRUE) %>% 
  layout(
    title = "Button Restyle",
    xaxis = list(domain = c(0.1, 1)),
    yaxis = list(title = "y"),
    updatemenus = list(
      list(
        type = "buttons",
        y = 0.8,
        buttons = list(
          list(method = "restyle",
               args = list("visible", "all"),
               label = "show all"),
          list(method = "restyle",
               args = list("visible", "legendonly"),
               label = "hide all")
        )
      )
    )
  )

p
# save the plot as html
tmp = ggplotly(p)
htmlwidgets::saveWidget(tmp, "CA_counties_cases.html")
########



#3.2 draw the map plot
######## 
######## !!!!!! run the code below at first time to download the map data
# library(rjson)
# url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
# all_maps <- fromJSON(file=url)
# saveRDS(all_maps, file = "all_maps.rds")

# load RDS file of maps
all_maps <- readRDS("all_maps.rds")
GEO_ID <- lapply(lapply(all_maps$features, '[[', c('properties')), '[[', c('GEO_ID'))
GEO_ID_short <- substr(GEO_ID, 10, 14)

# generate the cases dataframe for map plot
map.df <- data.frame()
for (i in 1:length(us$data)){
  if(substr(us$data[[i]]$geoid, 5, 9) %in% GEO_ID_short){
    map.df[(nrow(map.df) + 1), "county"] <- us$data[[i]]$display_name
    map.df[(nrow(map.df)), "geoid"] <- substr(us$data[[i]]$geoid, 5, 9)
    map.df[(nrow(map.df)), "cases"] <- us$data[[i]]$latest$cases
  }
}

map.df$percent <- round(map.df$cases / sum(map.df$cases), 6) * 100
map.df$label <- paste(round(map.df$cases / sum(map.df$cases), 6) * 100, "%", sep='')


# draw the map
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('lightblue')
)
map <- plot_ly()
map <- map %>% add_trace(
  type="choropleth",
  geojson=all_maps,
  locations=map.df$geoid,
  z=map.df$percent,
  colorscale="Viridis",
  zmin=0,
  zmax=0.5,
  marker=list(line=list(width=0))
)

map <- map %>% colorbar(title = "Cases Proportion (%)")
map <- map %>% layout(title = "2020 US Covid-19 Cases by County")
map <- map %>% layout(geo = g)

map

# save the plot as html
tmp = ggplotly(map)
htmlwidgets::saveWidget(tmp, "cases_map_colored_by_percent.html")
########








