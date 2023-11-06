library (rvest)
library (dplyr)

linl = "..."
page = read_html(link)

dest.port = page %>% html_nodes("...") %>% html_text()
cur.port = page %>% html_nodes("...") %>% html_text()
v.type = page %>% html_nodes("...") %>% html_text()
l.area = page %>% html_nodes("...") %>% html_text()
lat = page %>% html_nodes("...") %>% html_text()
lon = page %>% html_nodes("...") %>% html_text()
speed = page %>% html_nodes("...") %>% html_text()
year = page %>% html_nodes("...") %>% html_text()
capacity = page %>% html_nodes("...") %>% html_text()

marineTraffic = data.frame(destination, current_port, vessel_type, local_area, latitude, longitue, speed, built, capacity_tonnage, stringAsFactors = FALSE)
write.csv(marineTraffic, "marineTraffic.csv")
