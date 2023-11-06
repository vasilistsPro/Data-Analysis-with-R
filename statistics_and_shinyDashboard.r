# libraries for the plots

install.packages("esquisse")
library(esquisse)
library(ggplot2)
library(readr)

marineTraffic = read_csv(".../marineTraffic.csv")
View(marineTraffic)
colnames(marineTraffic)
globalVariables(names(marineTraffic))

# first diagram 2.1
ggplot(marineTraffic)
    aes(x = `Vessel Type - Generic`) + 
    geom_bar(fill = "#0C4C8A") +
    labs(
        x = "Τύπος πλοίου",
        y = "Συχνότητα",
        title = "Κατανομή των πλοίων ανά κατηγορία"
    ) +
    theme_linedraw()


# second diagram 2.1
ggplot(marineTraffic)
    aes(x = `Local Area`) + 
    geom_bar(fill = "#4682B4") +
    labs(
        x = "Περιοχή πλοίων",
        y = "Συχνότητα",
        title = "Κατανομή των πλοίων ανά περιοχή"
    ) +
    theme_linedraw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# third diagram 2.1
ggplot(marineTraffic)
    aes(x = `Local Area`, colour = `Vessel Type - Generic`) + 
    geom_bar(aes(fill = `Vessel Type - Generic`), color = "black") +
    scale_color_hue(direction = 1) +
    labs(
        x = "Περιοχή πλοίων",
        title = "Κατανομή και είδη πλοίων ανά περιοχή"
    ) +
    theme_linedraw() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# diagram 2.2
ggplot(marineTraffic) +
    aes(x = `Vessel Type - Generic`, y = Speed) + 
    geom_point(
        shape = "diamond",
        size = 1.5,
        colour = "#020202"
    ) +
    labs(
        x = "Είδος πλοίου",
        y = "Ταχύτητα",
        title = "Διάγραμμα συσχέτισης μεταξύ είδους πλοίου και ταχύτητας"
    ) +
    theme_linedraw()

 # diagram 2.3
# Count the number of vessels in port and at sea
num_vessels_in_port <- sum(!is.na(marineTraffic$`Current Port`))
num_vessels_at_sea <- sum(is.na(marineTraffic$`Current Port`))

# Create a pie chart
labels <- c("Vessels in Port", "Vessels at Sea")
sizes <- c(num_vessels_in_port, num_vessels_at_sea)
colors <- c("lightgray", "lightblue")

pie(sizes, labels = labels, col = colors,
    main = "Distribution of Vessels: Port vs. Sea")

# diagram 2.4
ggplot(marineTraffic) +
    aes(x = Built, y = `Capacity - Dwt`) +
    geom_point(shape = "circle", size = 1.5, colour = "#ed0909") +
    scale_y_continuous(trans = "log2") +
    labs(
        x = "Χρονολογία ναυπήγησης"
        y = "Χωρητικότητα",
        title = "Διάγραμμα χρονολογίας ναυπήγησης - Χωρητικότητας"
    ) +
    theme_linedraw()


# diagram longitude - latitude
install.packages("plotly")
library(plotly)
graph = plot_geo(marineTraffic, lat =~ Latitude, lon =~ Longitude)
graph

library(dplyr)

# 3.1
avg_speed_by_vessel_type <- marineTraffic %>%
    group_by(`Vessel Type - Generic`) %>%
    summarise(avg_speed = mean(Speed, na.rm = TRUE))
# print the result
avg_speed_by_vessel_type

# 3.2
avg_speed_by_local_area <- marineTraffic %>%
    group_by(`Local Area`) %>%
    summarise(avg_speed = mean(Spead, na.rm = TRUE))
# print the result
avg_speed_by_local_area

# 3.3
# count the frequency of each local area and sort in descendig order
area_freq <- sort(table(marineTraffic$`Local Area`), decreasing = TRUE, na.last = FALSE)

# print the local area with the highest frequency
cat("Local area with the highest frequency: ", names(area_freq[1], "\n"))


# 3.4
# sort the data by Capacity - Dwt column in descending order and select the top row
vessel_with_largest_capacity <- marineTraffic %>%
    arrange(desc(`Capacity - Dwt`)) %>%
    slice(1)

# sort the data by Capacity - Dwt column in ascedning order and select the top row
vessel_with_smallest_capacity <- marineTraffic %>%
    arrange(`Capacity - Dwt`) %>%
    slice(1)

# print the result
cat("Vessel with the largest capacity: ", vessel_with_largest_capacity$`Vesse Type - Generic`, "\n")
cat("Vessel with the smallest capacity: ", vessel_with_smallest_capacity$`Vesse Type - Generic`, "\n")


# 3.5
# combine the Destination Port and Current Port columns and count the frequency of each port
port_freq <- sort(table(c(marineTraffic$`Destination Port`, marineTraffic$`Current Port`)), decreasing = TRUE, na.last = FALSE)

# print the port with the highest frequency
cat("Port with the highest frequency: ", names(port_freq)[1], "\n")


# 3.6
# count the frequency of each vessel type and sort in descending order
vessel_freq <- sort(table(marineTraffic$`Vessel Type - Generic`), decreasing = TRUE, na.last = FALSE)

# print the vessel type with the highest frequency
cat("Vessel type with the highest frequency: ", names(vessel_freq)[1], "\n")


# shiny dashboard
install.packages('shinydashboard')
library(shiny)
library(shinydashboard)

# UI Side
ui <- dashboardPage(
    dashboardHeader(title = "Marine Traffic Analysis"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Distributions1", tabName = "dashboard", icon = icon("dashboard")),
            menuItem("Distributions2", tabName = "distros", icon = icon("th")),
            menuItem("Plots", tabName = "plots", icon = icon("th")),
            menuItem("World Map", tabName = "world_map", icon = icon("th"))
        )
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        tabItems(
            # First tab content
            tabItem(tabName = "dashboard",
                    fluidRow(
                        box(plotOutput("dships_by_category"), height = 250),
                        box(plotOutput("ships_by_area"), height = 250)
                        )
                    ),
        
            # Second tab content
            tabItem(tabName = "plots",
                    fluidRow(
                        box(plotOutput("distr_and_category"), height = 250),
                        box(plotOutput("vessel_distribution"), height = 250)
                        )
                    ),
            # Third tab content
            tabItem(tabName = "plots",
                fluidRow(
                    box(plotOutput("category_speed"), height = 250),
                    box(plotOutput("year_capacity"), height = 250)
                )
            ),

            # Fourth tab content
            tabItem(tabName = "world_map",
                    fluidRow(
                        box(plotlyOutput("world_map"), width = 450, heght = 450)
                    )
                )
        )
    )
)



# Server Side
server <- function(input, output) {
    output$ships_by_category <- renderPlot({
        ggplot(marineTraffic)
        aes(x = `Vessel Type - Generic`) + 
        geom_bar(fill = "#0C4C8A") +
        labs(
            x = "Τύπος πλοίου",
            y = "Συχνότητα",
            title = "Κατανομή των πλοίων ανά κατηγορία"
        ) +
        theme_linedraw()
    })

    output$ships_by_area <- renderPlot({
        ggplot(marineTraffic)
        aes(x = `Local Area`) + 
        geom_bar(fill = "#4682B4") +
        labs(
            x = "Περιοχή πλοίων",
            y = "Συχνότητα",
            title = "Κατανομή των πλοίων ανά περιοχή"
        ) +
        theme_linedraw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    })

    output$distr_and_category <- renderPlot({
        ggplot(marineTraffic)
        aes(x = `Local Area`, colour = `Vessel Type - Generic`) + 
        geom_bar(aes(fill = `Vessel Type - Generic`), color = "black") +
        scale_color_hue(direction = 1) +
        labs(
            x = "Περιοχή πλοίων",
            title = "Κατανομή και είδη πλοίων ανά περιοχή"
        ) +
        theme_linedraw() +
        theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))
    })

    output$vessel_distribution <- renderPlot({
        labels <- c("Vessels in Port", "Vessels at Sea")
        sizes <- c(num_vessels_in_port, num_vessels_at_sea)
        colors <- c("lightgray", "lightblue")

        pie(sizes, labels = labels, col = colors,
        main = "Distribution of Vessels: Port vs. Sea")
    })

    output$category_speed <- renderPlot({
        ggplot(marineTraffic) +
        aes(x = `Vessel Type - Generic`, y = Speed) + 
        geom_point(
            shape = "diamond",
            size = 1.5,
            colour = "#020202"
        ) +
        labs(
            x = "Είδος πλοίου",
            y = "Ταχύτητα",
            title = "Διάγραμμα συσχέτισης μεταξύ είδους πλοίου και ταχύτητας"
        ) +
        theme_linedraw()
    })

    output$year_capacity <- renderPlot({
        ggplot(marineTraffic) +
        aes(x = Built, y = `Capacity - Dwt`) +
        geom_point(shape = "circle", size = 1.5, colour = "#ed0909") +
        scale_y_continuous(trans = "log2") +
        labs(
            x = "Χρονολογία ναυπήγησης"
            y = "Χωρητικότητα",
            title = "Διάγραμμα χρονολογίας ναυπήγησης - Χωρητικότητας"
        ) +
        theme_linedraw()
    })

    output$world_map <- renderPlot({
        plot_geo(marineTraffic, lat =~ Latitude, lon =~ Longitude)
    })
}
shinyApp(ui, server)