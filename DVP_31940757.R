#---------------------------Importing Libraries---------------------------------
#Tidyverse for better transform and tidy presentation of data
library(tidyverse)
#Dplyr for easy data manipulation
library(dplyr)
#libraries for data visualization
library(ggplot2)
library(plotly)
#Shiny for building web applications
library(shiny)
#Leaflet for creating dynamic online maps
library(leaflet)
# libraries for plotting the countires on the map
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
#---------------------------Data Reading----------------------------------------
igla_df <- read_csv('final_igla_processed.csv')
religion_df <- read_csv('final_religion_processed.csv')
gti_df <- read_csv('final_gti_processed.csv')
#-----------------------------Data Manipulation---------------------------------
colnames(igla_df)[colnames(igla_df) == "COUNTRY"] <- "Country"
igla_df <- igla_df[, -1]
combined_df <- merge(igla_df, religion_df, by = "Country", all = TRUE)
# Load the country polygons dataset
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge the country data with the polygons
merged_data <- merge(world, combined_df, by.x = "name", by.y = "Country", all.x = TRUE)
#Grouping data according to Legality of Homosexuality
mapping_df <- combined_df %>%
  group_by(LEGAL) %>%
  summarise(Count = n(),.groups = "drop")%>%
  na.omit()%>%
  as.data.frame()

#Creating color palettes to associate the legal status
colPal <- colorFactor(palette = "Dark2", domain=mapping_df$Legal)

# Calculate the percentage of countries where homosexuality is legal
percentage_data <- combined_df %>%
  count(LEGAL) %>%
  mutate(Percentage = n / sum(n) * 100)  %>%
  na.omit()
# color pallet for pie chart
colors <- c('rgb(211,94,96)', 'rgb(0,128,0)', 'rgb(144,103,167)')

#------------------------------Map 2--------------------------------------------
#filtering countries where legal status is NO
filtered_data <- combined_df %>% 
  filter(LEGAL == "NO")
# Grouping data based on religion
mapping_df_2 <- filtered_data %>%
  group_by(Religion) %>%
  summarise(Count = n(),.groups = "drop")%>%
  as.data.frame()

#Creating color palettes to associate the different religion
colPal2 <- colorFactor(palette = "Dark2", domain=mapping_df_2$Religion)

# Merge the country data with the polygons
merged_data2 <- merge(world, filtered_data, by.x = "name", by.y = "Country", all.x = TRUE)
merged_data2 <- st_as_sf(merged_data2)

#-----------------------------Penalty Distribution------------------------------
# calculates percentage distribution based on maximum penalty
percentage_data2 <- combined_df[combined_df$MAX_PENALTY != "-", ] %>%
  group_by(MAX_PENALTY) %>%
  summarise(Count = n()) %>%
  na.omit() %>%
  arrange(Count)
#-----------------------------Trend Lines---------------------------------------
# Sort the data frame by score in descending order
gti_df_sorted <- gti_df %>%
  arrange(desc(Total))

# Get the top 5 countries
top_countries <- gti_df_sorted %>%
  distinct(Country) %>%
  slice_head(n = 5) %>%
  pull(Country)

# Filter the data frame for the top 5 countries
df_top_countries <- gti_df_sorted %>%
  filter(Country %in% top_countries)

gti_df_sorted <- gti_df %>%
  arrange(Total)

# Get the bottom 5 countries
bottom_countries <- gti_df_sorted %>%
  distinct(Country) %>%
  slice_head(n = 5) %>%
  pull(Country)

# Filter the data frame for the bottom 5 countries
df_bottom_countries <- gti_df_sorted %>%
  filter(Country %in% bottom_countries)

#-----------------------------Shiny App-----------------------------------------
# Defining the UI component of the web page
ui<- fixedPage(
  tags$head(
    tags$style(
      HTML("
        .custom-panel {
          background: linear-gradient(to right, red, orange, yellow, green, blue, indigo, violet);
          padding: 20px;
          color: black;
        }
      ")
    )
  ),
  
  wellPanel(
    class = "custom-panel",
    h1("Diversity and Inclusion",
       align = "center",
       style = "font-size: 50px; text-decoration: underline; font-weight: bold;"
    ),
    h4("A dive into queer rights and acceptance around the globe.",
       align = "center",
       style = "font-size: 30px; font-weight: bold;"
    )
  ),
  fluidRow(
    column(
      width = 12,
      wellPanel( h4("The map below displays the countries which provides legal
                    status for the queer community. The dropdown next to it helps
                    users to see what countries offer the selected rights and
                    laws."),
                 style = "background-color: #FFFFB3;")
    ),
    column(
      width = 9,
      leafletOutput("map1"), # Add a placeholder for the map
      
    ),
    column(
      width = 3,
      selectInput(
        "dropdown",
        label = "Select Category",
        choices = c("Legality",
          "Constitutional Protection",
          "Employment Protection",
          "Ban on Conversion Therapy",
          "Marriage Rights",
          "Joint Adoption"
        )
      )
    )
  ),
  fluidRow(
    column(width = 12,
           wellPanel( h4("Pie-Chart below helps the users to understand the
                         percentage distribution of legality and other human rights for
                        queer community across the globe. The Pie-Chart shows the
                         distribution for the rights selected above from the 
                         drop dowm."),
                      style = "background-color: #FFFFB3;")
           ),
    column(
      width = 12,
      plotlyOutput("pie")
    )
  ),
  fluidRow(
    column(
      width = 8,
      leafletOutput("map2")  # Add a placeholder for the map2
    ),
    column(
      width = 4,
      wellPanel( h4("Religion is one of the reasons which might impact in 
      acceptance of the community among the people around. The map on the right 
      highlights the coutries where being queer is illegal and their most followed
      religion. Religion plays one crucial role in acceptance. 
      Islam and Christianity straight away claims this as sin 
      and people have their reservations about this being natural.
      "), style = "background-color: #FFFFB3;")
    )
  ),
  fluidRow(
    column(width = 3,
      wellPanel(h4("The donut chart on the right discusses the distribution of
      severity and types of punishment for LGBTQIA+ society around the globe.
      Hovering over the section of the dinut we can see the type pf punishment
      and count of the countries following those."),
                style = "background-color: #FFFFB3;")
    ),
    column(
      width = 9,
      plotlyOutput("donutChart")
    )
  ),
  fluidRow(
    column(width = 12,
     wellPanel( h4("Gay travel index provides the dataset; ranking the countries
     in their most to least likely to be visited by the community members. A 
     comparative analysis on the most and least preferred countries over the years is 
     shown in the line graph below. The graph can be toggled between the two
     using the radio buttons.
      "), style = "background-color: #FFFFB3;"))
  ),
  
  fluidRow(
    column(width = 3,
           align = "center",
           radioButtons("toggleGraph", label = "Toggle Graph", 
                              choices = c("Most Traveled", "Least Traveled"),
                              selected = "Most Traveled")),
    column(width = 9,
    plotlyOutput("lineGraph"))
  )
)

#Server component for Shiny webpage
server <- function(input, output) {
  selected_category <- reactive(input$dropdown)
  
  output$map1 <- renderLeaflet({
    filtered_data <- merged_data %>%
      filter(LEGAL == "YES") # Initialize filtered_data
    if (selected_category() == "Constitutional Protection") {
      filtered_data <- merged_data %>%
        filter(CONSTITUTIONAL_PROTECTION == "YES")
    } else if (selected_category() == "Employment Protection") {
      filtered_data <- merged_data %>%
        filter(EMPLOYMENT_PROTECTION == "YES")
    } else if (selected_category() == "Ban on Conversion Therapy") {
      filtered_data <- merged_data %>%
        filter(CONVERSION_THERAPY_BAN == "YES")
    } else if (selected_category() == "Marriage Rights") {
      filtered_data <- merged_data %>%
        filter(SAME_SEX_MARRIAGE == "YES")
    } else if (selected_category() == "Joint Adoption") {
      filtered_data <- merged_data %>%
        filter(JOINT_ADOPTION == "YES")
    }
    
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = filtered_data,
        fillColor = "purple",
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~LEGAL,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
  
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = merged_data2,
        fillColor = ~colPal2(Religion),
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        label = ~Religion,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "black",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
  })
  output$pie <- renderPlotly({
    # Calculate the percentage of countries where homosexuality is legal or not
    percentage_data <- combined_df %>%
      count(LEGAL) %>%
      mutate(Percentage = n / sum(n) * 100)  %>%
      na.omit()
    
    colors <- c('rgb(211,94,96)', 'rgb(0,128,0)', 'rgb(144,103,167)')
    if (selected_category() == "Constitutional Protection") {
        percentage_data <- combined_df %>%
        count(CONSTITUTIONAL_PROTECTION) %>%
        mutate(Percentage = n / sum(n) * 100)  %>%
        na.omit()
    } else if (selected_category() == "Employment Protection") {
        percentage_data <- combined_df %>%
        count(EMPLOYMENT_PROTECTION) %>%
        mutate(Percentage = n / sum(n) * 100)  %>%
        na.omit()
    } else if (selected_category() == "Ban on Conversion Therapy") {
        percentage_data <- combined_df %>%
        count(CONVERSION_THERAPY_BAN) %>%
        mutate(Percentage = n / sum(n) * 100)  %>%
        na.omit()
    } else if (selected_category() == "Marriage Rights") {
        percentage_data <- combined_df %>%
        count(SAME_SEX_MARRIAGE) %>%
        mutate(Percentage = n / sum(n) * 100)  %>%
        na.omit()
    } else if (selected_category() == "Joint Adoption") {
        percentage_data <- combined_df[combined_df$JOINT_ADOPTION != "-", ] %>%
        count(JOINT_ADOPTION) %>%
        mutate(Percentage = n / sum(n) * 100)  %>%
        na.omit()
    }
    plot_ly(
      percentage_data,
      labels = ~percentage_data[, 1],
      values = ~Percentage,
      type = 'pie',
      textposition = 'inside',
      textinfo = 'label+percent',
      insidetextfont = list(color = '#FFFFFF'),
      hoverinfo = 'text',
      text = ~paste(Percentage, '%'),
      marker = list(
        colors = colors,
        line = list(color = '#FFFFFF', width = 1)
      ),
      showlegend = TRUE
    ) %>% 
      layout(
        title = paste("Percentage Distribution of ", selected_category()),
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })
  # Generate the line graph using plotly
  output$lineGraph <- renderPlotly({
    if (input$toggleGraph == "Most Traveled") {
      df <- df_top_countries
      title <- "Five most visited countries over the years"
      color_values <- c("red", "blue", "green", "orange", "purple")
    } else {
      df <- df_bottom_countries
      title <- "Five least visited countries over the years"
      color_values <- c("purple", "orange", "green", "blue", "red")
    }
    
    p <- ggplot(df, aes(x = Year, y = Total, color = Country, group = Country)) +
      geom_line() +
      geom_point() +
      labs(x = "Year", y = "Score", title = title) +
      scale_color_manual(values = color_values) +
      theme_minimal()
    
    # Convert ggplot object to plotly
    p <- ggplotly(p)
    
    # Return the plotly object
    return(p)
  })
  output$donutChart <- renderPlotly({
    # Create an interactive donut chart
    donut_chart <- plot_ly(
      data = percentage_data2,
      labels = ~MAX_PENALTY,
      values = ~Count,
      type = 'pie',
      hole = 0.6,
      hoverinfo = 'text',
      text = ~paste("Punishment in years: ", MAX_PENALTY, "<br>Count: ", Count),
      textinfo = 'percent',
      textposition = 'inside',
      name = 'Punishment in years',
      legendgroup = 'Punishment in years'
    )
    
    # Customize the chart layout
    layout(donut_chart, title = 'Percentage Data', showlegend = TRUE)
    
    # Return the interactive chart
    donut_chart
  })
}

shinyApp(ui = ui, server = server)
