library(shiny)
library(ggplot2)
library(dplyr)
library(scales)
library(showtext)
library(plotly)
library(ggrepel)

parkData <- read.csv("US-National-Parks_RecreationVisits_1979-2023.csv")

state_lookup <- data.frame(
  StateName = c("Alabama", "Alaska", "Arizona", "Arkansas", "American Samoa", "California",
                "Colorado", "Connecticut", "Delaware", "District of Columbia", "Florida", "Georgia",
                "Guam", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky",
                "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota",
                "Mississippi", "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota",
                "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Pennsylvania",
                "Puerto Rico", "Rhode Island", "South Carolina", "South Dakota", "Tennessee",
                "Texas", "Trust Territories", "Utah", "Vermont", "Virginia", "Virgin Islands",
                "Washington", "West Virginia", "Wisconsin", "Wyoming"),
  StateCode = c("AL", "AK", "AZ", "AR", "AS", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "GU", "HI",
                "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO",
                "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "MP", "OH", "OK", "OR", "PA",
                "PR", "RI", "SC", "SD", "TN", "TX", "TT", "UT", "VT", "VA", "VI", "WA", "WV", "WI", "WY")
)
parkSummary <- parkData %>%
  group_by(ParkName) %>%
  summarise(TotalVisits = mean(RecreationVisits, na.rm = TRUE)) %>%
  arrange(desc(TotalVisits))

top10Parks <- parkSummary$ParkName[1:10]

minPark <- parkData %>%
  filter(RecreationVisits == min(RecreationVisits, na.rm = TRUE)) %>%
  slice_min(Year) %>%
  pull(ParkName)

maxPark <- parkData %>%
  filter(RecreationVisits == max(RecreationVisits, na.rm = TRUE)) %>%
  pull(ParkName)

newParkData <- parkData %>%
  mutate(ColorCategory = case_when(
    ParkName %in% minPark ~ "Min",
    ParkName %in% maxPark ~ "Max",
    ParkName %in% top10Parks ~ "Top 10 (avg.)",
    TRUE ~ "Other"
  ),
  LineWidth = case_when(
    ParkName %in% minPark ~ 1,
    ParkName %in% maxPark ~ 0.9,
    ParkName %in% top10Parks ~ 0.7,
    TRUE ~ 0.5
  ))
endPoints <- newParkData %>%
  filter(Year == 2023 & (ParkName %in% top10Parks | ParkName %in% minPark))

labelData <- endPoints %>%
  mutate(Label = case_when(
    ParkName %in% maxPark ~ "Great Smoky Mountains NP",
    ParkName %in% minPark ~ "Kobuk Valley NP",  
    ColorCategory == "Top 10 (avg.)" ~ ParkName,
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Label))

colorPalette <- c("Min" = "#ca0000", 
                  "Max" = "#00b711", 
                  "Top 10 (avg.)" = "#7b9fdf", 
                  "Other" = "grey85")

state_avg_visits <- parkData %>%
  group_by(State) %>%
  summarise(AverageVisits = mean(RecreationVisits, na.rm = TRUE)) %>%
  left_join(state_lookup, by = c("State" = "StateCode")) %>%
  arrange(desc(AverageVisits)) %>%
  mutate(
    LabelColor = ifelse(AverageVisits == max(AverageVisits), "#b30000",
                        ifelse(AverageVisits == min(AverageVisits), "#00b311", "#333344")),
    BarColor = ifelse(AverageVisits == max(AverageVisits), "#00b311",
                      ifelse(AverageVisits == min(AverageVisits), "#b30000", "#7c8aa0")))

park_avg_visits <- parkData %>%
  group_by(ParkName) %>%
  summarise(AverageVisits = mean(RecreationVisits, na.rm = TRUE)) %>%
  arrange(desc(AverageVisits)) %>%
  mutate(LabelColor = ifelse(AverageVisits == max(AverageVisits), "#b30000",
                             ifelse(AverageVisits == min(AverageVisits), "#00b311", "#333344")),
         BarColor = ifelse(AverageVisits == max(AverageVisits), "#00b311",
                           ifelse(AverageVisits == min(AverageVisits), "#b30000", "#7c8aa0")))

ui <- fluidPage(
  titlePanel(HTML("RECAP (Recreational Evaluation/Comparison for American Parks)<br><span style='font-size: 10px;'>[May be a slight delay while plots initially load.]</span>")),
  fluidRow(
    column(12,
           htmlOutput("subtitlePanel")
    )
  ),
  fluidRow(
    column(12,
           plotOutput("newParkPlot", height="700px")
    )
  ),
  htmlOutput("annotation4"),
  fluidRow(
    column(12,
           plotlyOutput("DPlot", height="500px")
    )
  ),
  htmlOutput("annotation3"),
  fluidRow(
    column(12,
           plotOutput("statePlot", height="500px")
    )
  ),
  htmlOutput("annotation2"),
  fluidRow(
    column(12,
           plotOutput("parkPlot", height="700px")
    )
  ),
  htmlOutput("annotationText"),
)

server <- function(input, output) {
  font_add("Proxima Nova", "proximanova_regular.ttf")
  showtext_auto()
  output$subtitlePanel <- renderUI({
    HTML("<b><span title='truon432@umn.edu'>Will Truong</span></b> coding in R for the 2025 Love Data Week BTAA Data Viz Competition.<br>---")
  })
  output$newParkPlot <- renderPlot({
    ggplot(newParkData, aes(x = Year, y = RecreationVisits, group = ParkName, color = ColorCategory)) +
      geom_line(aes(color = ColorCategory, linewidth = LineWidth)) +
      geom_point(data = endPoints, size = 1.2) +
      geom_text_repel(data = labelData, aes(x = Year + 0.5, y = RecreationVisits, label = Label, color = ColorCategory), 
                      nudge_x = 0.5,
                      direction = "y",
                      segment.color = "#FFFFFF",
                      hjust = 0, size = 3.5) +
      scale_color_manual(values = colorPalette) +
      scale_linewidth_identity() +
      scale_y_continuous(labels = comma)+
      labs(title = "Recreation Visits vs Year, by Park",
           x = "Year",
           y = "Recreation Visits",
           color = "Legend:") +
      theme_minimal() +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5), legend.position = "bottom",
            text = element_text(family = "Proxima Nova")) +
      xlim(min(parkData$Year), max(parkData$Year) + 2.3)
  })
  output$parkPlot <- renderPlot({
    ggplot(park_avg_visits, aes(x = reorder(ParkName, AverageVisits), y = AverageVisits, fill = BarColor)) +
      geom_bar(stat = "identity")+
      scale_fill_identity() +
      coord_flip() +
      scale_y_continuous(labels = comma)+
      labs(title = "Average Recreational Visits by National Park (1979-2023)",
           subtitle = "Highlighting the most (in green) and least (in red) visited parks",
           x = NULL,
           y = "Average Visits" ) +
      theme_minimal() +
      theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
            plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
            axis.title.x = element_text(size = 12),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10, color = park_avg_visits$LabelColor, margin = margin(r = -50)),
            panel.grid.major.x = element_line(color = "#eeeeee", linewidth = 0.1),
            panel.grid.major.y = element_line(color = "#eeeeee", linewidth = 0.1),
            panel.grid.minor = element_blank(),
            text = element_text(family = "Proxima Nova")
      )
  })
  output$statePlot <- renderPlot({
    ggplot(state_avg_visits, aes(x = reorder(StateName, AverageVisits), y = AverageVisits, fill = BarColor)) +
      geom_bar(stat = "identity") +
      scale_fill_identity() +
      coord_flip() +
      scale_y_continuous(labels = comma) +
      labs(
        title = "Average National Park Visits by State (1979-2023)",
        subtitle = "Highlighting the most (in green) and least (in red) visited states",
        x = NULL,
        y = "Average Visits"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, face = "italic", hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10, color = state_avg_visits$LabelColor, margin = margin(r = -30)),
        panel.grid.major.x = element_line(color = "#eeeeee", size = 0.1),
        panel.grid.major.y = element_line(color = "#eeeeee", size = 0.1),
        panel.grid.minor = element_blank(),
        text = element_text(family = "Proxima Nova"))
  })
  output$DPlot <- renderPlotly({
    DPlot <- plot_ly(parkData, 
                     x = ~Year, 
                     y = ~Region, 
                     z = ~RecreationVisits, 
                     color = ~Region,
                     type = "scatter3d", 
                     mode = "markers",
                     marker = list(size = 4, opacity = 0.7)) %>%
      layout(
        scene = list(
          xaxis = list(title = "Year", fixedRange=TRUE),
          yaxis = list(title = "Region", fixedRange=TRUE),
          zaxis = list(title = "Recreational Visits", fixedRange=TRUE),
          camera = list(
            eye = list(x = 1, y = 2, z = 0.1)
          )
        ),
        title = "3D Scatter Plot of Recreation Visits by Year and Region"
      )
  })
  output$annotationText <- renderUI({
    HTML("<h4>Key Findings:</h4><p>1. Great Smoky Mountains is by far the most visited, with an average of ~9.77 million visits. Experts suggest this is due to  it's stunning beauty, easy accessibility, and engaging activities.<br>2. Kobuk Valley beats out the Gates of the Arctic by about 600 to be the least visited, averaging ~6,200 visits. This is just over 1/1500th of the leading park's average, presumably due to it's remote Alaskan location, with no roads, trails, or facilities.</p><h4>Miscellaneous Findings:</h4><p>1. Surprisingly, many tropical parks have been neglected, with parks like Virgin Islands, Biscayne, and Channel Islands all in the bottom 50% of visitation. A park like Channel Islands just off Los Angeles should certainly garner more attention.<br>2. In relation, hot & tropic climates seem to negatively impact visitation. Modest climates dominate the top 5 with cold, damp, and rugged terrains such as Yosemite, the Rocky Mountains, and the Great Smoky Mountains.<br>")
  })
  output$annotation2 <- renderUI({
    HTML("<h4>Key Findings:</h4><p>1. Tennessee dominates this chart, due to the prevalence of the Great Smoky Mountains park.<br>2. The American Samoa comes last on the list by ~5k, owning a singular park with just under 15k visits.</p><h4>Correlated Findings:</h4><p>1. As mentioned previously, modest/chilly climates reign over the top of the chart. This is likely due to harsh dry climates being less attractive for tourism and visitation.</p>---")
  })
  output$annotation3 <- renderUI({
    HTML("<h4>Findings:</h4><p>1. Alaskan parks have lived through permanently low visitation.</p><p>2. Midwest and Northeast parks have struggled recently, but were prominent in years past!</p><p>3. Intermountain and Pacific West parks have trended to new peak visitation.</p><p>4. Southeast parks have one outlying park with grandiose visitation, and the rest remain relatively dormant.</p>---")
  })
  output$annotation4 <- renderUI({
    HTML("<h4>Findings:</h4><br>1. Great Smoky Mountains represents the most popular park over time due to it's dominance in visitation.<br>2. Kobuk Valley's line remains stagnant in this graph, a flatline with scarce activity.<br>3. To complete the most popular ten: 2. Grand Canyon, 3. Yellowstone, 4. Zion, 5. Rocky Mountain, 6. Yosemite, 7. Acadia, 8. Grand Teton, 9. Olympic, 10. Gateway Arch.<br>---")
  })
  
}

shinyApp(ui = ui, server = server)