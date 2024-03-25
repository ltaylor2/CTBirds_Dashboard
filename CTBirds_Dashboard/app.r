library(tidyverse)
library(sf)
library(shiny)

# Runtime ---------------------------
taxonomy <- readRDS("taxonomy.RDS")
taxonomyDict <- taxonomy$Taxon_Scientific
names(taxonomyDict) <- taxonomy$Taxon_Common
proportions <- readRDS("species_observations_proportions.RDS")
locations <- readRDS("species_observations_locations.RDS")
maxWeeks <- readRDS("species_observations_maxWeeks.RDS")
changeModels <- readRDS("species_observations_changeModels.RDS")
ctMap <- readRDS("ct_shapefile.RDS")

# ui ---------------------------
ui <- fluidPage(
   titlePanel(h1("CT Birds Dashboard", align="center"), windowTitle="CT Birds Dashboard"),
   h3(textOutput("titleCommon"), em(textOutput("titleScientific")), align="center"), 
   fluidRow(column(width=6, offset=0, plotOutput("timeseries")), column(width=6, offset=0, plotOutput("map"))),
   fluidRow(column(width=3, offset=1, selectizeInput("common", label="Species", choices=names(taxonomyDict), selected="Tree Swallow"))),
   fluidRow(column(width=9, offset=1, helpText(strong("Data from 548,593 complete eBird checklists in Connecticut, 2015-2023 only.")))),
   fluidRow(column(width=9, offset=1, helpText(strong("Left (time series):"), 
                                              p("For each week of 2024, boxplots summarize the percentage of eBird checklists in which the selected species was observed during previous years."),
                                              p("Black line shows the general trend by smoothing across all points."),
                                              p("Upper and lower hinges of each box indicate 25th and 75th percentiles. Whiskers extend to the smallest/largest values within 1.5x the height of the box. Points show outliers. Trend line smoothed with LOESS (span = 0.1)."),
                                              p("For example: On the 13th week of 2015, 55 of 860 checklists (6.4%) featured Tree Swallows. On the 13th week of 2020, 121 of 1161 checklists (10.4%) featured Tree Swallows. The 13th week of 2024 begins on March 25.")))),
   fluidRow(column(width=9, offset=1, helpText(strong("Right (map):"), 
                                               p("Points show the location of checklists featuring the species."),
                                               p("Brighter colors highlight regions where the greatest numbers of individual birds for that species have been observed in any one checklist."),
                                               p("For example: a bright red hexagon could indicate ~1000 Tree Swallows seen in one checklist in the area, whereas a faded hexagon indicates a maximum record of only 10 Tree Swallows.
                                                  Colors are relative for each species; red for Tree Swallows could indicate hundreds of birds, whereas red for Kentucky Warblers could indicate a single bird."))))
)

# server ---------------------------
server <- function(input, output, session) {

    output$titleCommon <- renderText({
      input$common
    })
    
    output$titleScientific <- renderText({
      if (input$common=="") {
        ""
      } else {
        taxonomyDict[[input$common]]
      }
    })
    
    output$modeledChange <- renderText({
      paste0("CT population: ", changeModels[changeModels$Taxon_Scientific==taxonomyDict[[input$common]],"Change"],
            " (",changeModels[changeModels$Taxon_Scientific==taxonomyDict[[input$common]],"Significance"],")")
    })
    
    subsetProportions <- reactive({
        req(input$common)
        filter(proportions, Taxon_Scientific==taxonomyDict[[input$common]])
    })

    subsetLocations <- reactive({
        req(input$common)
        filter(locations, Taxon_Scientific==taxonomyDict[[input$common]])
    })

    output$timeseries <- renderPlot({
        df <- subsetProportions()
        taxon_scientific <- df$Taxon_Scientific[1]
        taxon_common <- names(taxonomyDict)[taxonomyDict==taxon_scientific]
        
        ggplot(df, aes(x=Week_of_2024, y=Percentage_Checklists_Observed)) +
            geom_vline(xintercept=maxWeeks[taxon_scientific], colour="red", alpha=0.15, linewidth=2) +
            geom_boxplot(aes(group=Week_of_2024), colour="darkgray", fill="transparent", alpha=0.3, outlier.size=0.5) +
            geom_smooth(formula="y~x", method="loess", span=0.15, colour="black", se=FALSE, linewidth=0.8) +
            scale_x_date(date_breaks="1 month",
                        date_labels="%b") +
            scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, by=10),
                            labels=~paste0(.,"%")) +
            xlab("Predicted 2024") +
            ylab("Species observed (2015-2023)") +
            theme_bw() +
            theme(axis.text.x=element_text(size=10),
                  panel.grid.major=element_line(color=alpha("#f0f0f0", 0.5)),
                  panel.grid.minor.x=element_blank(),
                  panel.grid.minor.y=element_blank())
    }, res=96) 

    output$map <- renderPlot({
        df <- subsetLocations()

        ggplot(df) +
            geom_hex(aes(x=Longitude, y=Latitude, weight=Log10_Max_Observed), alpha=0.80, colour="transparent", binwidth=c(0.08, 0.06)) +
            geom_point(aes(x=Longitude, y=Latitude), size=0.1, colour="black") +
            geom_sf(data=ctMap, colour="black", fill="transparent", linewidth=1.3) +
            scale_fill_gradient(low="transparent", high="red") +
            guides(fill="none") +
            theme_void()
    }, res=96) 
}
shinyApp(ui, server)
