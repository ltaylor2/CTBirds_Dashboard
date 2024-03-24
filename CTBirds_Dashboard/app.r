library(tidyverse)
library(shiny)

# Runtime ---------------------------
taxonomy <- read_csv("taxonomy.csv", show_col_types=FALSE)
taxonomyDict <- taxonomy$Taxon_Scientific
names(taxonomyDict) <- taxonomy$Taxon_Common
obs <- read_csv("ebd_ct_cleaned_observation_proportions.csv", show_col_types=FALSE)

# ui ---------------------------
ui <- fluidPage(
   fluidRow(column(width=11, offset=0, plotOutput("plot"))),
   fluidRow(column(width=3, offset=1, selectizeInput("common", label="Species", choices=names(taxonomyDict), selected="Tree Swallow"))),
   fluidRow(column(width=10, offset=1, helpText("For each week of 2024, boxplots summarize the percentage of eBird checklists in which the selected species was observed during previous years. The black line shows the general trend by smoothing across all points."))),
   fluidRow(column(width=10, offset=1, helpText("For example: On the 13th week of 2015, 55 of 860 checklists (6.4%) featured Tree Swallows. On the 13th week of 2020, 121 of 1161 checklists (10.4%) featured Tree Swallows. The 13th week of 2024 begins on March 25."))),
   fluidRow(column(width=10, offset=1, helpText("Upper and lower hinges of each box indicate 25th and 75th percentiles, summarizing across 9 years of eBird data for each week (2015-2023). Whiskers for each box extend to the smallest/largest values within 1.5x the height of the box. Points show outliers. Trend line smoothed with LOESS (span = 0.1)."))),
   fluidRow(column(width=10, offset=1, helpText("Data from 548,593 complete eBird checklists in Connecticut.")))
)

# server ---------------------------
server <- function(input, output, session) {
    subsetted <- reactive({
        req(input$common)
        filter(obs, Taxon_Scientific==taxonomyDict[[input$common]])
    })

    output$plot <- renderPlot({
        df <- subsetted()
        taxon_scientific <- df$Taxon_Scientific[1]
        taxon_common <- names(taxonomyDict)[taxonomyDict==taxon_scientific]
        
        ggplot(df, aes(x=Week_of_2024, y=Percentage_Checklists_Observed)) +
         geom_boxplot(aes(group=Week_of_2024), colour="darkgray", fill="transparent", alpha=0.3, outlier.size=0.5) +
         geom_smooth(formula="y~x", method="loess", span=0.15, colour="black", se=FALSE, linewidth=0.8) +
         scale_x_date(date_breaks="1 month",
                      date_labels="%b") +
         scale_y_continuous(limits=c(0, 100), breaks=seq(0, 100, by=10),
                            labels=~paste0(.,"%")) +
         xlab("Predicted 2024") +
         ylab("CT checklists with species observed (2015-2023)") +
         ggtitle(bquote(.(taxon_common)~"("*italic(.(taxon_scientific))*")")) +
         theme_bw() +
         theme(axis.text.x=element_text(size=10),
               panel.grid.major=element_line(color=alpha("#f0f0f0", 0.5)),
               panel.grid.minor.x=element_blank(),
               panel.grid.minor.y=element_blank())
    }, res=100) 
}
shinyApp(ui, server)
