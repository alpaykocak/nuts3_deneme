library(shiny)
library(tidyverse)
library(gapminder)
library(treemapify)
library(readxl)
library(readxl)
pivot <- read_excel("/Users/necmettinalpaykocak/Documents/shiny_trials/nuts3_deneme/pivot.xls",skip = 1)
pivot <- pivot[,3:6]
colnames(pivot) <- c("tip", "il","yıl","veri")
i1 <- !is.na(unique(pivot$tip))
tip <- unique(pivot$tip)[i1]
i2 <- !is.na(unique(pivot$il))
il <- unique(pivot$il)[i2]
i3 <- !is.na(unique(pivot$yıl))
yıl <- unique(pivot$yıl)[i3]
data <- data.frame("tip" = as.factor(rep(tip, each = length(il)*length(yıl))),
                   "il" = as.factor(rep(il, each = length(yıl), times = length(tip))),
                   "yıl" = as.numeric(rep(yıl, times = length(tip)*length(il))),
                   "veri" = as.numeric(pivot$veri)
)
library(tidyr)
data <- separate(data,col = il,into = c("il_adi","il_kod"),sep = "-")
data$il_adi <- as.factor(data$il_adi)
data$il_kod <- as.factor(data$il_kod)
il_sinif <- read_excel("/Users/necmettinalpaykocak/Documents/shiny_trials/nuts3_deneme/il_sinif.xls")
datam <- merge(data,il_sinif,by = "il_kod",all.x = TRUE)
datam[,6:9] <- lapply(datam[,6:9], as.factor)

ui <-fluidPage(
    # Give the page a title
    titlePanel("Türkiye NUTS-3 Bazında Göstergeler için Gösterim Çalışması"),
    
    # Generate a row with a sidebar
    sidebarLayout(      
        
        # Define the sidebar with one input
        sidebarPanel(
            selectInput("continent", "NUTS-1:", 
                        choices = unique(datam$`NUTS-1`)),
            hr(),
            selectInput("indicator", "Gösterge:", 
                        choices = unique(datam$tip)),
            helpText("il bazında GSYH Verisi")
        ),
        
        # Create a spot for the rectplot
        mainPanel(
            plotOutput("rectPlot")  
        )
        
    ),
    fluidRow(
        shiny::column(4, offset = 4,
                      sliderInput("year", "Yıl",
                                  min = min(datam$yıl), max = max(datam$yıl),
                                  value = min(datam$yıl), animate = TRUE,
                                  step = 1,sep = "")
        )
    )
)
server <- function(input, output) {
    n <- reactive({
        data <- datam %>% filter(`NUTS-1` == input$continent & yıl == input$year & tip == input$indicator) %>%
            select(`NUTS-3`, `NUTS-1`, yıl, veri,`NUTS-2`)
    })
    name <- reactive({
        paste0("Yıllar itibariyla ", input$indicator)
    })
    output$rectPlot <- renderPlot({
        verim <- as.data.frame(n())
        ggplot(verim, aes(area = verim[,4],fill = verim[,5], label =verim[,1])) +
            geom_treemap(show.legend = F) +
            geom_treemap_text() +
            labs(title = name(),
                 caption  = "Veri kaynağı: TÜİK") 
    })
}
# Run the application 
shinyApp(ui = ui, server = server)