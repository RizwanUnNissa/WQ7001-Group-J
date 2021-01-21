#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


library(shinydashboard)
library(shinydashboardPlus)
library(tidyverse)
library(markdown)
library(hrbrthemes)
library(lintr)
library(extrafont)

library(readxl)
library(dplyr)
library(ggplot2)

library(shinyWidgets)




df <- readxl::read_xlsx("LE_data.xlsx", col_names = TRUE)
dft <- df %>% group_by(`Year`,`Region`) %>% summarise(`Life Expectancy` = mean(`Life Expectancy`))

lifeExp2 <- readxl::read_xlsx("LE_data_1.xlsx")


countrychoice = lifeExp2$Country

countryList <- sort(unique(df$Country))
yearList <- sort(unique(df$Year))
regionList <- sort(unique(df$Region))

# Define UI for application that draws a histogram

ui <- dashboardPage( skin = "green",
                     
                     dashboardHeader(title = "Life Expectancy at Birth" , titleWidth = "250px"),
                     dashboardSidebar( width = "250px",
                                       sidebarMenu(
                                           menuItem("About", tabName = "About", icon = icon("globe")),
                                           
                                           menuItem("Region and Country", tabName = "Region_1", icon = icon("map")),
                                           menuItem("Income Group", tabName = "Income_Group", icon = icon("money") ),
                                           menuItem("Comparison", tabName = "chart2", icon = icon("chart-line"))
                                       )
                     ),
                     dashboardBody(
                         
                         
                         
                         tabItems(
                             # Income group
                             tabItem("Income_Group",
                                     setBackgroundColor(
                                         color = "white",
                                         gradient = c("linear", "radial"),
                                         direction = c("bottom", "top", "right", "left"),
                                         shinydashboard = TRUE
                                     ),
                                     fluidRow(
                                         box(
                                             sliderInput(
                                                 inputId='yearslider',
                                                 label='Choose the Year',
                                                 min=min(df$Year, na.rm=T),
                                                 max=max(df$Year, na.rm=T),
                                                 value=min(df$Year, na.rm=T),
                                                 width = 700,
                                                 step = 1
                                             ),
                                             selectInput(
                                                 inputId='regionselect',
                                                 label='Choose Region',
                                                 choices = c("", unique(df$Region)), 
                                                 selected = "South Asia",
                                                 multiple = FALSE
                                             ) #end of input
                                         )#end of box
                                     ),# end of fluid row
                                     
                                     br(),
                                     br(),
                                     br(),
                                     fluidRow(column(6, offset= 2,
                                                     plotOutput('bar'))) # end of fluid row
                             ),# end of tab item
                             # Region and country
                             tabItem("Region_1",
                                     navbarPage("Life Expectancy at Birth Over ",
                                                tabPanel("Region",
                                                         verticalLayout(
                                                             titlePanel("Life Expectancy at Birth By Region"),
                                                             plotOutput("plot1"),
                                                             br(),
                                                             br(),
                                                             br(),
                                                             wellPanel(
                                                                 gradientBox(
                                                                     title = "Select Country",
                                                                     icon = "fa fa-th",
                                                                     width = 12,
                                                                     collapsible = FALSE,
                                                                     gradientColor = "teal", 
                                                                     boxToolSize = "sm", 
                                                                     footer = sliderInput("n", "Year", min(yearList), max(yearList),
                                                                                          value = 5, step = 1, format="####"),
                                                                     
                                                                     
                                                                 ),#end of gradient box
                                                             ) # end of well panel
                                                         ) # end of vertical layout
                                                ),# end of tab panel
                                                tabPanel("Country",
                                                         fluidRow( column(8,
                                                                          gradientBox(
                                                                              title = "Select Country",
                                                                              icon = "fa fa-th",
                                                                              collapsible = FALSE,
                                                                              gradientColor = "teal", 
                                                                              boxToolSize = "sm", 
                                                                              footer = 
                                                                                  
                                                                                  selectInput("n1", "", 
                                                                                              choices=countryList), # end of input
                                                                              
                                                                              
                                                                          ),# end of gradient box
                                                         )), # end of fluid row
                                                         
                                                         br(),
                                                         br(),
                                                         br(),
                                                         
                                                         fluidRow(
                                                             column(6,offset = 2, 
                                                                    plotOutput("plot2")
                                                             )
                                                         ) # end of fluid row  
                                                         
                                                         
                                                ) # end of tab panel
                                                
                                     ) # end of navbarPage
                                     ),# end of tab item
                             
                             # Comparison
                             
                             tabItem(tabName = "chart2", 
                                     
                                     h2(textOutput("twoCountries")),
                                     
                                     
                                     fluidRow(
                                         
                                         #select Input
                                         box(  
                                             tags$h3("Malaysia"),
                                             tags$h3("v/s"),
                                             
                                             selectInput(inputId = "countryInput2",
                                                         label = "Select Second Country:",
                                                         choices = sort(unique(lifeExp2$Country)),
                                                         selected = "China",multiple = FALSE), # end of select input
                                         )# end of box
                                         
                                     ), # end of fluid row
                                     
                                     
                                     br(),
                                     br(),
                                     
                                     fluidRow(
                                         column(width = 6,offset = 2,
                                                plotOutput("scatterPlot1"))
                                     ) #end of fluidRow
                             ), #end of tab Item 
                             
                             # About the app
                             tabItem("About",
                                     tags$head(
                                         tags$style("h1{font-family:Calibri; font-size: 32px}"),
                                         tags$style("body{font-size:18px")
                                     ),
                                     tags$h1("Life Expectancy At Birth"),
                                     tags$body(" Life expectancy at birth is defined as the average number of years that a newborn could expect to live if he or she were to pass through life subject to the age-specific mortality rates of a given period.It reflects the overall mortality level of a population. It summarizes the mortality pattern that prevails across all age groups - children and adolescents, adults and the elderly."),
                                     br(),
                                     tags$body("Furthermore, happiness index of any country is directly proportional to its life expectancy at birth. Life expectancy at birth of a country gives a fair idea about its citizen satisfaction level."),
                                     br(),br(),
                                     tags$h1("Objective"),
                                     
                                     tags$body("Human beings have always strived to improve quality of life. Advancements in technology have further accelerated this process. Motivated by this spirit we intend to give people an interactive interface through the medium of this app to gain insight into variations in life expectancy at birth across the globe that have taken place over decades. It provides comparative view of life expectancy at birth not only across different countries but also across different regions and even different income groups."),
                                     br(),br(),br(),
                                     tags$h1("Dataset"),
                                     tags$body("The dataset used was downloaded from data.world. For further information you may click on the link below."),br(),
                                     tags$a(href = "https://data.world/makeovermonday/life-expectancy-at-birth-by-country", "Dataset"),
                                     
                                     br(),br(),br(),
                                     tags$h1("Tab Info"),
                                     tags$body("Following is the information about app tabs to help user in navigation."),
                                     br(),br(),br(),
                                     fluidRow(
                                         boxPlus(title = "Income group",
                                                 closable = FALSE, width =4,height = 150, background =  "teal",solidHeader = T,
                                                 p("This tab of the app lets user experience differences in life expectancy at birth of a country based on the income group of the family a child is born in.")),
                                         boxPlus(title = "Region", closable = FALSE, width =4, height = 150,background =  "purple",
                                                 p("This tab of the app lets user experience differences in life expectancy at birth of various regions of earth and also of a country from 1960 to 2015")),
                                         boxPlus(title = "Time-Series", closable = FALSE, width =4, height=150,background =  "yellow",
                                                 p("This tab of the app is for comparison of life expectancy at birth in Malysia with that of the other countries")),
                                     )# end of fluid row
                                     
                                     
                             ) # end of tab item
                             
                             
                         ) # end of tab items
                     ) # end of dashboard body
) # end of dashboard page




server <- function(input, output,session) {
    
    # Income Group
    
    output$bar <- renderPlot({
        pclass_year_data <- subset(df, Year %in% input$yearslider)
        pclass_year_data2 <- subset(pclass_year_data, Region %in% input$regionselect) %>%
            select(`Income Group`, `Life Expectancy`) %>%
            group_by(`Income Group`) %>%
            summarise(LifeExpectancy = round(mean(`Life Expectancy`, na.rm=TRUE), 2))
        
        ggplot(data=pclass_year_data2, aes(x=`Income Group`, y=LifeExpectancy, fill = `Income Group`)) +
            geom_bar(stat='identity', width = 0.5) +
            
            geom_text(aes(label=LifeExpectancy,color = `Income Group`), vjust=-0.3, size=5) +
            ggtitle('Average Life Expectancy at Birth for different Income Groups') +
            labs(x="Income Group",y="Life Expectancy") + scale_fill_manual(values=c("High income" = "#ffb3ba", "Upper middle income" = "#baffc9", "Lower middle income" = "#bae1ff", "Low income" = "#ffffba"))+
            
            theme(axis.text=element_text(size=12),axis.title.x = element_text(size = 16), axis.title.y = element_text(size = 16),plot.title=element_text(size=32), panel.background=element_blank())
    }, height = 500, width = 1000 )
    
    # Region and Country
    output$plot1 <- renderPlot({
        select_year <- filter(dft, `Year` == input$n)
        ggplot(select_year, aes(x=factor(`Region`), y=`Life Expectancy`, fill=`Region`)) +
            geom_bar(stat="identity")+theme_minimal()+coord_flip()+
            geom_text(aes(label=sprintf("%0.2f", round(`Life Expectancy`, digits = 2))), position=position_dodge(width=0.9), vjust=-0.25) + labs(x="Region") + theme(axis.text= element_text(size = 14),axis.title.x = element_text(size = 18, hjust = 0.5, vjust = 3), axis.title.y = element_text(size = 18, hjust = 0.5, vjust = 3))
    })
    
    output$plot2 <- renderPlot({
        select_country <- filter(df, `Country` == input$n1)
        ggplot(data=select_country, aes(x=`Year`, y=`Life Expectancy`)) +
            geom_line( color="blue") +
            geom_point(shape=21, color="black", fill="#69b3a2", size=2) + labs(x="Life Expectancy", y = "Country") + theme_ipsum() + 
            theme(axis.title.x = element_text(size = 18, hjust = 0.5, vjust = 3), axis.title.y = element_text(size = 18, hjust = 0.5, vjust = 3))
    }, height = 500, width = 1000)
    output$twoCountries <- renderText({ 
        paste("Comparison of Life Expectancy at birth between Malaysia and other countries")
    }) #end of renderText
    
    # Comparison
    output$scatterPlot1 <- renderPlot({
        lifeExp2 %>% 
            filter(Country == "Malaysia" | Country == input$countryInput2) %>% 
            ggplot(aes(y = LifeExpectancy , x = Year, color = Country)) +
            geom_line() + theme_classic() + geom_point(alpha = 0.5, size = 1.5)+
            coord_cartesian(ylim = c(min(lifeExp2$LifeExpectancy), max(lifeExp2$LifeExpectancy)), 
                            xlim = c(min(lifeExp2$Year), max(lifeExp2$Year))) +
            labs(y = "Life Expectancy", x = "Year") + 
            scale_y_continuous(breaks=seq(0, 100, 10)) + 
            scale_x_continuous(breaks=seq(1800, 2020, 10)) +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            ggtitle("Malaysia v/s Other Countries") + 
            theme(axis.text=element_text(size=14), plot.title = element_text(hjust = 0.5, size = 20),axis.title.x = element_text(size = 18, hjust = 0.5, vjust = 3), axis.title.y = element_text(size = 18, hjust = 0.5, vjust = 3))
    }, height = 500, width = 1000) 
    
    
    
    
}

shinyApp(ui, server)