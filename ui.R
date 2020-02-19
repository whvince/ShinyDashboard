
dashboardPage(
  
  dashboardHeader(title = 'Beer Industry Study'), 
  
  
  dashboardSidebar(
    
    sidebarUserPanel('', image = "https://image.shutterstock.com/image-photo/beer-bar-free-space-your-260nw-1094023034.jpg"),
  
  sidebarMenu(
    menuItem("Industry", tabName = "Industry", icon = icon("map")), 
    menuItem("Category", tabName = "Category", icon = icon("table")),
    menuItem("Light vs. Heavy", tabName = "LightorHeavy", icon = icon("chart-line"))
    ),
  
    selectizeInput(inputId = 'variableSelected', 
                   label = 'Select Variable', 
                   choices = names(rating_brewery_state)[-1]),
   
    selectizeInput(inputId = 'stateSelected',
                   label = 'Select State',
                   choices = sort(unique(rating_brewery_state$state))),
  
    # selectizeInput(inputId = 'categorySelected',
    #                label = 'Select Category',
    #                choices = sort(unique(beer_category$style_category))),
    # 
    checkboxGroupInput(inputId = 'categorySelected',
                label = "Select Beer Category",
                choices = sort(unique(beer_category$style_category)),
                selected = sort(unique(beer_category$style_category)))
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "Industry", 
              
              fluidRow(box(htmlOutput("map"), width = "100%", title = 'Rating (out of 5) and Brewery Concentration by State')),
              fluidRow(box(plotlyOutput("style_concentration"), width = "100%"))
 
              ),
      
      tabItem(tabName = "Category", 
              fluidRow(box(dataTableOutput("table"), width = "100%"))
              ),
      
      tabItem(tabName = "LightorHeavy", 
              fluidRow(box(plotlyOutput("lightHeavy"), width = "100%")), 
              fluidRow(box(plotlyOutput("abvRating"), width = "100%"))
      )
      
    )
  )
)
