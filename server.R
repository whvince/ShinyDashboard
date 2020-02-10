
function(input, output){ 
  output$map <- renderGvis({
    
    gvisGeoChart(filter(rating_brewery_state, !is.na(state)),
                        'state',
                        input$variableSelected, 
                        options = list(region = "US", displayMode="regions",
                                       resolution="provinces", width="auto", height="auto",
                                       title='Rating by State and Brewery Concentration'))
    
  })
  

  
  output$style_concentration <- renderPlotly({
    
    concentration_plot <- inner_join(style_concentration, style_rating, by = "style") %>% 
      filter(style_category %in% input$categorySelected) %>%
      ggplot(aes(x = num_breweries, y = score_avg, text = style)) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      coord_cartesian(ylim = c(2.0, 4.5)) + 
      geom_point(aes(color = style_category)) + 
      labs(title='Beer Style Concentration Analysis',
           x='Number of Breweries',
           y='Average Score') 
    
      ggplotly(concentration_plot, tooltip = c("style"))
      
  })
  

  output$table <- renderDataTable({
    datatable(filter(complete_beer_info, style_category == input$categorySelected, state == input$stateSelected) %>% 
                    select(-style_category, -state_country_abbr, -state, -brewery_id) %>% arrange(desc(score))
                  , rownames = FALSE, filter = "top") 
  })
  
  
  output$lightHeavy <- renderPlotly({
    
    lightHeavy_plot <- ggplot(ranking_lightHeavy, aes(x = year, y = rank)) + 
      scale_y_continuous(breaks=seq(1,14, by = 1), trans = "reverse")+
      scale_x_continuous(breaks=seq(2000, 2018, by = 1))+
      geom_line(aes(color = beer_name)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title='For Pale Lager and Pilsner, Heavy is better',
           x='Year',
           y='Rank')
    
    ggplotly(lightHeavy_plot, tooltip = c('beer_name'))
    
  })
  
  output$abvRating <- renderPlotly({
    
    abvRating_plot <- complete_beer_info %>% 
      filter(style_category %in% input$categorySelected) %>%
      ggplot(aes(x = abv, y = score)) +
      geom_point(aes(color = style_category)) + 
      geom_smooth() +
      theme(plot.title = element_text(hjust = 0.5)) +
      coord_cartesian(ylim = c(1.0, 5.0)) + 
      labs(title='Alcohol % vs. Rating',
           x='Alcohol %',
           y='Rating')
    
    ggplotly(abvRating_plot)
    
  })
  
  
}