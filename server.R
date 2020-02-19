
function(input, output){ 
  output$map <- renderGvis({
    
    gvisGeoChart(filter(rating_brewery_state, !is.na(state)),
                        'state',
                        input$variableSelected, 
                        options = list(region = "US", displayMode="regions",
                                       resolution="provinces", width="auto", height="auto"
                                       ))
    
  })
  
  
  output$style_concentration <- renderPlotly({
    
    concentration_plot <- inner_join(style_concentration, style_rating, by = "style") %>% 
      filter(style_category %in% input$categorySelected) %>%
      ggplot(aes(x = num_breweries, y = score_avg, text = style)) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      scale_y_continuous(limits = c(2.0, 4.5)) + 
      scale_x_continuous(limits = c(0, 6500)) + 
      geom_point(aes(color = style_category)) + 
      geom_hline(yintercept=3.23, linetype="dashed", color = "red") +
      geom_vline(xintercept=3000, linetype="dashed", color = "red") +
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
    
    lightHeavy_plot <-  ggplot(rating_lightHeavy, aes(x=year, y = average_rating)) + 
      geom_line(aes(color = HeavyLight)) +
      theme(plot.title = element_text(hjust = 0.5)) +
      labs(title='For Pale Lager and Pilsner, heavy beer is consistently rated higher than light.',
           x='Year',
           y='Rating')
    
    
    ggplotly(lightHeavy_plot, tooltip = c('HeavyLight'))
    
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