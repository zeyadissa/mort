map_ui <- function(){
  htmltools::tagList(
    sidebarLayout(
      sidebarPanel(
        titlePanel(h4(strong('MAP MAKER'),icon('adjust',class='about-icon fa-pull-left'))),
        em('This allows you to create simple maps based on ICB boundaries, with the option to show Ward, LAD, LSOA and (later) Place boundaries for each ICB. Simply select the ICBs you want and click the boundary layer button on the map to select what you want viewed. You can also edit the colour in the sidebar panel on the left. The intention is that this can be used to quickly and easily get every possible health boundary permutation that could be needed and be plopped up on a slide or report'),
        hr(),
        multiInput(
          inputId='ICB_SELECT',
          label = 'Select which ICB(s) you want shown',
          choices = icb$ICB23NM,
          selected="",
          width = '120%'),
        hr(),
        shiny::actionButton(
          inputId='FIT_BOUNDS_PRESET',
          label = 'Fit to bounds',
          icon = icon('compass'),
          status = 'primary',
          width = '100%'),
        hr(),
        colourpicker::colourInput(
          'ICB_COLOUR',
          'Choose fill colour',
          '#26a5b8')),
      mainPanel(leafletOutput('main_map_preset',height='85vh',width='100%')))
  )
}