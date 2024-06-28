faq_ui <- function(){
  htmltools::tagList(
    fluidRow(
      titlePanel(h4(strong('Frequently Asked Questions (FAQs)'))),
      hr(),
      h5(strong('What can this application do?')),
      h5('You can use this to look-up national NHS health and admistrative boundaries, create pre-set maps of those boundaries, and create custom maps using little information. Basically, if you want a map, use this tool. There are future plans to include catchment area finders using a weighted Voronoi method, as well as some other neat stuff. But right now there is not much free development time'),
      hr(),
      h5(strong('It wont let me submit a map!')),
      h5('There are often three problems. First is you may be using an incorrect geography. Check that the geography codes are all supporter (See supported geography codes in the Geography Lookups page). The second is that the data types do not match. So for example, if you are trying to plot names of ICBs and you select a variable type to be Numeric or Categorical, it will naturally throw an error. Finally you could simply be uploading a file in the wrong format. Currently this only accepts .csv files'),
      hr(),
      h5(strong('I am having issues uploading markers')),
      h5('Note that there are certain templates marker date must be inputted. The first column needs to be the marker longitude, and second is latitude. The third, optional column, needs to be the value or name associated with the marker'),
      hr(),
      h5(strong('I have a problem, who do I contact?')),
      h5('Feel free to contact me at zeyadissa@gmail.com'),
      hr(),
      h5(strong('I want a new feature or geography added')),
      h5('See above! More than happy to sit down and discuss future additions'),
      style = 'align: centre;padding-left:10px')
  )
}