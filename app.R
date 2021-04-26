ui <- fluidPage(
        #header
        fluidRow()
        #tabs
        fluidRow()
        #text blurb
        fluidRow()
        #user input (select and submit button)
        fluidRow(
          selectInput()
          submitButton()
        )
        #park information
        fluidRow()
        #animals heading + ranking
        fluidRow()
        #vertebrates heading + ranking
        fluidRow()
        ##vertebrates plot
        fluidRow()
        #invertebrates heading + ranking
        fluidRow()
        ##invertebrates plot + descriptions
        fluidRow()
        #plants heading + ranking
        fluidRow()
        ##plants plot + definitions
        fluidRow() 
        #algae/fungi heading + ranking
        fluidRow()
        #algae/fungi plot
        fluidRow()
        #disclaimer
        fluidRow()
)