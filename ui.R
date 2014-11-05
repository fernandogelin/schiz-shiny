library(shiny)

# Define UI for dataset viewer application
shinyUI(fluidPage(style="margin: 10px 5% 10% 10%;",
  tagList(
    tags$head(
      tags$link(rel="stylesheet", type="text/css",href="style.css"),
      tags$script(type="text/javascript", src = "busy.js")
    )
  ),
  div(class = "busy",  
      p("Calculation in progress.."), 
      img(src="http://imageshack.us/a/img827/4092/ajaxloaderq.gif")
  ),
# Application title.
verticalLayout(
  titlePanel("Schizophrenia Exome Study"),
  
  # Sidebar with controls to select a dataset and specify the
  # number of observations to view. The helpText function is
  # also used to include clarifying text. Most notably, the
  # inclusion of a submitButton defers the rendering of output
  # until the user explicitly clicks the button (rather than
  # doing it immediately when inputs change). This is useful if
  # the computations required to render output are inordinately
  # time-consuming.
  
    # Show a summary of the dataset and an HTML table with the
    # requested number of observations. Note the use of the h4
    # function to provide an additional header above each output
    # section.
    
    mainPanel(style="width: 100%",
      div(
      span(h4("Overview"),
          p("This app reports the first results of the exome sequence analyses for the Schizophrenia project. For each family selected below, the tab pannel will be updated to show the respective results. The Summary tab contais the total, rare and shared variants. The variants tab, shows a result table with the shared variants for that family, these variants can be filterd and sorted directly in the app. The pedigree tab shows the family pedigree with the individuals that were sampled colored in blue. The venn tab shows a Venn Diagram with the shared rare variants among individuals of that family. The methods tab contains an overview of the datasets and data analyis.")),
      div(style=" margin-left: auto;margin-right: auto;width: 25%;",selectInput("dataset", "Choose a family:", 
                      choices = c("Family 30-30134","Family 30-30142","Family 30-30117","Family 30-30135","Family 30-30136","Family 31-31114","Family 31-31119","Family 32-32213","Family 44-1053","Family 45-1040","Family 49-1002","Family 53-108","Family 56-195","Family 70-1088","Family 70-1096","Family 70-1120","Family 70-1179","Family 71-5077")),
          div(submitButton("Go!"))),
      br(),
      br(),
      br()),
      
      
      
      h3(uiOutput('heading')),
      

      tabsetPanel(
           tabPanel('Summary', dataTableOutput('summary_all')),
           tabPanel('GC Axeq', dataTableOutput('InfoTableAxeq')),
           tabPanel('IC Axeq', dataTableOutput('InfoTableIndAxeq')),
           tabPanel('GC GATK', dataTableOutput('InfoTableGatk')),
           tabPanel('IC GATK', dataTableOutput('InfoTableIndGatk')),
           tabPanel('IC GATK T7', dataTableOutput('InfoTableIndGatkT7')),
           tabPanel('IC GATK FP Recal', dataTableOutput('InfoTableIndGatkFPrecal')),
           tabPanel('Family Pedigree', plotOutput("ped"), p(verbatimTextOutput('relate'))),
           tabPanel('Selected Genes', includeHTML("www/selected_variants.html")),
           tabPanel('Methods', includeHTML("www/schiz-methods.html")),
           tabPanel('Table columns IDs', includeHTML("www/vcf_headings.html"))
    )
  )
))
)