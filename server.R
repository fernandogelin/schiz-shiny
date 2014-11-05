library(shiny)
library(shinyIncubator)
library(ggplot2)
library(RJSONIO)
library(data.table)

summary_all <- read.csv("data/summary_table.csv", header=TRUE, na.strings=NA)

source('data/schizPedigree.R')

load('data/tables.RData')


shinyServer(function(input, output) {
  datasetInputInfoAxeq <- reactive({
    switch(input$dataset,
           "Family 30-30134" = id_30134_axeq,
           "Family 30-30142" = id_30142_axeq,
           "Family 30-30117" = id_30117_axeq,
           "Family 30-30135" = id_30135_axeq,
           "Family 30-30136" = id_30136_axeq,
           "Family 31-31114" = id_31114_axeq,
           "Family 31-31119" = id_31119_axeq,
           "Family 32-32213" = id_32213_axeq,
           "Family 44-1053" = id_441053_axeq,
           "Family 45-1040" = id_451040_axeq,
           "Family 49-1002" = id_491002_axeq,
           "Family 53-108" = id_53108_axeq,
           "Family 56-195" = id_56195_axeq,
           "Family 70-1088" = id_701088_axeq,
           "Family 70-1096" = id_701096_axeq,
           "Family 70-1120" = id_701120_axeq,
           "Family 70-1179" = id_701179_axeq,
           "Family 71-5077" = id_715077_axeq
    )
  })
       datasetInputInfoGatk <- reactive({
            switch(input$dataset,
                   "Family 30-30134" = id_30134_gatk,
                   "Family 30-30142" = id_30142_gatk,
                   "Family 30-30117" = id_30117_gatk,
                   "Family 30-30135" = id_30135_gatk,
                   "Family 30-30136" = id_30136_gatk,
                   "Family 31-31114" = id_31114_gatk,
                   "Family 31-31119" = id_31119_gatk,
                   "Family 32-32213" = id_32213_gatk,
                   "Family 44-1053" = id_441053_gatk,
                   "Family 45-1040" = id_451040_gatk,
                   "Family 49-1002" = id_491002_gatk,
                   "Family 53-108" = id_53108_gatk,
                   "Family 56-195" = id_56195_gatk,
                   "Family 70-1088" = id_701088_gatk,
                   "Family 70-1096" = id_701096_gatk,
                   "Family 70-1120" = id_701120_gatk,
                   "Family 70-1179" = id_701179_gatk,
                   "Family 71-5077" = id_715077_gatk
            )
       })
  datasetInputInfoIndGatk <- reactive({
       switch(input$dataset,
              "Family 30-30134" = id_30134_ind_gatk,
              "Family 30-30142" = id_30142_ind_gatk,
              "Family 30-30117" = id_30117_ind_gatk,
              "Family 30-30135" = id_30135_ind_gatk,
              "Family 30-30136" = id_30136_ind_gatk,
              "Family 31-31114" = id_31114_ind_gatk,
              "Family 31-31119" = id_31119_ind_gatk,
              "Family 32-32213" = id_32213_ind_gatk,
              "Family 44-1053" = id_441053_ind_gatk,
              "Family 45-1040" = id_451040_ind_gatk,
              "Family 49-1002" = id_491002_ind_gatk,
              "Family 53-108" = id_53108_ind_gatk,
              "Family 56-195" = id_56195_ind_gatk,
              "Family 70-1088" = id_701088_ind_gatk,
              "Family 70-1096" = id_701096_ind_gatk,
              "Family 70-1120" = id_701120_ind_gatk,
              "Family 70-1179" = id_701179_ind_gatk,
              "Family 71-5077" = id_715077_ind_gatk
       )
  })
  datasetInputInfoIndAxeq <- reactive({
       switch(input$dataset,
              "Family 30-30134" = id_30134_ind_axeq,
              "Family 30-30142" = id_30142_ind_axeq,
              "Family 30-30117" = id_30117_ind_axeq,
              "Family 30-30135" = id_30135_ind_axeq,
              "Family 30-30136" = id_30136_ind_axeq,
              "Family 31-31114" = id_31114_ind_axeq,
              "Family 31-31119" = id_31119_ind_axeq,
              "Family 32-32213" = id_32213_ind_axeq,
              "Family 44-1053" = id_441053_ind_axeq,
              "Family 45-1040" = id_451040_ind_axeq,
              "Family 49-1002" = id_491002_ind_axeq,
              "Family 53-108" = id_53108_ind_axeq,
              "Family 56-195" = id_56195_ind_axeq,
              "Family 70-1088" = id_701088_ind_axeq,
              "Family 70-1096" = id_701096_ind_axeq,
              "Family 70-1120" = id_701120_ind_axeq,
              "Family 70-1179" = id_701179_ind_axeq,
              "Family 71-5077" = id_715077_ind_axeq
       )
  })
  datasetInputInfoIndGatkT7 <- reactive({
       switch(input$dataset,
              "Family 30-30134" = id_30134_gatk_t7,
              "Family 30-30142" = id_30142_gatk_t7,
              "Family 30-30117" = id_30117_gatk_t7,
              "Family 30-30135" = id_30135_gatk_t7,
              "Family 30-30136" = id_30136_gatk_t7,
              "Family 31-31114" = id_31114_gatk_t7,
              "Family 31-31119" = id_31119_gatk_t7,
              "Family 32-32213" = id_32213_gatk_t7,
              "Family 44-1053" = id_441053_gatk_t7,
              "Family 45-1040" = id_451040_gatk_t7,
              "Family 49-1002" = id_491002_gatk_t7,
              "Family 53-108" = id_53108_gatk_t7,
              "Family 56-195" = id_56195_gatk_t7,
              "Family 70-1088" = id_701088_gatk_t7,
              "Family 70-1096" = id_701096_gatk_t7,
              "Family 70-1120" = id_701120_gatk_t7,
              "Family 70-1179" = id_701179_gatk_t7,
              "Family 71-5077" = id_715077_gatk_t7
       )
  }) 
  datasetInputInfoIndGatkFPrecal <- reactive({
       switch(input$dataset,
              "Family 30-30134" = id_30134_gatk_fp_recal,
              "Family 30-30142" = id_30142_gatk_fp_recal,
              "Family 30-30117" = id_30117_gatk_fp_recal,
              "Family 30-30135" = id_30135_gatk_fp_recal,
              "Family 30-30136" = id_30136_gatk_fp_recal,
              "Family 31-31114" = id_31114_gatk_fp_recal,
              "Family 31-31119" = id_31119_gatk_fp_recal,
              "Family 32-32213" = id_32213_gatk_fp_recal,
              "Family 44-1053" = id_441053_gatk_fp_recal,
              "Family 45-1040" = id_451040_gatk_fp_recal,
              "Family 49-1002" = id_491002_gatk_fp_recal,
              "Family 53-108" = id_53108_gatk_fp_recal,
              "Family 56-195" = id_56195_gatk_fp_recal,
              "Family 70-1088" = id_701088_gatk_fp_recal,
              "Family 70-1096" = id_701096_gatk_fp_recal,
              "Family 70-1120" = id_701120_gatk_fp_recal,
              "Family 70-1179" = id_701179_gatk_fp_recal,
              "Family 71-5077" = id_715077_gatk_fp_recal
       )
  })
  
  datasetHeading <- reactive({
    switch(input$dataset,
           "Family 30-30134" = "Family 30-30134 - European American",
           "Family 30-30142" = "Family 30-30142 - European American",
           "Family 30-30117" = "Family 30-30117 - African American",
           "Family 30-30135" = "Family 30-30135 - African American",
           "Family 30-30136" = "Family 30-30136 - African American",
           "Family 31-31114" = "Family 31-31114 - European American",
           "Family 31-31119" = "Family 31-31119 - European American",
           "Family 32-32213" = "Family 32-32213 - European American",
           "Family 44-1053" = "Family 44-1053 - African American",
           "Family 45-1040" = "Family 45-1040 - European American",
           "Family 49-1002" = "Family 49-1002 - African American",
           "Family 53-108" = "Family 53-108 - Hispanic",
           "Family 56-195" = "Family 56-195 - Hispanic",
           "Family 70-1088" = "Family 70-1088 - European American",
           "Family 70-1096" = "Family 70-1096 - European American",
           "Family 70-1120" = "Family 70-1120 - European American",
           "Family 70-1179" = "Family 70-1179 - European American",
           "Family 71-5077" = "Family 71-5077 - European American"
    )
  })
  
  datasetInputRel <- reactive({
    switch(input$dataset,
           "Family 30-30134" = relate30134,
           "Family 30-30142" = relate30142,
           "Family 30-30117" = relate30117,
           "Family 30-30135" = relate30135,
           "Family 30-30136" = relate30136,
           "Family 31-31114" = relate31114,
           "Family 31-31119" = relate31119,
           "Family 32-32213" = relate32213,
           "Family 44-1053" = relate441053,
           "Family 45-1040" = relate451040,
           "Family 49-1002" = relate491002,
           "Family 53-108" = relate53108,
           "Family 56-195" = relate56195,
           "Family 70-1088" = relate701088,
           "Family 70-1096" = relate701096,
           "Family 70-1120" = relate701120,
           "Family 70-1179" = relate701179,
           "Family 71-5077" = relate715077
    )
  })
  plotInput <- reactive({
	  switch(input$dataset,
		  "Family 30-30134" = plot(venn.id_30134),
		  "Family 30-30142" = plot(venn.id_30142),
		  "Family 30-30117" = plot(venn.id_30117),
		  "Family 30-30135" = plot(venn.id_30135),
		  "Family 30-30136" = plot(venn.id_30136),
		  "Family 31-31114" = plot(venn.id_31114),
		  "Family 31-31119" = plot(venn.id_31119),
		  "Family 32-32213" = plot(venn.id_32213),
		  "Family 44-1053" = plot(venn.id_441053),
		  "Family 45-1040" = plot(venn.id_451040),
		  "Family 49-1002" = plot(venn.id_491002),
		  "Family 53-108" = plot(venn.id_53108),
		  "Family 56-195" = plot(venn.id_56195),
		  "Family 70-1088" = plot(venn.id_701088),
		  "Family 70-1096" = plot(venn.id_701096),
		  "Family 70-1120" = plot(venn.id_701120),
		  "Family 70-1179" = plot(venn.id_701179),
		  "Family 71-5077" = plot(venn.id_715077)
		  )
  })
  pedigreeInput <- reactive({
	  switch(input$dataset,
		  "Family 30-30134" = plot(ped30134, col=ifelse(fam30134$avail, 4, 1), id=id30134),
		  "Family 30-30142" = plot(ped30142, col=ifelse(fam30142$avail, 4, 1), id=id30142),
		  "Family 30-30117" = plot(ped30117, col=ifelse(fam30117$avail, 4, 1), id=id30117),
		  "Family 30-30136" = plot(ped30136, col=ifelse(fam30136$avail, 4, 1), id=id30136),
		  "Family 30-30135" = plot(ped30135, col=ifelse(fam30135$avail, 4, 1), id=id30135),
		  "Family 31-31114" = plot(ped31114, col=ifelse(fam31114$avail, 4, 1), id=id31114),
		  "Family 31-31119" = plot(ped31119, col=ifelse(fam31119$avail, 4, 1), id=id31119),
		  "Family 32-32213" = plot(ped32213, col=ifelse(fam32213$avail, 4, 1), id=id32213),
		  "Family 44-1053" = plot(ped441053, col=ifelse(fam441053$avail, 4, 1), id=id441053),
		  "Family 45-1040" = plot(ped451040, col=ifelse(fam451040$avail, 4, 1), id=id451040),
		  "Family 49-1002" = plot(ped491002, col=ifelse(fam491002$avail, 4, 1), id=id491002),
		  "Family 53-108" = plot(ped53108, col=ifelse(fam53108$avail, 4, 1), id=id53108),
		  "Family 56-195" = plot(ped56195, col=ifelse(fam56195$avail, 4, 1), id=id56195),
		  "Family 70-1088" = plot(ped701088, col=ifelse(fam701088$avail, 4, 1), id=id701088),
		  "Family 70-1096" = plot(ped701096, col=ifelse(fam701096$avail, 4, 1), id=id701096),
		  "Family 70-1120" = plot(ped701120, col=ifelse(fam701120$avail, 4, 1), id=id701120),
		  "Family 70-1179" = plot(ped701179, col=ifelse(fam701179$avail, 4, 1), id=id701179),
		  "Family 71-5077" = plot(ped715077, col=ifelse(fam715077$avail, 4, 1), id=id715077)
		  )
  })
  output$InfoTableAxeq <- renderDataTable({
    library(ggplot2)
    table <- datasetInputInfoAxeq()
    names(table)[80] = "AB"
    names(table)[81] = "AD"
    names(table)[82] = "DP"
    names(table)[83] = "GQ"
    names(table)[84] = "GT"
    names(table)[85] = "MQ0"
    names(table)[86] = "PL"
    table[,c(-11:-14,-16,-17,-21:-25,-27:-40,-42:-63,-65,-67,-69,-79), with=FALSE]
  })
  
  output$InfoTableGatk <- renderDataTable({
       library(ggplot2)
       table <- datasetInputInfoGatk()
       names(table)[80] = "AB"
       names(table)[81] = "AD"
       names(table)[82] = "DP"
       names(table)[83] = "GQ"
       names(table)[84] = "GT"
       names(table)[85] = "MQ0"
       names(table)[86] = "PL"
       table[,c(-8,-11:-14,-16,-17,-21:-25,-27:-40,-42:-63,-65,-67,-69,-79), with=FALSE]
  })
  
  output$InfoTableIndAxeq <- renderDataTable({
       library(ggplot2)
       table <- datasetInputInfoIndAxeq()
       names(table)[80] = "AB"
       names(table)[81] = "AD"
       names(table)[82] = "DP"
       names(table)[83] = "GQ"
       names(table)[84] = "GT"
       names(table)[85] = "MQ0"
       names(table)[86] = "PL"
       table[,c(-8:-40,-42:-63,-65,-67,-69,-79), with=FALSE]
  })
  
  output$InfoTableIndGatk <- renderDataTable({
       library(ggplot2)
       table <- datasetInputInfoIndGatk()
       names(table)[80] = "AB"
       names(table)[81] = "AD"
       names(table)[82] = "DP"
       names(table)[83] = "GQ"
       names(table)[84] = "GT"
       names(table)[85] = "MQ0"
       names(table)[86] = "PL"
       table[,c(-8:-40,-42:-63,-65,-67,-69,-79), with=FALSE]
  })
  
  output$InfoTableIndGatkT7 <- renderDataTable({
       library(ggplot2)
       table <- datasetInputInfoIndGatkT7()
       names(table)[80] = "AB"
       names(table)[81] = "AD"
       names(table)[82] = "DP"
       names(table)[83] = "GQ"
       names(table)[84] = "GT"
       names(table)[85] = "MQ0"
       names(table)[86] = "PL"
       table[,c(-8:-40,-42:-63,-65,-67,-69,-79), with=FALSE]
  })
  output$InfoTableIndGatkFPrecal <- renderDataTable({
       library(ggplot2)
       table <- datasetInputInfoIndGatkFPrecal()
       names(table)[80] = "AB"
       names(table)[81] = "AD"
       names(table)[82] = "DP"
       names(table)[83] = "GQ"
       names(table)[84] = "GT"
       names(table)[85] = "MQ0"
       names(table)[86] = "PL"
       table[,c(-8:-40,-42:-63,-65,-67,-69,-79), with=FALSE]
  })
     output$plot <- renderPlot({
		plotInput()
	}, height = 400, width = 400)
  
	output$ped <- renderPlot({
	    pedigreeInput()
		}, height = 400, width = 700)
  output$rel <- renderPrint({
    datasetInputRel()
  })
  output$heading <- renderUI({
    datasetHeading()
  })
  output$summary <- renderTable({
    datasetSummary()
  })
  output$summary_all <- renderDataTable({
    summary_all
  })
  output$relate <- renderText({ 
    paste("relatedeness =", datasetInputRel(), sep=" ")
  })
})