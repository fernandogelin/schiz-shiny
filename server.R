library(shiny)
library(shinyIncubator)
library(ggplot2)
library(rjson)

load('data/env2.RData', envir=.GlobalEnv)
load('data/summary.RData', envir=.GlobalEnv)
var01 <- readRDS('data/var01.json.rds')
var2 <- readRDS('data/var2.json.rds')
var3 <- readRDS('data/var3.json.rds')
var4 <- readRDS('data/var4.json.rds')
var5 <- readRDS('data/var5.json.rds')
var6 <- readRDS('data/var6.json.rds')
var7 <- readRDS('data/var7.json.rds')
var8 <- readRDS('data/var8.json.rds')
var9 <- readRDS('data/var9.json.rds')
var10 <- readRDS('data/var10.json.rds')
var11 <- readRDS('data/var11.json.rds')
var13 <- readRDS('data/var13.json.rds')
var14 <- readRDS('data/var14.json.rds')
var15 <- readRDS('data/var15.json.rds')
var16 <- readRDS('data/var16.json.rds')
var17 <- readRDS('data/var17.json.rds')
var18 <- readRDS('data/var18.json.rds')
var19 <- readRDS('data/var19.json.rds')

source('data/schizPedigree.R')
source('data/venn.R') 

shinyServer(function(input, output) {
  datasetInputInfo <- reactive({
    switch(input$dataset,
           "Family 30-30134" = as.data.table(fromJSON(var8)),
           "Family 30-30142" = as.data.table(fromJSON(var6)),
           "Family 30-30117" = as.data.table(fromJSON(var16)),
           "Family 30-30135" = as.data.table(fromJSON(var18)),
           "Family 30-30136" = as.data.table(fromJSON(var17)),
           "Family 31-31114" = as.data.table(fromJSON(var11)),
           "Family 31-31119" = as.data.table(fromJSON(var14)),
           "Family 32-32213" = as.data.table(fromJSON(var2)),
           "Family 44-1053" = as.data.table(fromJSON(var15)),
           "Family 45-1040" = as.data.table(fromJSON(var01)),
           "Family 49-1002" = as.data.table(fromJSON(var19)),
           "Family 53-108" = as.data.table(fromJSON(var4)),
           "Family 56-195" = as.data.table(fromJSON(var5)),
           "Family 70-1088" = as.data.table(fromJSON(var13)),
           "Family 70-1096" = as.data.table(fromJSON(var9)),
           "Family 70-1120" = as.data.table(fromJSON(var10)),
           "Family 70-1179" = as.data.table(fromJSON(var3)),
           "Family 71-5077" = as.data.table(fromJSON(var7))
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
  datasetSummary <- reactive({
    switch(input$dataset,
           "Family 30-30134" = summary.table.id_30134,
           "Family 30-30142" = summary.table.id_30142,
           "Family 30-30117" = summary.table.id_30117,
           "Family 30-30135" = summary.table.id_30135,
           "Family 30-30136" = summary.table.id_30136,
           "Family 31-31114" = summary.table.id_31114,
           "Family 31-31119" = summary.table.id_31119,
           "Family 32-32213" = summary.table.id_32213,
           "Family 44-1053" = summary.table.id_441053,
           "Family 45-1040" = summary.table.id_451040,
           "Family 49-1002" = summary.table.id_491002,
           "Family 53-108" = summary.table.id_53108,
           "Family 56-195" = summary.table.id_56195,
           "Family 70-1088" = summary.table.id_701088,
           "Family 70-1096" = summary.table.id_701096,
           "Family 70-1120" = summary.table.id_701120,
           "Family 70-1179" = summary.table.id_701179,
           "Family 71-5077" = summary.table.id_715077
    )
  })
  datasetInputRel <- reactive({
    switch(input$dataset,
           "Family 30-30134" = 0.0625,
           "Family 30-30142" = 0.03125,
           "Family 30-30117" = 0.015625,
           "Family 30-30135" = 0.0625,
           "Family 30-30136" = 0.0,
           "Family 31-31114" = 0.03125,
           "Family 31-31119" = 0.015625,
           "Family 32-32213" = 0.5,
           "Family 44-1053" = 0.0625,
           "Family 45-1040" = 0.0078125,
           "Family 49-1002" = 0.09375,
           "Family 53-108" = 0.015625,
           "Family 56-195" = 0.015625,
           "Family 70-1088" = 0.03125,
           "Family 70-1096" = 0.0625,
           "Family 70-1120" = 0.00197266,
           "Family 70-1179" = 0.03125,
           "Family 71-5077" = 0.03125
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
		  "Family 70-1096" = plot(ped701120, col=ifelse(fam701096$avail, 4, 1), id=id701096),
		  "Family 70-1120" = plot(ped701120, col=ifelse(fam701120$avail, 4, 1), id=id701120),
		  "Family 70-1179" = plot(ped701179, col=ifelse(fam701179$avail, 4, 1), id=id701179),
		  "Family 71-5077" = plot(ped715077, col=ifelse(fam715077$avail, 4, 1), id=id715077)
		  )
  })
  output$InfoTable <- renderDataTable({
    library(ggplot2)
    table <- datasetInputInfo()
    table
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
})