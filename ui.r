  #using the navbar
  getTool <- function(inputId) {
  tagList(
    singleton(tags$head(tags$script(src = "js/navbar.js"))),
    tags$html(includeHTML('www/navbar.html'))
  )
}

# UI for app
shinyUI(pageWithSidebar(
 
 # title
  headerPanel(""),
  
  # input
  sidebarPanel
  (
	  getTool("tool"), # navbar
		
		#using NAVBAR
		conditionalPanel(condition = "input.tool == 'data'|input.tool == 'dataview'",
			h3('Data'),
				uiOutput("datasets"),
				uiOutput("network_data_upload"),
				# tags$details(tags$summary("Upload Options"),
				# withTags(div(class='row-fluid',
					 # div(class='span3', checkboxInput(inputId = "csv_row_header", label = "row names",value=TRUE)),
					 # div(class='span5', checkboxInput(inputId = "csv_col_header", label = "column names",value=TRUE)))
					 # ),
				# HTML("<label>Load data: (.csv)</label>"),
				# uiOutput("upload_local_server"),
				# HTML("<label>Paste data:</label>"),
				# tags$textarea(id="copyAndPaste", rows=3, cols=40, "")),
				uiOutput("network_data_manage"),
				uiOutput("data_translation_options")
		),
		conditionalPanel(condition = "input.tool == 'network'",		
			h3('Network Options'),
			uiOutput("network_index_info_bio"),
			uiOutput("network_index_info_chem"),
			uiOutput("network_index_info_spec"),
			uiOutput("network_index_info_cor"),
			tags$details(tags$summary("More Options"),
				checkboxInput(inputId = "unique_edges", label = "Unique edges",value=TRUE),
				uiOutput("translate_edge_index")
				)
		)		
	),
				
				
  mainPanel(
	# h3(input.tool),
	# conditionalPanel(condition = "input.sidepanel == 'Data'",
	conditionalPanel(condition = "input.tool == 'data'|input.tool == 'dataview'",
		tabPanel("Data", list(downloadButton('downloadDataObject', 'Download'),tableOutput("view_data")))),
	# conditionalPanel(condition = "input.sidepanel == 'Network'",	
	conditionalPanel(condition = "input.tool == 'network'",
		tabsetPanel( id = "metabomapr",
			# tabPanel("Data", tableOutput("view_data")),
			tabPanel("Edge List", list(actionButton("create_edgelist", "Calculate Connections"),
			downloadButton('downloadEdgeList', 'Download'),
			br(),
			br(),
			tableOutput("edge_list"))),
			tabPanel("Node Attributes",downloadButton('downloadNodeAttributes', 'Download'),
				tableOutput("node.attributes")),
			tabPanel("Network", list(actionButton("create_edgelist_network", "Draw Network"),
			tags$details( # options for network plotting
				checkboxInput(inputId = "network_plot_show_name", label = "show names",value=TRUE),
				uiOutput("node_names"),
				checkboxInput(inputId = "network_plot_bezier", label = "curved edges",value=FALSE),
				numericInput(inputId = "network_plot_edge_size", "edge thickness", min = 0, max = 20, value = 2, step = .25),
				numericInput(inputId = "network_plot_name_size", "label size", min = 0, max = 20, value = 5, step = 1),
				numericInput(inputId = "network_plot_node_size", "vertex size", min = 0, max = 20, value = 5, step = 1)
			),
			br(), 
			plotOutput("network",width = 850, height = 650))),#height = "100%"
			tabPanel("Debug", verbatimTextOutput("debug"))),
			conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
				id='progressIndicator',
				"Calculating...",
				div(id='progress',includeHTML("www/js/timer.js"))
			)
	),
		tags$head(tags$style(type="text/css",
		  '#progressIndicator {',
		  '  position: fixed; top: 100px; right: 8px; width: 200px; height: 50px;',
		  '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
		  '}'
		))
	)
))