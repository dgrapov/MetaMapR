  #using the navbar
  getTool <- function(inputId) {
  tagList(
    singleton(tags$head(tags$script(src = "js/navbar.js"))),
    tags$html(includeHTML('www/navbar.html'))
  )
}

#custom mods to shiny UI to get
mainPanel2<-function (..., width1 = 4, width2=10) 
{
    div(class = paste0("span", width2," ","push",width1), ...)
}

# UI for app w/ fluid row UI
shinyUI(fluidPage(
 
  # navbar
  getTool("tool"),
  br(),
  br(),
  #input to the left
	fluidRow(
		column(3,
			wellPanel(
				tags$head(tags$script(src = "http://d3js.org/d3.v3.min.js")) , #for d3networks
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
			)
		),
		
		column(9,
			#progress indicator
			tagList(
				tags$head(
				  tags$link(rel="stylesheet", type="text/css",href="www/style.css"),
				  tags$script(type="text/javascript", src = "js/busy.js"),
				  tags$script(type="text/javascript", src = "js/timer.js") 
				)
			),
			div(class = "busy",  
			  img(src="images/progressauto.gif"),
			  div(id='progress',includeHTML("www/js/timer.js"))
			),
			includeCSS('www/css/style.css'),
			# h3(input.tool),
			# conditionalPanel(condition = "input.sidepanel == 'Data'",
			conditionalPanel(condition = "input.tool == 'data'|input.tool == 'dataview'",
				tabPanel("Data", list(downloadButton('downloadDataObject', 'Download'),tableOutput("view_data")))),
			# conditionalPanel(condition = "input.sidepanel == 'Network'",	
			conditionalPanel(condition = "input.tool == 'network'",
				tabsetPanel( id = "metabomapr",
					#Network
					tabPanel("Network", list(actionButton("create_edgelist_network", "Draw Network"),
					br(),
					br(),
					tags$details( # options for network plotting
						fluidRow(
							column(2,
							h5("Plot"),
							  checkboxGroupInput(inputId="network_plot_type", label="Plot type", choices=c("interactive","PNG","SVG"),selected = c("interactive")),
							  numericInput(inputId = "plot_output_width", "width", min = 0,  value = 400, step = 10),
							  numericInput(inputId = "plot_output_height", "height", min = 0,  value = 400, step = 10),
							  tags$style(type="text/css", "#plot_output_height     { width:50px;}"),
							  tags$style(type="text/css", "#plot_output_width     { width:50px;}")
							),
							column(2,
								h5("Nodes"),
								checkboxInput(inputId = "network_plot_show_name", label = "show names",value=TRUE),
								uiOutput("node_names"),
								numericInput(inputId = "network_plot_name_size", "label size", min = 0, max = 20, value = 5, step = 1),
								numericInput(inputId = "network_plot_node_size", "vertex size", min = 0, max = 20, value = 5, step = 1),
								numericInput(inputId = "network_plot_node_alpha", "transparency", min = 0, max = 1, value = 1, step = .1),
								tags$style(type="text/css", "#network_plot_name_size     { width:50px;}"),
								tags$style(type="text/css", "#network_plot_node_size     { width:50px;}"),
								tags$style(type="text/css", "#node_names     { width:150px;}"),
								tags$style(type="text/css", "#network_plot_node_alpha     { width:50px;}")
							),
							column(2,
								h5("Edges"),
								checkboxInput(inputId = "network_plot_bezier", label = "curved edges",value=FALSE), # only with renderPlot too slow for SVG
								numericInput(inputId = "network_plot_edge_size", "thickness", min = 1,  value = 2, step = .25),
								numericInput(inputId = "network_plot_edge_length", "length", min = 1,  value = 70, step = 10),
								tags$style(type="text/css", "#network_plot_edge_length     { width:50px;}"),
								tags$style(type="text/css", "#network_plot_edge_size     { width:50px;}"),
								numericInput(inputId = "network_plot_edge_alpha", "transparency", min = .01,  value = .6, step = .1),
								tags$style(type="text/css", "#network_plot_edge_alpha     { width:50px;}")
							)
						  )
					),
					br(),
						# htmlOutput('networkPlot_legend'), # can not correctly position the legend as html, including in network
						htmlOutput('networkPlot'),
						htmlOutput("SVGnetwork"),
						plotOutput("network")
					)),#height = "100%"

					#edge list
					tabPanel("Edge List", list(actionButton("create_edgelist", "Calculate Connections"),
					downloadButton('downloadEdgeList', 'Download'),
					htmlOutput('edgelist_error_message'),
					# br(),
					# br(),
					tableOutput("edge_list"))),
					#node attributes
					tabPanel("Node Attributes",downloadButton('downloadNodeAttributes', 'Download'),
						br(),
						br(),
						tableOutput("node.attributes")),
					
					tabPanel("Debug", verbatimTextOutput("debug")))#,
					# conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
						# id='progressIndicator',
						# "Calculating...",
						# div(id='progress',includeHTML("www/js/timer.js"))
					# )
				)#,
				# tags$head(tags$style(type="text/css",
				  # '#progressIndicator {',
				  # '  position: fixed; top: 100px; right: 8px; width: 200px; height: 50px;',
				  # '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
				  # '}'
				# ))
		)
	)	
))




# # UI for app
# shinyUI(pageWithSidebar(
 
 # # title
  # headerPanel(""),
 
  # # input
  # sidebarPanel
  # (
	  # getTool("tool"), # navbar
		# tags$head(tags$script(src = "http://d3js.org/d3.v3.min.js")) , #for d3networks
		# #using NAVBAR
		# conditionalPanel(condition = "input.tool == 'data'|input.tool == 'dataview'",
			# h3('Data'),
				# uiOutput("datasets"),
				# uiOutput("network_data_upload"),
				# # tags$details(tags$summary("Upload Options"),
				# # withTags(div(class='row-fluid',
					 # # div(class='span3', checkboxInput(inputId = "csv_row_header", label = "row names",value=TRUE)),
					 # # div(class='span5', checkboxInput(inputId = "csv_col_header", label = "column names",value=TRUE)))
					 # # ),
				# # HTML("<label>Load data: (.csv)</label>"),
				# # uiOutput("upload_local_server"),
				# # HTML("<label>Paste data:</label>"),
				# # tags$textarea(id="copyAndPaste", rows=3, cols=40, "")),
				# uiOutput("network_data_manage"),
				# uiOutput("data_translation_options")
		# ),
		# conditionalPanel(condition = "input.tool == 'network'",		
			# h3('Network Options'),
			# uiOutput("network_index_info_bio"),
			# uiOutput("network_index_info_chem"),
			# uiOutput("network_index_info_spec"),
			# uiOutput("network_index_info_cor"),
			# tags$details(tags$summary("More Options"),
				# checkboxInput(inputId = "unique_edges", label = "Unique edges",value=TRUE),
				# uiOutput("translate_edge_index")
				# )
		# )		
	# ),
				
				
  # mainPanel2(
	# #progress indicator
	# tagList(
		# tags$head(
		  # tags$link(rel="stylesheet", type="text/css",href="www/style.css"),
		  # tags$script(type="text/javascript", src = "js/busy.js"),
		  # tags$script(type="text/javascript", src = "js/timer.js") 
		# )
	# ),
	# div(class = "busy",  
	  # img(src="images/progressauto.gif"),
	  # div(id='progress',includeHTML("www/js/timer.js"))
	# ),
	# includeCSS('www/css/style.css'),
	# # h3(input.tool),
	# # conditionalPanel(condition = "input.sidepanel == 'Data'",
	# conditionalPanel(condition = "input.tool == 'data'|input.tool == 'dataview'",
		# tabPanel("Data", list(downloadButton('downloadDataObject', 'Download'),tableOutput("view_data")))),
	# # conditionalPanel(condition = "input.sidepanel == 'Network'",	
	# conditionalPanel(condition = "input.tool == 'network'",
		# tabsetPanel( id = "metabomapr",
			# #Network
			# tabPanel("Network", list(actionButton("create_edgelist_network", "Draw Network"),
			# br(),
			# br(),
			# tags$details( # options for network plotting
				# fluidRow(
					# column(2,
					# h5("Plot"),
					  # checkboxGroupInput(inputId="network_plot_type", label="Plot type", choices=c("interactive","PNG","SVG"),selected = c("interactive")),
					  # numericInput(inputId = "plot_output_width", "width", min = 0,  value = 850, step = 10),
					  # numericInput(inputId = "plot_output_height", "height", min = 0,  value = 850, step = 10),
					  # tags$style(type="text/css", "#plot_output_height     { width:50px;}"),
					  # tags$style(type="text/css", "#plot_output_width     { width:50px;}")
					# ),
					# column(2,
						# h5("Nodes"),
						# checkboxInput(inputId = "network_plot_show_name", label = "show names",value=TRUE),
						# uiOutput("node_names"),
						# numericInput(inputId = "network_plot_name_size", "label size", min = 0, max = 20, value = 5, step = 1),
						# numericInput(inputId = "network_plot_node_size", "vertex size", min = 0, max = 20, value = 5, step = 1),
						# numericInput(inputId = "network_plot_node_alpha", "transparency", min = 0, max = 1, value = 1, step = .1),
						# tags$style(type="text/css", "#network_plot_name_size     { width:50px;}"),
					    # tags$style(type="text/css", "#network_plot_node_size     { width:50px;}"),
						# tags$style(type="text/css", "#node_names     { width:150px;}"),
						# tags$style(type="text/css", "#network_plot_node_alpha     { width:50px;}")
					# ),
					# column(2,
						# h5("Edges"),
						# checkboxInput(inputId = "network_plot_bezier", label = "curved edges",value=FALSE), # only with renderPlot too slow for SVG
						# numericInput(inputId = "network_plot_edge_size", "thickness", min = 1,  value = 2, step = .25),
						# tags$style(type="text/css", "#network_plot_edge_size     { width:50px;}"),
						# numericInput(inputId = "network_plot_edge_alpha", "transparency", min = .01,  value = .6, step = .1),
						# tags$style(type="text/css", "#network_plot_edge_alpha     { width:50px;}")
					# )
				  # )
			# ),
			# br(),
				# # htmlOutput('networkPlot_legend'), # can not correctly position the legend as html, including in network
				# htmlOutput('networkPlot'),
				# htmlOutput("SVGnetwork"),
				# plotOutput("network")
			# )),#height = "100%"

			# #edge list
			# tabPanel("Edge List", list(actionButton("create_edgelist", "Calculate Connections"),
			# downloadButton('downloadEdgeList', 'Download'),
			# htmlOutput('edgelist_error_message'),
			# # br(),
			# # br(),
			# tableOutput("edge_list"))),
			# #node attributes
			# tabPanel("Node Attributes",downloadButton('downloadNodeAttributes', 'Download'),
				# br(),
				# br(),
				# tableOutput("node.attributes")),
			
			# tabPanel("Debug", verbatimTextOutput("debug")))#,
			# # conditionalPanel("updateBusy() || $('html').hasClass('shiny-busy')",
				# # id='progressIndicator',
				# # "Calculating...",
				# # div(id='progress',includeHTML("www/js/timer.js"))
			# # )
		# )#,
		# # tags$head(tags$style(type="text/css",
		  # # '#progressIndicator {',
		  # # '  position: fixed; top: 100px; right: 8px; width: 200px; height: 50px;',
		  # # '  padding: 8px; border: 1px solid #CCC; border-radius: 8px;',
		  # # '}'
		# # ))
	# )
# ))