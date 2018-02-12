library(shiny)
library(markdown)
library(data.table)
library(plyr)
library(sqldf)
library(igraph)
library(data.table)
library(networkD3)
ui <- navbarPage('Wanjing_Zhang_Social Network Analysis',
                   tabPanel('Upload_File',
                            sidebarLayout(
                              sidebarPanel(
                                fileInput('file1',h3('Upload "email-Eu-core-department-labels.txt" Here'),
                                          multiple=FALSE),
                                tags$hr(),
                                fileInput('file2',h3('Upload "email-Eu-core.txt" Here'),
                                          multiple=FALSE),
                                tags$hr(),
                                numericInput('num',h3('Display n connections'),value=1)
                              ),
                              mainPanel(
                                tableOutput('connections')
                              )
                            )
                   ),
                   navbarMenu(
                     title = "Top 10 Email Sent/Receive",
                     tabPanel("Count Email Sent",tableOutput('countsent')),
                     tabPanel("Count Email Received",tableOutput('countreceive')),
                     tabPanel('2 Hop of Top 10 Email Sent',tableOutput('top10s'),
                              htmlOutput("selectUI"),
                              tabsetPanel(type = "tabs",
                                          tabPanel("hop-1 neighors", tableOutput("t4h1")),
                                          tabPanel("hop-2 neighbors",tableOutput("t4h2"))
                              )),
                     tabPanel('2 Hop of Top 10 Email Received',tableOutput('top10r'),
                              htmlOutput("selectUI2"),
                              tabsetPanel(type = "tabs",
                                          tabPanel("hop-1 neighors", tableOutput("t5h1")),
                                          tabPanel("hop-2 neighbors",tableOutput("t5h2"))))
                     ),
                 tabPanel('Top 10 Degree Centrality',
                          tableOutput('dc10'),
                          htmlOutput('selectUI3'),
                          forceNetworkOutput('dc1'),
                          htmlOutput('selectUI4'),
                          forceNetworkOutput('dc2')
                            ),
                 tabPanel('Top 10 Betweenness Centrality',
                          tableOutput('bc10'),
                          htmlOutput('selectUI5'),
                          forceNetworkOutput('bc1'),
                          htmlOutput('selectUI6'),
                          forceNetworkOutput('bc2')
                          ),
                 tabPanel('Top 10 Indegree Centrality',
                          tableOutput('ic10'),
                          htmlOutput('selectUI7'),
                          forceNetworkOutput('ic1'),
                          htmlOutput('selectUI8'),
                          forceNetworkOutput('ic2')),
                 navbarMenu(
                   title = "Department",
                   tabPanel("Table",tableOutput('dept')),
                   tabPanel('Visualiztion',
                            htmlOutput("selectUI9"),
                            forceNetworkOutput('vd')))
)
server<- function(input,output){
  data<-reactive({
    req(input$file2)
    inFile <- input$file2
    df<-read.table(inFile$datapath,sep='',header=FALSE,col.names =c('sender','receiver'))
  })
  dfa<-reactive({
    req(input$file1)
    inFile<-input$file1
    df<-read.table(inFile$datapath,sep='',header=FALSE,col.names=c('id','dep'))
  })
  top4<-reactive({
    table<-count(data(),"sender")
    t10<-data.frame(head(arrange(table,desc(freq)), n = 10))
    colnames(t10)[1]<-'top10_sender'
    keeps <- c("top10_sender")
    t10[keeps]
  })
  top5<-reactive({
    table2<-count(data(),"receiver")
    t10_2<-data.frame(head(arrange(table2,desc(freq)), n = 10))
    colnames(t10_2)[1]<-'top10_receiver'
    keeps <- c("top10_receiver")
    t10_2[keeps]
  })
  nodes<-reactive({
    req(input$file2)
    inFile <- input$file2
    df<-read.table(inFile$datapath,sep='',header=FALSE,col.names =c('sender','receiver'))
    nodes <- data.frame(id=unique(c(df$sender, df$receiver)))
  })
  q4h1<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$topSid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    colnames(hop1id)[1]<-"id"
    return(hop1id)
  })
  q4h2<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$topSid],to=V(net), weights=NA)
    x<-which(dfid==2, arr.in=TRUE)
    hop2id<-data.frame(colnames(dfid)[x[,2]])
    colnames(hop2id)[1]<-"id"
    return(hop2id)
  })
  q5h1<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$topRid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    colnames(hop1id)[1]<-"id"
    return(hop1id)
  })
  q5h2<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$topRid],to=V(net), weights=NA)
    x<-which(dfid==2, arr.in=TRUE)
    hop2id<-data.frame(colnames(dfid)[x[,2]])
    colnames(hop2id)[1]<-"id"
    return(hop2id)
  })
  tdc10<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    rIC10<-data.frame(head(sort(degree(net, loops = FALSE),decreasing =TRUE ),n=10))
    IC10<-setDT(rIC10,keep.rownames = TRUE)[]
    colnames(IC10)[1]<-'id'
    return(IC10[,1])
    })
  dchop1<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    dfid <- distances(net, v=V(net)[name==input$dcid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1dc<-data.frame(colnames(dfid)[x[,2]])
    colnames(hop1dc)[1]<-"id"
    return(hop1dc)
  })
  dchoplink<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    dfid <- distances(net, v=V(net)[name==input$dcid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    nodeid<-rownames(dfid)[x[,1]]
    hop1id$nodeid<-nodeid
    colnames(hop1id)[1]<-'hop1'
    return(hop1id)
  })
  dchop2link<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$dcid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    nodeid<-rownames(dfid)[x[,1]]
    hop1id$nodeid<-nodeid
    colnames(hop1id)[1]<-'hop1'
    y<-which(hop==1, arr.in=TRUE)
    net1id<-data.frame(colnames(hop)[y[,2]])
    colnames(net1id)[1]<-'x'
    netrow<-data.frame(colnames(hop)[y[,1]])
    colnames(netrow)[1]<-'y'
    net1id$y<-netrow$y
    df7<-net1id[net1id$x %in% hop1id$hop1,]
    df8<-df7[df7$x==input$hop1,]
    df9 <-data.frame(sqldf("SELECT*FROM df8 WHERE df8.y NOT IN (SELECT hop1 FROM hop1id)"))
    })
  tbc10<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    rDB10<-data.frame(head(sort(betweenness(net), decreasing = TRUE),n=10))
    DB10<-setDT(rDB10, keep.rownames = TRUE)[]
    colnames(DB10)[1]<-'id'
    return(DB10[,1])
  })
  bchop1<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    dfid <- distances(net, v=V(net)[name==input$bcid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1dc<-data.frame(colnames(dfid)[x[,2]])
    colnames(hop1dc)[1]<-"id"
    return(hop1dc)
  })
  bchoplink<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    dfid <- distances(net, v=V(net)[name==input$bcid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    nodeid<-rownames(dfid)[x[,1]]
    hop1id$nodeid<-nodeid
    colnames(hop1id)[1]<-'hop1'
    return(hop1id)
  })
  bchop2link<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$bcid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    nodeid<-rownames(dfid)[x[,1]]
    hop1id$nodeid<-nodeid
    colnames(hop1id)[1]<-'hop1'
    y<-which(hop==1, arr.in=TRUE)
    net1id<-data.frame(colnames(hop)[y[,2]])
    colnames(net1id)[1]<-'x'
    netrow<-data.frame(colnames(hop)[y[,1]])
    colnames(netrow)[1]<-'y'
    net1id$y<-netrow$y
    df7<-net1id[net1id$x %in% hop1id$hop1,]
    df8<-df7[df7$x==input$hop1a,]
    df9 <-data.frame(sqldf("SELECT*FROM df8 WHERE df8.y NOT IN (SELECT hop1 FROM hop1id)"))
  })
  tic10<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    rDC10<-data.frame(head(sort(degree(net, mode='in'), decreasing = TRUE), n = 10) )
    DC10<-setDT(rDC10,keep.rownames=TRUE)[]
    colnames(DC10)[1]<-'id'
    return(DC10[,1])
  })
  ichop1<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    dfid <- distances(net, v=V(net)[name==input$icid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1dc<-data.frame(colnames(dfid)[x[,2]])
    colnames(hop1dc)[1]<-"id"
    return(hop1dc)
  })
  ichoplink<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    dfid <- distances(net, v=V(net)[name==input$icid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    nodeid<-rownames(dfid)[x[,1]]
    hop1id$nodeid<-nodeid
    colnames(hop1id)[1]<-'hop1'
    return(hop1id)
  })
  ichop2link<-reactive({
    net<-graph_from_data_frame(d=data(), vertices=nodes(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$icid],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    hop1id<-data.frame(colnames(dfid)[x[,2]])
    nodeid<-rownames(dfid)[x[,1]]
    hop1id$nodeid<-nodeid
    colnames(hop1id)[1]<-'hop1'
    y<-which(hop==1, arr.in=TRUE)
    net1id<-data.frame(colnames(hop)[y[,2]])
    colnames(net1id)[1]<-'x'
    netrow<-data.frame(colnames(hop)[y[,1]])
    colnames(netrow)[1]<-'y'
    net1id$y<-netrow$y
    df7<-net1id[net1id$x %in% hop1id$hop1,]
    df8<-df7[df7$x==input$hop1b,]
    df9 <-data.frame(sqldf("SELECT*FROM df8 WHERE df8.y NOT IN (SELECT hop1 FROM hop1id)"))
  })
  dt<-reactive({
    inFile2 <- input$file2
    df1<-read.table(inFile2$datapath,sep='',header=FALSE,col.names =c('sender','receiver'))
    inFile1<-input$file1
    df2<-read.table(inFile1$datapath,sep='',header=FALSE,col.names=c('id','dep'))
    formal<-sqldf('SELECT df1.receiver, df2.dep FROM df1 INNER JOIN df2 ON df1.receiver = df2.id')
    formal2<-sqldf('SELECT df1.sender, df2.dep FROM df1 INNER JOIN df2 ON df1.sender = df2.id')
    com<-data.frame(formal2$dep)
    colnames(com)[1]<-'senderDepartment'
    com$receiverDepartment<-formal$dep
    return(com)
  })
  depN<-reactive({
    inFile<-input$file1
    df<-read.table(inFile$datapath,sep='',header=FALSE,col.names=c('id','dep'))
    dn<- data.frame(id=unique(df$dep))
    Dep<-arrange(dn,id)
    })
  depG<-reactive({
    net <- graph_from_data_frame(d=dt(), vertices=depN(), directed=T)
    hop<-distances(net)
    dfid <- distances(net, v=V(net)[name==input$dep],to=V(net), weights=NA)
    x<-which(dfid==1, arr.in=TRUE)
    d<-data.frame(colnames(dfid)[x[,2]])
    nodeid<-rownames(dfid)[x[,1]]
    d$dep<-nodeid
    colnames(d)[1]<-'conn'
    countd<-count(d)
    return(countd)
    })
  dG<-reactive({
    c1<-dt()[dt()$senderDepartment==input$dep,]
    c2<-dt()[dt()$receiverDepartment==input$dep,]
    comb<-rbind(c1,c2)
    c0<-count(comb)
  })
  output$connections<-renderTable({head(data(),n=input$num)})
  output$countsent<-renderTable(count(data(),'sender'))
  output$countreceive<-renderTable(count(data(),'receiver'))
  output$top10s<-renderTable(top4())
  output$top10r<-renderTable(top5())
  output$selectUI <- renderUI({ 
    selectInput("topSid", "Select id", top4() )
  })
  output$selectUI2 <- renderUI({ 
    selectInput("topRid", "Select id", top5() )
  })
  output$t4h1<-renderTable(q4h1())
  output$t4h2<-renderTable(q4h2())
  output$t5h1<-renderTable(q5h1())
  output$t5h2<-renderTable(q5h2())
  output$dc10<-renderTable(tdc10())
  output$selectUI3 <- renderUI({ 
    selectInput("dcid", "Select one of the Top 10 id and see its 1-hop neighbors", tdc10() )
  })
  output$selectUI4<-renderUI({
    selectInput('hop1','Select one of its 1-hop and see its 2-hop neighbors connected by this 1-hop',dchop1())
  })
  output$dc1<-renderForceNetwork({forceNetwork(Links=dchoplink(), Nodes =dfa(),NodeID = "id", Group = "dep",zoom=TRUE)})
  output$dc2<-renderForceNetwork({forceNetwork(Links=dchop2link(), Nodes =dfa(),NodeID = "id", Group = "dep",zoom=TRUE)})
  output$bc10<-renderTable(tbc10())
  output$selectUI5<-renderUI({
    selectInput("bcid", "Select one of the Top 10 id and see its 1-hop neighbors", tbc10() )
  })
  output$selectUI6<-renderUI({
    selectInput('hop1a','Select one of its 1-hop and see its 2-hop neighbors connected by this 1-hop',bchop1())
  })
  output$bc1<-renderForceNetwork({forceNetwork(Links=bchoplink(), Nodes =dfa(),NodeID = "id", Group = "dep",zoom=TRUE)})
  output$bc2<-renderForceNetwork({forceNetwork(Links=bchop2link(), Nodes =dfa(),NodeID = "id", Group = "dep",zoom=TRUE)})
  output$ic10<-renderTable(tic10())
  output$selectUI7<-renderUI({
    selectInput("icid", "Select one of the Top 10 id and see its 1-hop neighbors", tic10() )
  })
  output$selectUI8<-renderUI({
    selectInput('hop1b','Select one of its 1-hop and see its 2-hop neighbors connected by this 1-hop',ichop1())
  })
  output$ic1<-renderForceNetwork({forceNetwork(Links=ichoplink(), Nodes =dfa(),NodeID = "id", Group = "dep",zoom=TRUE)})
  output$ic2<-renderForceNetwork({forceNetwork(Links=ichop2link(), Nodes =dfa(),NodeID = "id", Group = "dep",zoom=TRUE)})
  output$dept<-renderTable(count(dt()))
  output$selectUI9<-renderUI({
    selectInput('dep','Select Department',depN())
  })
  output$vd<-renderForceNetwork({forceNetwork(Links=dG(),Nodes=depN(),Source = "senderDepartment", Target = "receiverDepartment",Value = "freq",NodeID='id',Group='id',arrows=TRUE,zoom=TRUE)})
}
shinyApp(ui,server)

