#' UI for the Shiny app `sfollow()`
#' @import shiny
#' @return A UI function
sfollow_ui <- function(){

  fluidPage(
    ### header
    wellPanel(
      h4("Type an ID and clock 'Go':"),
      textInput('fr_id', label = '', value = 'dapeng'),
      actionButton('fr_go', 'Go!')
    ),

    ### follower vs following
    wellPanel(
      h4("Results"),
	  # wordclouds for the followers
          plotOutput('fr_plot'),
          p(),
          fluidRow(
            column(4, sliderInput("fr_slider_followers",
                                  "Followers Top:",
                                  min = 1,
                                  max = 100,
                                  value = c(1, 30)
            )),
            column(4, sliderInput("fr_slider_posts",
                                  "Posts Top:",
                                  min = 1,
                                  max = 100,
                                  value = c(1, 30)
            )),
            column(4, sliderInput("fr_slider_vests",
                                  "Vests Top:",
                                  min = 1,
                                  max = 100,
                                  value = c(1, 30)
            ))
          ),
          hr(),
		  # Ven diagram for the followers and followings
          wellPanel(
            plotOutput('fr_venplot')
          ),

      hr(),
      strong("Following but not followers"),
      uiOutput('fr_follower'),
      p(),
      hr(),
      p(),
      strong("Follower but not following"),
      uiOutput('fr_following')
    ),

    ### follower report
    wellPanel(
      h4("Data of his/her followers"),
      dataTableOutput('fr_dtfer')
    ),

    ### following report
    wellPanel(
      h4("Data of his/her following"),
      dataTableOutput('fr_dtfing')
    )
  )
}


#' Server for the Shiny app sfollow display and analysis
#'
#' @param input The input of the server.
#' @param output The output of the server.
#' @param session The session of the server.
#' @return A server function
sfollow_server <- function(input, output, session) {


  ### input the ID
  myid <- eventReactive(input$fr_go, {
    ifelse(substr(input$fr_id, 1, 1) == '@',
           gsub('@', '', input$fr_id),
           input$fr_id)
  })

  ### Get the followers of the given ID
  fer <- eventReactive(input$fr_go,
                       {gfollower(id = myid())}
  )

  ### Get the following of the given ID
  fing <- eventReactive(input$fr_go,
                        {
                          fing <- gfollowing(id = myid())
                          fing$Action <- ifelse(fing$What == 'Follow', 'follow', 'un')
                          fing
                        }
  )

  ### Get the list of follower vs following
  mylist <- eventReactive(input$fr_go, {
    myferfing <- gfollow(id = myid())
    myfer <- unique(myferfing$followers)
    myfing <- unique(myferfing$following)
    finginfer <- myfing %in% myfer
    ferinfing <- myfer %in% myfing
    mylist <- list(not_follower = myfing[!finginfer][order(myfing[!finginfer])],
                   not_following = myfer[!ferinfing][order(myfer[!ferinfing])],
                   followers = myfer,
                   following = myfing)
    mylist
  })

  ### create the html code for the report of follower vs following
  output$fr_follower <- renderUI({
    HTML(idlink(mylist()[[1]]))
  })

  output$fr_following <- renderUI({
    HTML(idlink(mylist()[[2]]))
  })

  ### create the slide bars for the wordclouds
    observe({
    slmax <- nrow(fer())
    # Control the value, min, max, and step.
    # Step size is 2 when input value is even; 1 when value is odd.
    updateSliderInput(session,
                      "fr_slider_followers",
                      value = c(1, ifelse(slmax > 36, 36, slmax)),
                      min = 1,
                      max = slmax,
                      step = 1)
    updateSliderInput(session,
                      "fr_slider_posts",
                      value = c(1, ifelse(slmax > 36, 36, slmax)),
                      min = 1,
                      max = slmax,
                      step = 1)
    updateSliderInput(session,
                      "fr_slider_vests",
                      value = c(1, ifelse(slmax > 36, 36, slmax)),
                      min = 1,
                      max = slmax,
                      step = 1)
  })

  ### create the Venn diagram for the followers and followings
  output$fr_venplot <- renderPlot({
    VennDiagram::draw.pairwise.venn(area1 = length(mylist()$followers),
                                    area2 = length(mylist()$following),
                                    cross.area = sum(mylist()$following %in% mylist()$followers),
                                    category = c("Followers", "Following"),
                                    fill = c("red", "blue"),
                                    alpha=c(0.2,0.2),
                                    ext.text = TRUE,
                                    ext.percent = c(0.1,0.1,0.1),
                                    ext.length = 0.6,
                                    label.col = rep("gray10",3),
                                    lwd = 0,
                                    cex = 2,
                                    fontface = rep("bold",3),
                                    fontfamily = rep("sans",3),
                                    cat.cex = 1.5,
                                    cat.fontface = rep("plain",2),
                                    cat.fontfamily = rep("sans",2),
                                    cat.pos = c(0, 0),
                                    print.mode = c("percent","raw")
    )
  })

    ### create the wordclouds for the followers
  output$fr_plot <- renderPlot({
    if (nrow(fer()) == 0) {
      plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
      text(1, 1, 'No data for wordclouds!', col = 'red', cex = 2)
    } else {
      datatime =  format(Sys.time(), format = '%Y-%m-%d %H:%M:%S %Z')
      par(mar = c(0,0,0,0))
      layout(mat = matrix(
        c(1, 1, 1, 2, 2, 2, 3, 4, 5, 6, 6, 6),
        nrow = 4,
        byrow = TRUE
        ),
        heights = c(2, 2, 20, 2))
      plot(1,
           type = "n",
           axes = FALSE,
           xlab = "",
           ylab = "")
      text(1, 1,
           paste0('@', myid(), "'s followers"),
           col = 'steelblue',
           cex = 3)
      plot(1,
           type = "n",
           axes = FALSE,
           xlab = "",
           ylab = "")
      text(1, 1, datatime, cex = 2)
      freq <- 'vests'
      myfer <- fer()
      slr <- input$fr_slider_followers

      colplot <- c('Followers', 'Posts','vests')
      sliderplot <- list(input$fr_slider_followers,
                         input$fr_slider_posts,
                         input$fr_slider_vests)
      for (i in 1:3){
        wcplot <- myfer[rev(order(myfer[, colplot[i]])), ][sliderplot[[i]][1]: sliderplot[[i]][2], ]
        wordcloud::wordcloud(wcplot$Account,
                             freq = wcplot[, colplot[i]],
                             colors = RColorBrewer::brewer.pal(4, "Dark2"),
                             scale = c(4, 0.6))
        legend('top',
               legend = paste0(colplot[i],
                               ' Top ',
                               sliderplot[[i]][1],
                               '--',
                               sliderplot[[i]][2]),
               bty = 'n',
               cex = 2)
      }
      plot(1,
           type = "n",
           axes = FALSE,
           xlab = "",
           ylab = "")
      text(1, 1,
           'made by steemr',
           col = 'steelblue',
           cex = 2)
    }
  })

  ### Create the table of the followers
  output$fr_dtfer = renderDataTable({
    if (nrow(fer()) == 0) {
      data.frame(Account = NA,
                 Followers = NA,
                 Posts = NA,
                 vests = NA)
    } else {
      fer()[, c('Account', 'Followers', 'Posts', 'vests')]
    }
  },
  options = list(lengthMenu = c(10, 30, 50, 100),
                 pageLength = 10))

  ### Create the table of the following
  output$fr_dtfing = renderDataTable({
    fing()[, c(1, 2, 5)]
  },
  options = list(lengthMenu = c(10, 30, 50, 100, 1000),
                 pageLength = 10)
  )

}


#' A shiny app to display and anaylize the followers a given ID.
#' sfollow means shiny app for followers.
#'
#' @return a shinyapp which can be displayed in a web browser.
#' @export
#' @examples
#' \dontrun{
#' sfollow()
#' }
sfollow <- function(){
  return(shiny::shinyApp(ui = sfollow_ui, server = sfollow_server))
}

#' Plot function for the Shiny app scner
#'
#' @param i A numeric indicator
#' @param sliderplot The ID of the sliderplot
#' @param whwechat The data from of the cner
#' @param zh The Chinese dictionary
#'
#' @return A diagram
pcner <- function(i, sliderplot, whwechat, zh){
  mymat <- whwechat
  if (nrow(mymat) == 0) {
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, 'No data available!', col = 'red', cex = 2)
  } else {
    datatime =  format(Sys.time(), format = '%Y-%m-%d %H:%M:%S %Z')
    par(mar = c(0,0,0,0))
    # layout(mat = matrix(c(1, 1, 2, 2, 3, 4, 5, 5), nrow = 4, byrow = TRUE), heights = c(2, 2, 20, 2))
    layout(mat = matrix(c(1:4), nrow = 4, byrow = TRUE), heights = c(2, 2, 20, 2))
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, zh$zh[zh$en =='Steem CN Wechat Hall'], col = 'steelblue', cex = 3)
    plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
    text(1, 1, datatime, cex = 2)
    colplot <- c('esp', 'rep','online', 'value')

    mymat <- mymat[!is.na(mymat[, colplot[i]]), ]
    wcplot <- mymat[rev(order(mymat[, colplot[i]])), ][sliderplot[1]: sliderplot[2], ]
    wordcloud::wordcloud(wcplot$name, freq = wcplot[, colplot[i]], colors = RColorBrewer::brewer.pal(4, "Dark2"), scale = c(4, 0.6))
    # legend('bottomleft', legend = paste0(fignr[i], ' ', sliderplot[[i]][1], '-', sliderplot[[i]][2]), bty = 'n', cex = 2)
  }
  #    mywc <- function(freq) {
  #   wcplot <- myfer[rev(order(myfer[, freq])), ][input$fr_slider[1]: input$fr_slider[2], ]
  #   wordcloud(wcplot$Account, freq = wcplot[, freq], colors = brewer.pal(4, "Dark2"), scale = c(4, 0.6))
  #   legend('top', legend = paste0(freq, ' Top 30'), bty = 'n', cex = 2)
  # }
  # lapply(c("Followers", "Posts", "vests"), mywc)
  plot(1, type = "n", axes = FALSE, xlab = "", ylab = "")
  text(1, 1, 'made by @dapeng', col = 'steelblue', cex = 2)
}

#' Plot a histogram of a vector.
#'
#' @param data A numeric vector to plot.
#' @param mybreaks Breaks of the plotted bars.
#' @param myxlim x limit.
#' @param myylim y limit.
#' @param eightlines Whether to draw the eight lines.
#' @param eightdigit The digit number of the figures displayed on the diagram.
#' @param eightcex The character size of the figures.
#' @param eightcolors The colors of the eight lines.
#' @param mylegend The legend.
#' @param myxlab The x label.
#' @param return_df Whether return the data frame of summary.
#' @param myfreq Whether display the frequency,
#' @param show_n Whether show the sample number.
#' @param show_skewness Whether show the skewness test.
#' @param show_density Whether show the density.
#' @param show_normline Whether show the line of the normal distribution.
#' @param show_mean Whether show the mean value.
#'
#' @return A histogram diagram.
#' @export
#'
#'
phist <- function(data = rnorm(1000),
                  mybreaks = "Sturges",
                  myxlim = NULL,
                  myylim = NULL,
                  eightlines = TRUE,
                  eightdigit = 0,
                  eightcex = 0.8,
                  eightcolors = c('red','darkgreen','blue', 'black', 'purple', 'gold')[c(1,2,3,2,1,6,6,5,4,5)],
                  mylegend = '',
                  myxlab = '',
                  return_df = FALSE,
                  myfreq = FALSE,
                  show_n = TRUE,
                  show_skewness = TRUE,
                  show_density = FALSE,
                  show_normline = FALSE,
                  show_mean = FALSE) {

  # plot the hist
  oldpar <- par(mar = c(4, 4, 0.5, 1), las = 1)
  if (is.null(myxlim)) {
    hist(data,
         col = 'grey',
         border = NA,
         main = '',
         freq = myfreq,
         breaks = mybreaks,
         xlab = myxlab,
         ylim = myylim)
  } else {
    hist(data,
         col = 'grey',
         border = NA,
         main = '',
         freq = myfreq,
         breaks = mybreaks,
         xlab = myxlab,
         xlim = myxlim,
         ylim = myylim)
  }

  # add the density line
  if (show_density) {
    lines(density(data[!is.na(data)], bw = "SJ"))
  }

  # add the normal distribution line
  if (show_normline) {
    xnormline <- dnorm(x = data,
                       mean = mean(data, na.rm = TRUE),
                       sd(data, na.rm = TRUE))
    curve(xnormline,
          add = TRUE,
          col = 'purple')
  }

  # add the rug line
  rug(data, col = 'darkgrey')

  # add the legend
  legend('top', bty = 'n', legend = mylegend)
  myskew <- skewness(data)
  mylegend <- paste(ifelse(show_n,
                           paste0('n = ', sum(!is.na(data)), '\n'),
                           ''),
                    ifelse(show_mean,
                           paste0('mean = ', round(mean(data, na.rm = TRUE), 0),
                                  '\n'), ''),
                    ifelse(show_skewness,
                           paste0('skewness = ', round(myskew, 2), ifelse(myskew > 1.96 | myskew < -1.96, '', '(*)')),
                           ''),
                    sep = '')
  legend('right', bty = 'n',legend = mylegend)

  # add the eight lines
  if (eightlines) {
    myfive <- fivenum(data)
    threshold <- IQR(data, na.rm = TRUE) * 1.5
    mtext(text = round(myfive, eightdigit),
          side = 3,
          line = c(-1.2, -2.2, -1.7, -2.2, -1.2),
          at = myfive,
          col = eightcolors[1:5],
          cex = eightcex)
    axis(3,
         at = myfive[c(1, 3, 5)],
         col.ticks = eightcolors[c(1, 3, 5)],
         labels = rep('', 3),
         tck = 0.02)
    axis(3,
         at = myfive[3],
         col.ticks = eightcolors[3],
         labels = '',
         tck = 0.05)
    axis(3,
         at = myfive[c(2, 4)],
         col.ticks = eightcolors[c(2, 4)],
         labels = rep('', 2),
         tck = 0.08)
    mymean <- mean(data, na.rm = TRUE)
    mysd <- sd(data, na.rm = TRUE)
  }
  box()
  par(oldpar)

  # return the summary data frame
  if (return_df) {
    mydf <- data.frame(para = c('min', '1q', 'median', '3q', 'max',
                                'lower', 'upper', 'mean', 'sd'),
                       value =c(myfive, myfive[2] - threshold,
                                myfive[4] + threshold, mymean, mysd))
    return(mydf)
  }
}


#' UI for the Shiny app scner display and analysis
#'
#' @return A UI function
scner_ui <- function(){


  fluidPage(
    sidebarLayout(
      sidebarPanel(
        ### Brief instruction
        h2("Steem CNers"),
        h4("Features: "),
        "To display the statistics of the Steem CN Community.",
        hr(),
		### wordclouds for the CN members according to the SP, Rep, online days, or account value.
        wellPanel(
          plotOutput('wh_plot1'),
          p(),
          sliderInput("wh_slider_sp",
                      "Rank in Wechat Group (Effective Steem Power)",
                      min = 1,
                      max = 108,
                      value = c(1, 36)
          ),
          p(),
          hr(),
          plotOutput('wh_plot2'),
          p(),
          sliderInput("wh_slider_rep",
                      "Rank in Wechat Group (Reputation)",
                      min = 1,
                      max = 108,
                      value = c(1, 36)
          ),
          p(),
          hr(),
          plotOutput('wh_plot3'),
          p(),
          sliderInput("wh_slider_online",
                      "Rank in Wechat Group (Online)",
                      min = 1,
                      max = 108,
                      value = c(1, 36)
          ),
          p(),
          hr(),
          plotOutput('wh_plot4'),
          p(),
          sliderInput("wh_slider_value",
                      "Rank in Wechat Group (ID Value)",
                      min = 1,
                      max = 108,
                      value = c(1, 36)

          )),

### histograms of the distribution according to ESP, SP, Rep, online days, and account value.
        plotOutput('wh_phist_esp'),
        plotOutput('wh_phist_sp'),
        plotOutput('wh_phist_rep'),
        plotOutput('wh_phist_online'),
        plotOutput('wh_phist_value')
      ),

      mainPanel(
        ### query an ID
        wellPanel(
          tags$script(' $(document).on("keydown", function (e) {
                      Shiny.onInputChange("lastkeypresscode", e.keyCode);
});
                      '),
          textInput('wh_id', label = '', value = 'dapeng'),
          # actionButton('wh_go', 'Go!'),
          # verbatimTextOutput("wh_summary")
          tableOutput("wh_summary")
        ),

        ### Data frame for the CN member list.
        wellPanel(
          h4("Name list:"),
          hr(),
          checkboxGroupInput(
            inputId = "colsel",
            label   = "Choose the columns to display:",
            choices = c('Intro', 'Reputation', 'Steem Power','ESP', 'Online', 'steem','sbd','VP','Account Value', 'Level', 'Character'), #'level',
            selected = c('Intro', 'Reputation', 'Steem Power','ESP', 'VP', 'Level', 'Character'),
            inline = TRUE
          ),
          hr(),
          radioButtons(
            inputId = "N",
            label   = "Rank in:",
            choices = c('Reputation', 'Steem Power','ESP', 'Online', 'Account Value'),
            selected = 'ESP',
            inline = TRUE
          ),
          hr(),

          dataTableOutput('wh_dt')
        )
        ),
      position = "right"
    )
  )
}


scner_server <- function(input, output, session) {


  ### prepare the data
  whwechat <- gcner()
  hero108 <- readLines(system.file(paste0('dict/108.csv'),
                                   package = 'steemr'),
                       encoding = 'UTF-8')
  zhLines <- readLines(system.file(paste0('dict/zh.csv'),
                                   package = 'steemr'),
                       encoding = 'UTF-8')
  zh <- data.frame(matrix(unlist(strsplit(zhLines, ',')),
                          ncol = 2,
                          byrow = TRUE),
                   stringsAsFactors = FALSE)
  names(zh) <- c('en', 'zh')

  slmax <- nrow(whwechat)
  ### update the sliders for the word clouds
  # Control the value, min, max, and step.
  # Step size is 2 when input value is even; 1 when value is odd.
  updateSliderInput(session,
                    "wh_slider_sp",
                    value = c(1, ifelse(slmax > 36, 36, slmax)),
                    min = 1,
                    max = slmax,
                    step = 1)
  updateSliderInput(session,
                    "wh_slider_rep",
                    value = c(1, ifelse(slmax > 36, 36, slmax)),
                    min = 1,
                    max = slmax,
                    step = 1)
  updateSliderInput(session,
                    "wh_slider_online",
                    value = c(1, ifelse(slmax > 36, 36, slmax)),
                    min = 1,
                    max = slmax,
                    step = 1)
  updateSliderInput(session,
                    "wh_slider_value",
                    value = c(1, ifelse(slmax > 36, 36, slmax)),
                    min = 1,
                    max = slmax,
                    step = 1)

  observe({
    ### Get the ID to query
    if(!is.null(input$lastkeypresscode)) {
      if(input$lastkeypresscode == 13){
        if (is.null(input$wh_id) || input$wh_id == "") return()

        mywhid <- input$wh_id

        ### query the ID information
        if (mywhid %in% whwechat$name){
          nr <- slmax
          pc <- c('rep', 'sp', 'esp', 'online', 'value')
          pczh <- zh$zh[match(c('Reputation', 'Steem Power', 'ESP', 'Online', 'Account Value'), zh$en)]
          mywhidinfor <- data.frame(Ref = pczh)
          mywhidinfor$value <- unlist(whwechat[whwechat$name == mywhid, pc])
          mywhidinfor[2:5, 'value'] <- as.character(round(mywhidinfor[2:5, 'value'], 0))
          # mywhidinfor$rank <- sapply(pc, function(x) which(mywhid == whwechat$name[rev(order(whwechat[, x]))]))
          mywhidinfor$rank <- sapply(pc,
                                     function(x) {
                                       floor(rank(-whwechat[, x], ties.method = 'random')[mywhid == whwechat$name])
                                     }
          )
          mywhidinfor$hero <- hero108[mywhidinfor$rank]
          nb1 <- mywhidinfor$rank - 1
          nb1 <- ifelse(nb1 < 1, NA, nb1)
          nb2 <- mywhidinfor$rank + 1
          nb2 <- ifelse(nb2 > nr, NA, nb2)
          nb <- NULL
          for (i in 1:length(pc)) {
            newid <- whwechat$name[rev(order(whwechat[, pc[i]]))]
            nb11 <- ifelse(is.na(nb1[i]),
                           '',
                           paste0('@', newid[nb1[i]]))
            nb22 <- ifelse(is.na(nb2[i]),
                           '',
                           paste0('@', newid[nb2[i]]))
            nb <- c(nb, paste(nb11, nb22))
          }
          mywhidinfor$neighbors <- nb
          mywhidinfor$rank <- gsub('.00', '', mywhidinfor$rank)
          names(mywhidinfor) <- zh$zh[match(c('Parameter', 'Value', 'Rank' ,'Character', 'Neighbor'), zh$en)]
        } else {
          mywhidinfor <- paste0('@', mywhid, zh$zh[match(c('not there.'), zh$en)])
        }

        ### create the report of the ID information
        output$wh_summary = renderTable({
          mywhidinfor
        })
      }
    }
  })

  ### create the data frame of the CNers.
  output$wh_dt = renderDataTable({
    # mycol <- c("N", "id", 'rep', 'sp', 'esp', 'online','steem','sbd','vp','value','name')
    mymat <- whwechat
    mymat <- mymat[, -ncol(mymat)]
    mymat[, c("sp", "esp", "steem", "sbd", "value", "online", 'vp')] <- round(mymat[, c("sp", "esp", "steem", "sbd", "value", "online",'vp')], 0)
    mymat$hero <- hero108[1:slmax]
    whalelevel <- readLines(system.file(paste0('dict/zh_level.txt'),
                                        package = 'steemr'),
                            encoding = 'UTF-8')
    mymat$level <- whalelevel[mymat$level]

    names(mymat) <- c('Rank','ID', 'Reputation', 'Steem Power', "ESP", 'Online', 'steem','sbd','VP','Account Value', 'Level', 'Intro','Character') #'level',
    mymat[, 'Rank'] <- floor(rank(-mymat[, input$N]))
    mymat <- mymat[order(mymat[, 'Rank']), ]
    mymatout <- mymat[, c('Rank', 'ID', input$colsel)]
    names(mymatout) <- zh$zh[match(names(mymatout), zh$en)]
    mymatout
  },
  options = list(lengthMenu = c(36, 72, 108, nrow(whwechat)),
                 pageLength = 108),
  escape = FALSE
  )

### create the wordcloud diagrams
  output$wh_plot1 <- renderPlot({
    pcner(1,
          whwechat = whwechat,
          input$wh_slider_sp,
          zh = zh)
  })

  output$wh_plot2 <- renderPlot({
    pcner(2,
          whwechat = whwechat,
          input$wh_slider_rep,
          zh = zh)
  })

  output$wh_plot3 <- renderPlot({
    pcner(3,
          whwechat = whwechat,
          input$wh_slider_online,
          zh = zh)
  })
  output$wh_plot4 <- renderPlot({
    pcner(4,
          whwechat = whwechat,
          input$wh_slider_value,
          zh = zh)
  })

  ### create the histograms of the distributions
  output$wh_phist_rep <- renderPlot({
    phist(whwechat$rep,
          show_density = TRUE,
          myxlab = 'Reputation')
  })
  output$wh_phist_online <- renderPlot({
    phist(whwechat$online,
          show_density = TRUE,
          myxlab = 'Online Days',
          myylim = c(0, 0.015))
  })
  output$wh_phist_sp <- renderPlot({
    phist(whwechat$sp,
          show_density = TRUE,
          myxlab = 'Steem Power')
  })
  output$wh_phist_esp <- renderPlot({
    phist(whwechat$esp,
          show_density = TRUE,
          myxlab = 'Effective Steem Power',
          myylim = c(0, 0.001))
  })
  output$wh_phist_value <- renderPlot({
    phist(whwechat$value,
          show_density = TRUE,
          myxlab = 'Account Value',
          myylim = c(0, 0.002))
  })

}

#' A shiny app to display and anaylize the CNers.
#' scner means shiny app for CNers.
#' @return a shinyapp which can be displayed in a web browser.
#' @export
scner <- function(){
  return(shiny::shinyApp(ui = scner_ui, server = scner_server))
}



#' UI for the Shiny app sposts display and analysis
#'
#' @return A UI function
sposts_ui <- function(){


  fluidPage(
    sidebarLayout(
      mainPanel(
        h4("Input a Steem ID and Enter to display all his/her posts."),
        wellPanel(
	  ### Get an ID of a Steemian
          h4("Steem ID:"),
          tags$script(' $(document).on("keydown", function (e) {
                      Shiny.onInputChange("lastkeypresscode2", e.keyCode);
});
                      '),
          textInput('ps_id', label = '', value = 'dapeng'),
		  ### Display the posts of the given ID
          dataTableOutput('ps_dt')
        )
        ),

      sidebarPanel(
        wellPanel(
		### Display the growth of the given ID.
          h4("Cumulative Payouts Growth"),
          plotOutput('ps_plot2', height = 200),
          hr(),
          h4("Cumulative Votes Growth"),
          plotOutput('ps_plot1', height = 200),
          hr(),
          h4("Cumulative Posts Growth"),
          plotOutput('ps_plot3', height = 200),
          hr(),
		### Display the statistics of the given ID.
          h4("Distribution of post payout"),
          'blue: median, red: range, green: 1- and 3-quantiles.',
          p(),
          plotOutput('ps_plot5', height = 300),
          hr(),
          h4("Distribution of post votes"),
          'blue: median, red: range, green: 1- and 3-quantiles.',
          plotOutput('ps_plot6', height = 300),
          hr(),
          h4("Active hours (UTC)"),
          plotOutput('ps_plot4')
        )
      ),
      position = "left"
    )
  )
}


#' Server for the Shiny app sposts display and analysis
#'
#' @param input The input of the server.
#' @param output The output of the server.
#' @param session The session of the server.
sposts_server <- function(input, output, session) {


### Get the ID and query the posts.
  observe({
    if(!is.null(input$lastkeypresscode2)) {
      if(input$lastkeypresscode2 == 13){
        if (is.null(input$ps_id) || input$ps_id == "") return()
        mypsid <- tolower(gsub('@', '', input$ps_id))
        myposts <-  gidposts(id = mypsid, method = 'appbase_api')
### Process the data of the posts
        if (nrow(myposts) == 0){
          output$ps_dt = renderDataTable({
            data.frame(message = paste0('No post by @', mypsid))
          },
          options = list(lengthMenu = 1),
          escape = FALSE
          )
        } else {
          myposts <- myposts[order(myposts$datetime), ]
          myposts$title <- paste0('<a href="https://steemit.com/',
                                  myposts$category,
                                  '/@', mypsid, '/',
                                  myposts$permlink, '">',
                                  myposts$title , '</a>')
          sites <- c('cnsteem.com', 'busy.org', 'steemdb.com', 'steemd.com')
          sitesshort <- sapply(sites, function(x) strsplit(x, '\\.')[[1]][1])
          for (i in 1:length(sites)) {
            myposts[, paste0('site ', i)] <- paste0('<a href="https://',
                                                    sites[i], '/',
                                                    myposts$category,
                                                    '/@', mypsid, '/',
                                                    myposts$permlink, '">',
                                                    sitesshort[i] , '</a>')
          }
          names(myposts)[names(myposts) == 'total_payout'] <- 'payout'
          output$ps_dt = renderDataTable({
            myposts[, c("datetime", "title", "payout", "votes", "comments", "category", "tags", "site 1", "site 2", "site 3", "site 4")]
          },
          options = list(lengthMenu = c(10, 20, 50, 100, nrow(myposts)),
                         # width = '100px',
                         # targets = c(3:8),
                         pageLength = 10),
          escape = FALSE
          )
          myposts$cum_posts <- 1:nrow(myposts)
          myposts$cum_payout <- cumsum(myposts$payout)
          myposts$cum_votes <- cumsum(myposts$votes)
          # myposts$rep <- repcalc(myposts$author_reputation)
          myxlim <- range(myposts$date)

		  ### create the scatter plots for the time series
          output$ps_plot1 <- renderPlot({
            pdate(myposts$date,
                  myposts$cum_votes,
                  mylegend = paste0('Votes@', mypsid),
                  myxlim = myxlim,
                  mycol = 'blue')
          })
          output$ps_plot2 <- renderPlot({
            pdate(myposts$date,
                  myposts$cum_payout,
                  mylegend = paste0('Payout(SBD)@', mypsid),
                  myxlim = myxlim,
                  mycol = 'red')
          })
          output$ps_plot3 <- renderPlot({
            pdate(myposts$date,
                  myposts$cum_posts,
                  mylegend = paste0('Posts@', mypsid),
                  myxlim = myxlim)
          })

		  ### create the hour rose of the active time
          output$ps_plot4 <- renderPlot({
            phour(myposts, col_time = 'datetime')
          })

### create the histograms of the distributions
          output$ps_plot5 <- renderPlot({
            phist(myposts$payout,
                  myxlab = 'Payout of a post (SBD)',
                  show_skewness = FALSE,
                  show_mean = TRUE,
                  myfreq = TRUE)
          })
          output$ps_plot6 <- renderPlot({
            phist(myposts$votes,
                  myxlab = 'Votes of a post',
                  show_skewness = FALSE,
                  show_mean = TRUE,
                  myfreq = TRUE)
          })
        }
      }
    }
  })
}


#' Calculate the reputation of an ID
#'
#' @param rep A numeric value of the raw reputation.
#'
#' @return A numeric value of the real reputation
repcalc <- function(rep){
  if (rep > -1000000000 & rep < 1000000000) return(25)
  log10(as.numeric(rep)) * 9 - 56
}

#' A shiny app to display and anaylize the posts of a given ID.
#' sposts means shiny app for posts.
#'
#' @return a shinyapp which can be displayed in a web browser.
#' @export
sposts <- function(){
  return(shiny::shinyApp(ui = sposts_ui, server = sposts_server))
}

