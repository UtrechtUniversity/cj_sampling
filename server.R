server <- function(input, output, session) {
  set.seed(11)
  library(ggplot2)
  circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
    r = diameter / 2
    tt <- seq(0,2*pi,length.out = npoints)
    xx <- center[1] + r * cos(tt)
    yy <- center[2] + r * sin(tt)
    return(data.frame(x = xx, y = yy))
  }
  
  
  #15x10
  # 600 x 400
  
  # Cases
  init_app <- function(){
    x <- as.vector(replicate(10, sample.int(15, 10))) + rnorm(100, sd = .2)
    y <- rep(1:10, each = 10) + rnorm(100, sd = .2)
    shape <- factor(sapply(x, function(xes){
      ps <- .1 + .8*(xes/max(x))
      ps <- c(ps, 1-ps)
      sample.int(2, 1, replace = TRUE, prob = ps)
    }))
    
    fill <- factor(sapply(y, function(ys){
      ps <- c((.05 + .5*(ys/max(y))), (.4*(ys/max(y))))
      ps <- c(ps, rep((1-sum(ps))/3, 3))
      sample.int(5, 1, replace = TRUE, prob = ps)
    }))

    data <- data.frame(x=x, y=y, shape = shape, fill = fill)
    
    # Stick figure
    rx <- sample.int(15, 1)
    ry <- sample.int(10, 1)
    man_data <- data.frame(x = rx+c(0, 0, -.3, -.05, -.3, 0, .1, .35, .1, 0, 0, -.15, 0, .2), y = ry+1*c(0, -.2, -.4, -.6, -.4, -.2, -.4, -.6, -.4, -.2, -.6, -1.1, -.6, -1.1))
    
    
    p <- ggplot(data, aes(x, y)) + 
      geom_point(size = 7, stroke = 1, aes(shape = shape, fill = fill)) + 
      scale_fill_manual(values = c("red", "green", "yellow", "#990000", "#ff6600"), guide = FALSE) +
      scale_shape_manual(values = c(21, 22), guide = FALSE)+
      theme(axis.text.y   = element_blank(),
            axis.text.x   = element_blank(),
            axis.title.y  = element_blank(),
            axis.title.x  = element_blank(),
            axis.ticks = element_blank(),
            panel.background = element_blank(),
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(),
            axis.line = element_line(colour = "black"),
            panel.border = element_rect(colour = "black", fill=NA, size=5)
            
      ) +
      geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
      geom_path(data = man_data, size = 1) + 
      scale_x_continuous(limits = c(min(x), max(x))) +
      scale_y_continuous(limits = c(min(y), max(y)))
      output$plot1 <- renderPlot({
        p
      })
    assign("x", x, .GlobalEnv)
    assign("y", y, .GlobalEnv)
    assign("rx", rx, .GlobalEnv)
    assign("ry", ry, .GlobalEnv)
    assign("data", data, .GlobalEnv)
    assign("man_data", man_data, .GlobalEnv)
    assign("p", p, .GlobalEnv)
  }
  
  init_app()
  
  observeEvent(input$reset, {
    init_app()
  })
  
  observeEvent(input$do_sample, {
    switch(input$sample_type,
           "Convenience" = {
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             sample_dat <- data[order(distances)[1:input$n], ]
             sample_plot <- p +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill)) +
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)+
               geom_path(data = circleFun(c(rx,ry), 2*distances[order(distances)[input$n]], npoints = 40), linetype = 2)
             },
           "Snowball" = {
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             snowball <- rep((as.numeric(names(which.min(distances)))-1), input$n)
             prev_x <- x[snowball[1]]
             prev_y <- y[snowball[1]]
             for(i in 2:input$n){
               snowball[i] <- as.numeric(names(which.min(as.matrix(dist(matrix(c(prev_x, x, prev_y, y), ncol = 2)))[-1,1][-unique(snowball)])))-1
               prev_x <- x[snowball[i]]
               prev_y <- y[snowball[i]]
             }
             sample_dat <- data[snowball, ]
             sample_plot <- p + 
               geom_path(data = rbind(c(rx, ry, NA, NA), sample_dat), linetype = 2) +
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill))+
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)
           },
           "Purposive" = {
             conv_percent <- .8
             n_conv <- 1:(input$n*conv_percent)
             n_purposive <- (round(input$n*conv_percent)+1):input$n
             distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
             sample_dat <- data[order(distances)[1:input$n], ]
             sample_dat$colour <- "a"
             sample_dat[n_purposive, ] <- NA
             purposive <- c(order(distances)[n_conv], rep(NA, length(n_purposive)))
             for(i in n_purposive){
               sample_cells <- table(sample_dat[ , c(3,4), drop = F])
               available_combinations <- which(sample_cells < table(data[-na.omit(purposive), c(3,4), drop = F]))
               select_from_these <- which(sample_cells == min(sample_cells[available_combinations]))
               select_from_these <- available_combinations[available_combinations %in% select_from_these]
               more_of_this_cell <- sample(select_from_these, 1)
               more_of_this_fill <- ceiling(more_of_this_cell/2)
               more_of_this_shape <- ifelse(more_of_this_cell%%2 == 0, 2, 1)
               distances <- as.matrix(dist(matrix(c(rx, x, ry, y), ncol = 2)))[-1,1]
               purposive[i] <- as.numeric(names(which.min(distances[data$shape == more_of_this_shape & data$fill == more_of_this_fill & !(names(distances) %in% as.character(purposive+1))])))-1
               sample_dat[i, ] <- c(data[purposive[i], ], "b")
             }
             distances <- as.matrix(dist(matrix(c(rx, sample_dat$x, ry, sample_dat$y), ncol = 2)))[-1,1]
             sample_plot <- p + 
               geom_point(data = sample_dat, size = 7, stroke = 2, aes(shape = shape, fill = fill, colour = colour)) + 
               scale_color_manual(values = c("black", "purple"), guide = FALSE) + 
               geom_path(data = circleFun(c(rx,ry), 2*distances[order(distances)[input$n]], npoints = 40), linetype = 2) +
               geom_point(x = rx, y = ry, fill = "black", shape = 21, size = 4) + 
               geom_path(data = man_data, size = 1)

           })
    
    output$plot1 <- renderPlot({
      sample_plot
    })
  })


}

