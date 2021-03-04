


library(ggplot2)
library(gganimate)
library(ggforce)
library(glue)
library(waiter)

server = function(input, output, session){
  
  Sys.sleep(2)
  waiter_hide()
  
  observeEvent(input$go_to_work, {
    updateNavbarPage(session = session, "navbar", "Work")
  }) 
  
  ## initialize loading screen for anim1
  w1 = Waiter$new(
    id = "anim1",
    html = tagList(
      div(
        spin_loaders(42, color = "#aaa"),
        div("Loading...",style = "color:#aaa;")
    )),
    color = "#272b30"
  )
  
  ## intialize loading screen for anim2
  w2 = Waiter$new(
    id = "anim2",
    html = tagList(
      div(
        spin_loaders(42, color = "#aaa"),
        div("Loading...", align = "left", style = "color:#aaa;")
      )),
    color = "#272b30"
  )
  
  observeEvent(input$run, {
    n = input$n
    d1 = the_data(n)
    disable(id = "run")

    output$anim1 = renderImage({
      
      
      
      w1$show()  # show the waiter screen
      
      if(n %in% pre_rendered) {
        filename <- normalizePath(
          file.path('./anims2', glue("anim1_{n}.gif"))  # get a pre-rendered image
        )
        Sys.sleep(1.5)  # to show the loading bar
        w1$hide()  # hide the waiter screen
        
        list(src = filename,
             contentType = 'image/gif'
        )
      }
      
      else{
        anim1 = make_anim1(d1)  # make the animation
        outfile <- tempfile(fileext='.gif')
        save_animation(anim1, outfile)
        
        w1$hide()
        
        
        list(src = outfile,
             contentType = 'image/gif'
        )
      }
      
      
    }, deleteFile = FALSE)
    
    ##############################################
    output$anim2 = renderImage({
      w2$show()
      
      if(n %in% pre_rendered) {
        filename <- normalizePath(
          file.path('./anims2', glue("anim2_{n}.gif"))
        )
        Sys.sleep(1.5)
        w2$hide()
        delay(12000, enable("run"))  # enable the action button when animation is done
        
        
        list(src = filename,
             contentType = 'image/gif'
        )
        
      }
      
      else {
        anim2 = make_anim2(d1)
        
        outfile <- tempfile(fileext='.gif')
        save_animation(anim2, outfile)
        
        w2$hide()
        delay(10000, enable("run"))  # enable the action button when animation is done
        list(src = outfile,
             contentType = 'image/gif'
        )
      }
    }, deleteFile = FALSE)
    
  })
}