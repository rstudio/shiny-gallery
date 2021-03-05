onclick(
  "chapter_sel",
  showModal(
    tags$div(
      id = "modal",
      modalDialog(
        # make it closable by pressing anywhere around it
        easyClose = TRUE,
        title = "Click on the chapter you want to go to",
        includeHTML("html_files/selector.html")
      )
    )
  )
)