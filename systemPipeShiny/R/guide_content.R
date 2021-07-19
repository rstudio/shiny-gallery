#################### Define your custom SPS tutorials ##########################
# use `shinydashboardPlus::messageItem` to add your tutorials UI to this list
guide_ui <- try(list(
    ## An example is provided below
    shinydashboardPlus::messageItem(
        inputId = "guide_main",
        from = "Main Guide",
        icon = icon('home'),
        message = "Brief introduction"
    )
))

# use `cicerone::Cicerone$new()` to add your tutorials content to this list
# See help `?cicerone::Cicerone`
# A named list, each item's name must match the `inputId` in UI to trigger it in app.
guide_content <- try(list(
    ## An example is provided below, replace or add your own to the list
    guide_main = cicerone::Cicerone$new(overlay_click_next = TRUE)$
        step(el = "sidebarItemExpanded",
             title = "SPS tabs",
             description = "Browse SPS functionalities as tabs from the left",
             position = "right-center")$
        step("sidebarItemExpanded .treeview a[href='#shiny-tab-module_main']",
             "Default modules",
             "Click here to go to the SPS modules main tab. Here you can see
             what different modules are.",
             "right-center")$
        step("sidebarItemExpanded .treeview a[href='#shiny-tab-vs_main']",
             "Custom visualization tabs",
             "Click here for the your custom visualization tabs.",
             "right-center")$
        step("sidebarItemExpanded a[href='#shiny-tab-core_canvas']",
             "Canvas",
             "When you have made some plots from the other tabs and click the
              'To Canvas' button, a screenshot of the plot will be sent to
             this tab for further image editing. You can drag to upload your own
             images as well.",
             "right-center")$
        step("sidebarItemExpanded a[href='#shiny-tab-core_about']",
             "About",
             "Take a look at the change log, get a link of the manual and other
             information here.",
             "right-center")$
        step(".main-header .notifications-menu",
             is_id = FALSE,
             "Notifications",
             "We will send out notifications to users when there is change or update.
             You can also customize the notifications.",
             "left")
), silent = TRUE) # Continue with errors here, SPS checks errors later
