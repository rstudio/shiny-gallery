---
layout:       app-showcase
title:        "systemPipeShiny"
user_name:    Le Zhang
user_url:     https://github.com/lz100
date:         2020-07-19
tags:         package, workflow management, bioinformatics, data visualization
app_url:      https://tgirke.shinyapps.io/systemPipeShiny/
source_url:   https://github.com/rstudio/shiny-gallery/systemPipeShiny
rscloud_url:  https://rstudio.cloud/project/2493103
rscomm_url:   https://community.rstudio.com/u/lz100
contest:      yes
contest-year: 2021
thumbnail:    systemPipeShiny.png
---

## Key features

- Three powerful default modules:
    - [Workflow](https://systempipe.org/sps/modules/workflow/):  use [systemPipeR](https://systempipe.org/sp/) (SPR) as backend to interactively generate, design, and run workflows. Current templates are biological-centric, but as a workflow environment, SPR can adapt to any type of data analysis workflow.
    - [RNAseq](https://systempipe.org/sps/modules/rnaseq/):  Interactive conduct downstream analysis of RNA sequencing raw counts, including different ways of data normalization, deferentially expressed gene analysis, and more than 10 types of clustering or summary plotting methods. 
    - [Quick ggplot](https://systempipe.org/sps/modules/ggplot/): To make a ggplot within a few mouse clicks with any kind of tabular data you upload. 
    - More modules in the future...
- [Extendable](https://systempipe.org/sps/adv_features/tabs/): users are able to add their own components to the dashboard as individual "tabs". 
    - Different templates that satisfy both beginners and advanced developers
        - Simple: high-level wrappers, you only need to focus on plotting code, no Shiny development knowledge is required.
        - Full: full Shiny code, for advanced users, customize everything of the tab.
    - Modularization: the whole app is built on top of Shiny modules, which gives it the flexibility to add new content without concerning conflicting with any existing content. Each "tab" on the dashboard is isolated with its own environment. 
- [Canvas](https://systempipe.org/sps/canvas/): A workbench for interactive quick image editing
    - Communicates with all plot options on other tabs, take screenshots of them and send to this tool for further image editing.
    - Combine/compare different plots, add annotations and make a scientific figure.
    - Drag to upload your own images. 
- Fully customizable: change almost everything of the default app
    - [Load/unload certain tabs](https://systempipe.org/sps/adv_features/displaytabs/)
    - [Overwrite default tabs](https://systempipe.org/sps/adv_features/overwritetabs/)
    - [App title/ logo](https://systempipe.org/sps/adv_features/other_customizations/)
    - and more...
- [Security and admin](https://systempipe.org/sps/adv_features/login/):
    - Options to turn on login feature with interactive loading screens that users can play with
    - User-defined secret URL for the admin page
        - View real-time app analytics
        - Control app accounts interactively
- [Notification](https://systempipe.org/sps/adv_features/notification/): a simple way to broadcast messages to your users without redeploying the app every time. 
- [User defined interactive tutorials](https://systempipe.org/sps/adv_features/guide/): Easy-to-user methods and templates to create your own interactive tutorials
- [Detailed debugging](https://systempipe.org/sps/adv_features/debug/): options to turn on verbose logging and traceback messages.
    - [Duel-end logging](https://systempipe.org/sps/dev/spscomps/server/#shinycatch): Exceptions handling with logging on both user-end and server-end. 
- [Deploy-ready](https://systempipe.org/sps/deploy/): When you initialize an SPS project, the app is deploy-ready. You can deploy it to Shiny servers like _shinyapps.io_ as soon as the project is created. 
- [Developer tools](https://systempipe.org/sps/dev/): If you see any good features from SPS, you can use them in your own apps. Core features of SPS have been split into smaller supporting packages {spsComps}, {drawer} and {spsUtil} for you to use in your own Shiny apps or Rmarkdowns.


This app was recognized _[as a runner up]_ on the 2021 Shiny Contest.
