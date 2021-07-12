# shark-attack

---

- Developed by: [Marcin Dubel](https://github.com/mdubel)
- Link to deployed app: https://mdubel.shinyapps.io/shark-attack/

---

# 2021 shiny-fluent Hackathon Appsilon/RStudio Contest SHARC!

## Goal
The ocean is the origin and the engine of all life on this planet — and it is under threat.
The game goal is to spread the awerness of the environmental issues via enjoyable way.
In the game the user (as a diver) will have one minute to collect as much trash as possible while avoiding sharks (on different difficulty levels).
The detailed tutorial is presented in the game main menu.

Learn more about the ocean pollution:
"https://www.conservation.org/stories/ocean-pollution-11-facts-you-need-to-know"
"https://www.noaa.gov/education/resource-collections/ocean-coasts/ocean-pollution"
"https://www.nrdc.org/stories/ocean-pollution-dirty-facts"
"http://www.unesco.org/new/en/natural-sciences/ioc-oceans/focus-areas/rio-20-ocean/blueprint-for-the-future-we-want/marine-pollution/"
"https://www.nationalgeographic.com/science/article/150109-oceans-plastic-sea-trash-science-marine-debris"

## Technical details
The app structure is based on [`shiny.fluent` framework](https://demo.appsilon.com/apps/fluentui/#!/) alongside the [`wahani/modules`](https://github.com/wahani/modules) and the [`R6 classes`](https://adv-r.hadley.nz/r6.html).
You can recreate the application environment locally with the `renv::restore` command.

## UI
Application UI is a single screen view, with additional modules for presenting game start and game end modals. The main view is build on top of css grid functionality. The objects displayed on the UI are the background-images for each single grid cell.
Moving with the diver on the grid is based on the JavaScript key-capture. Other moving objects are based on randomly generated direction with the JS interval.