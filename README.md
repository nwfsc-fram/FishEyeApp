FISHEyE App
===


```
FisheyeApp
   |___ ui.R (ui for shiny app) 
   |___ server.R (server for shiny app)
   |___ runApp.R (a setup file and run file for other systems (e.g., Todd's laptop))
   |___ external (source for all the components of ui/server)
       |__ serverHead.R (things that are loaded into Shiny session when it is first initialized. Includes data and some factor mapping)
       |__ explorer
          |___ explorer.R (holds all of the final output objects to be rendered in ui for the user)
          |___ explorerSourceFiles (all of the scripts that feed explorer.R)
              |___ about.R (the about tabset pane)
              |___ defaultText.R (output objects to be displayed when output has not been rendered)
              |___ definitions.R (defiinitions tabset panel)
              |___ doPlot.R (the plotting logic for plot output)
              |___ ex.io.sidebar1.R (the widgets in the sidebar)
              |___ ex.reactives.R (The data processing logic to create output tables)
              |___ help.R (Some help text (depricated?))
              |___ instructions.R (instructions tabset panel)
```


css files:
---
- booststrap.css holds downloaded css
- fisheye.css contains custom css selectors