## Step 0: Install Dash and dependencies if needed

# install.packages("remotes")
# remotes::install_github("plotly/dashR", upgrade = "always")

library(dash)
library(plotly)
library(tidyr)
library(dplyr)
library(Robyn)

# Change this to your Robyn outputs folder
OutputFolder <- "~/Desktop/Robyn_202211051523_init/"

# Change this to the input CSV's name for "Business as usual" days.
# The demo CSV uses "na" by default
BAU <- "na"

# Load InputCollect
InputCollect <- robyn_read(paste(OutputFolder,"RobynModel-inputs.json", sep = ""))$InputCollect

# Load source data
dt_campaigns <- read.csv("~/Desktop/dt_campaigns.csv") %>%
  rename(ds = InputCollect$date_var)

dt_meanspend <- dt_campaigns %>%
  select(InputCollect$paid_media_spends) %>%
  summarise_all(sum)

# Load source data
dt_models <- read.csv(paste(OutputFolder,"pareto_alldecomp_matrix.csv", sep = ""), na.strings="")

dt_roi <- dt_models %>%
  select(solID, InputCollect$paid_media_spends) %>%
  group_by(solID) %>%
  summarise_all(sum)

# load models and metrics
dt_clusters <- read.csv(paste(OutputFolder,"pareto_clusters.csv", sep = ""), na.strings="")

dt_top = dt_clusters %>%
  filter(top_sol == TRUE)

# Create a Dash app
app <- dash_app()

# Set the layout of the app
app %>% set_layout(
  
  # Headers
  h1('Robyn Dash'),
  div("A dashboard for evaluating Robyn models, and campaign impact on media spend."),
  
  # Graph 1: Model Performance
  h2('Model Performance'),
  div(
    dccRadioItems(id="topsol", options=list("Top Solutions", "All Solutions"), value="Top Solutions"),
    dccGraph(id="clusters")
  ),
  
  # Graph 2: Channel Response Comparison
  h2('Channel Response'),
  div(
    dccDropdown(id="model_dropdown", options=dt_clusters$solID, value=dt_top$solID, multi=TRUE),
    dccGraph(id="channel_response")
  ),
  
  # Graph 3: Marketing Activity
  h2('Marketing Activity'),
  div(
    dccGraph(id="marketing_activity")
  )
)

# Callback 1: Model Performance
app %>% add_callback(
  output(id="clusters", property="figure"),
  input(id="topsol", property="value"),
  function(filter_top_solutions)
  {
    dt_select <- dt_clusters
    
    if(filter_top_solutions=="Top Solutions")
    {
      dt_select <- dt_top
    }
    
    dt_select = dt_select %>% mutate(cluster = paste("Cluster", cluster))
    
    fig = plot_ly(
      data = dt_select,
      x = dt_select$nrmse,
      y = dt_select$decomp.rssd,
      color = dt_select$cluster,
      type = "scatter",
      mode = "markers",
      hovertemplate = paste(
        "<b>",dt_select$solID,"</b><br><br>",
        "%{yaxis.title.text}: %{y:.000}<br>",
        "%{xaxis.title.text}: %{x:.000}<br>"
      )
    ) %>% layout(
      xaxis = list(title="NRMSE"),
      yaxis = list(title="decomp.RSSD")
    ) %>% hide_colorbar()
  }
)

# Callback 2: Channel Response Comparison
app %>% add_callback(
  output(id="channel_response", property="figure"),
  input(id="model_dropdown", property="value"),
  function(model_list)
  {
    dt_select = dt_roi  %>%
      filter(solID %in% model_list)
    paid_vars = InputCollect$paid_media_spends
    
    fig <- plot_ly(
      data = dt_select,
      x = dt_select$solID,
      y = dt_select[[paid_vars[1]]] / dt_meanspend[[paid_vars[1]]],
      name = paid_vars[1],
      hovertemplate = paste(
        "<b>",paid_vars[1],"</b><br><br>",
        "%{yaxis.title.text}: %{y}<br>"
      ),
      type = "bar"
    ) %>%
    layout(yaxis = list(title="ROI"))
    for (i in 2:length(paid_vars))
    {  
      fig <- fig %>% add_trace(
        y = dt_select[[paid_vars[i]]] / dt_meanspend[[paid_vars[1]]], 
        name = paid_vars[i],
        hovertemplate = paste(
          "<b>",paid_vars[i],"</b><br><br>",
          "%{yaxis.title.text}: %{y}<br>"
          )
        )
    }
    fig
  }
)

# Callback 3: Marketing Activity Reporting
app %>% add_callback(
  output(id="marketing_activity", property="figure"),
  input(id="model_dropdown", property="value"),
  function(model_list)
  {
    factor_var = InputCollect$factor_vars[1]
    # Get revenue data and context variable from dt_mod
    dt_selectfactor <- dt_campaigns %>%
      select(ds, factor_var) %>%
      rename(activity =  factor_var)
    
    # Very long pipe to get marketing activity effects from the selected models, map it
    # to dates from source data, then find out the effects of each activity and
    # pivot to a plottable format
    dt_select <- dt_models %>%
      filter(solID %in% model_list) %>%
      select(solID, ds, factor_var) %>%
      rename(effect = factor_var) %>%
      left_join(dt_selectfactor, by="ds") %>%
      group_by(solID, activity) %>%
      summarize(effect = mean(effect, na.rm = TRUE)) %>%
      mutate(effect = effect - effect[activity==BAU]) %>%
      filter(activity != BAU) %>%
      pivot_wider(names_from = activity, values_from = effect, values_fill = 0)
    
    activity_list <- colnames(dt_select)[-1]
    
    fig <- plot_ly(
      type = "bar",
      x = dt_select$solID,
      y = dt_select[[activity_list[1]]],
      name = activity_list[1],
      hovertemplate = paste(
        "<b>",activity_list[1],"</b><br><br>",
        "%{yaxis.title.text}: %{y}<br>"
      )
    ) %>%
    layout(yaxis = list(title="Effect"))
    for (i in 2:length(activity_list))
    {  
      fig <- fig %>% add_trace(
        y = dt_select[[activity_list[i]]], 
        name = activity_list[i],
        hovertemplate = paste(
          "<b>",activity_list[i],"</b><br><br>",
          "%{yaxis.title.text}: %{y}<br>"
          )
        )
    }
    fig
  }
)

# Run the app
app %>% run_app()