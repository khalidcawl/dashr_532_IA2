library(dash)
library(dashBootstrapComponents)
library(dashCoreComponents)
library(ggplot2)
library(plotly)
library(purrr)

app <- Dash$new(external_stylesheets = dbcThemes$BOOTSTRAP)

df <- readr::read_csv(here::here('data', 'vgsales.csv'))

app$layout(
    dbcContainer(
        list(
            dccGraph(id='plot-area'),
            dccDropdown(
                id='sport-select',
                options = seq(1, 10, 1) %>%
                    purrr::map(function(col) list(label = col, value = col)), 
                value=20)
        )
    )
)

app$callback(
    output('plot-area', 'figure'),
    list(input('sport-select', 'value')),
    function(topN) {
        df_filtered <- df |> 
            select(Publisher) |> 
            group_by(Publisher) |>
            summarise(count = n()) |>
            arrange(desc(count)) |>
            head(topN)
        
        p <- ggplot(df_filtered, aes(y = reorder(Publisher, count), x = count))+ 
            geom_bar(stat="identity") +
            ggthemes::scale_color_tableau() +
            labs(x = "Release Count", y = "Publisher",
                 title = "Top publishers by release")
        
        ggplotly(p)
        
    }
)

app$run_server(debug = T)

app$run_server(host = '0.0.0.0')