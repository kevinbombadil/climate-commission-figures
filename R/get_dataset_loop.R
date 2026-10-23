
library(crosstalk)
library(shiny)
library(tibble)
library(dplyr)
library(purrr)   # functions for functions and vectors
library(plotly) # 1. Load the plotly library
library(readxl)
library(stringr)
library(tidyr)
library(cellranger)

YAML_PATH <- './data/test_techannex_wam.yaml'
YAML_PATH_MOBILE <- YAML_PATH

#  given the rules and a tabname return just that tab
trimRules <- function(rules, tab_name) {
    tabs <- rules$tabs
    mytabs <- detect(tabs, ~ .x$name == tab_name) 
    rules$tabs = list(mytabs)
    return(rules)
}

getRules <- function(mobile=FALSE) {
    if (mobile) {
        rules <- getCardYaml(YAML_PATH_MOBILE)
    } else {
        rules <- getCardYaml(YAML_PATH)
    }
    args <- commandArgs(trailingOnly = TRUE)
    if (length(args) > 0) {
        rules <- trimRules(rules, args[1])
    }
    return(rules)
}

#  I call this directly from RStudio while debugging
debugGetData <- function(tabname) {
    rules <- trimRules(getRules(), tabname)
    return(lapply(
        rules$tabs,
        get_tab_data,
        getState(rules, list())))
}


getCardYaml <- function(xlpath) {
    return(read_yaml(xlpath))
}


get_section_data <- function( section, tab_name, state ) {
    state <- getState(section, state)
    state$ranges = section$ranges
    return( list(paste(tab_name, section$name, sep='_'),
                 do.call(state$server_method, list(section, state))))                 
}
 
get_tab_data <- function(tab, state) {
    state <- getState(tab, state)
    lapply(tab$sections, get_section_data, tab$name, state)
}

#  cascade non list items up the stack in the STATE attribute
getState <- function(x, state) {
    return( modifyList(state,  keep(x, ~ !is.list(.x)), keep.null = TRUE))
}    

# handling range and cell descriptors in excel like "AD345:ZA2"
split_range <- function(range)    {
    return(str_split(range, ":"))}                        # return two cells

split_cell  <- function(cell)     {
    return(str_match(cell, "([A-Z]+)([0-9]+)")[2:3])}     # return column, row

str2range   <- function(range)    {
    x <- str_split(range, ":")[[1]];
    reply <- c();
    reply$from <- str2cell(x[1]);
    reply$to <- str2cell(x[2]);
    return(reply)}

str2cell    <- function(cell)     {
    x <- str_match(cell, "([A-Z]+)([0-9]+)")[2:3];
    reply <- c();
    reply$column <- x[1];
    reply$row <- as.integer(x[2]);
    return(reply)}

cell2str    <- function(cell)     {
    return(paste(cell$column, cell$row, sep=""))}

range2str   <- function(range)    {
    return(paste(cell2str(range$from), cell2str(range$to), sep=":"))}

add_ranges  <- function(from, to) {
    reply <- c();
    reply$from <- from$from;
    reply$to <- to$to;
    return(reply)}

get_range_width <- function(range) {
    return(letter_to_num(range$to$column) - letter_to_num(range$from$column))
}

get_slice_range <- function(section_range, year_range) {
    from_number <- section_range$from$row - year_range$from$row
    to_number <- from_number + section_range$to$row - section_range$from$row
    return(c(1,from_number:to_number))
}


#  create rows for this name were it's name is a value in the Type column
my_select <- function(df, name) {
    return (select(df, Year, any_of(name), name) %>% rename(CO2Count := name) %>% mutate(Type := name ))
}

#  given path, sheet, range and column names, create a tibble that can be used to generate a chart



get_projection_layout <- function(section, state) {
    range_width <- get_range_width(str2range(state$range)) + 1
    df <- read_excel(state$xlpath, sheet=state$sheet, range=state$range) %>%  rename(Type=1)
    if (!is.null(state$drop_row_from)) {
        df <- df[-(state$drop_row_from:state$drop_row_to),]
    }
    new_cols <- as.list
    df_longer <- df %>% pivot_longer(cols=2:range_width,names_to=state$x, values_to=state$unit)
    reply <- list('df' = df_longer, 'state' = state)
    return(list('data'=reply,'ui_method'=state$ui_method))
}

get_dataset_loop <- function(section, state ) { #sheet, range, colnames, from, to, slice ) {
    cols <- (2:(length(section$cols)+1))
    names(cols) <- section$cols

    ex <- read_excel(state$xlpath, sheet=section$sheet, section$range) %>% rename('Year' = 1) 
    transposed_ex <- ex %>% column_to_rownames("Year") %>%  t() %>% as_tibble(rownames = "Year")
    tp2 <- transposed_ex %>% rename(cols) # identify cols by number and rename them
    return( bind_rows( map( names(cols), \(x) my_select(tp2, x)))) # return the union of selects by each type
}

# TODO - currently unused
get_techannex1_loop <- function(section, state, cols) {
    df <- read_excel(state$xlpath, sheet=state$sheet, range=state$range) %>% rename('Type'=1)
    df_budget <- df %>% select(cols) %>% slice(4,5,6,7,8)
    budget_data <- df_budget %>% pivot_longer(cols=2:4, names_to="version", values_to="reductions")
    return(list('data'=budget_data,'ui_method'=state$ui_method))
}

get_techannex1a_loop <- function(section, state) {
    return (get_techannex1_loop(section, state, c(1,3,4,5)))
}

get_techannex1b_loop <- function(section, state) {
    return (get_techannex1_loop(section, state, c(1,6,7,8)))
}


# TODO hardcoded
get_basic <- function(section, state) {
    range_width <- get_range_width(str2range(state$range)) + 1
    df <- read_excel(state$xlpath, sheet=state$sheet, range=state$range) %>% rename('Type'=1)
    if (!is.null(state$drop_row_from)) {
        df <- df[-(state$drop_row_from:state$drop_row_to),]
    }
    df_longer <- df %>% pivot_longer(cols=2:range_width,names_to=state$x, values_to=state$unit)
    reply <- list('df' = df_longer, 'state' = state)
    return(list('data'=reply,'ui_method'=state$ui_method))
}



get_long_by_x <- function(section, state) {
    range_width <- get_range_width(str2range(state$range)) + 1
    df <- read_excel(state$xlpath, sheet=state$sheet, range=state$range) %>%  rename(Type=1)
    if (!is.null(state$drop_row_from)) {
        df <- df[-(state$drop_row_from:state$drop_row_to),]
    }
    df_longer <- df %>% pivot_longer(cols=2:range_width,names_to=state$x, values_to=state$unit)
    if (state$x == 'Year') {
        df_longer <- df_longer %>% mutate(Year = as.Date(as.numeric(Year) , origin = "1899-12-30"))
        }
    reply <- list('df' = df_longer, 'state' = state)
    return(list('data'=reply,'ui_method'=state$ui_method))
}



get_yearsAreColumns_loop <- function(section, state) {
    range_width <- get_range_width(str2range(state$range)) + 1
    df <- read_excel(state$xlpath, sheet=state$sheet, range=state$range) %>% rename('Type'=1)
    if (!is.null(state$drop_row_from)) {
        df <- df[-(state$drop_row_from:state$drop_row_to),]
    }
    df_longer <- df %>% pivot_longer(cols=2:range_width,names_to="Year", values_to=state$unit)
    reply <- list('df' = df_longer, 'state' = state)
    return(list('data'=reply,'ui_method'=state$ui_method))
}

# TODO hardcoding
get_yearsAreColumns_with_subplots_loop <- function(section, state) {
    dfreply <- list()
    range_width <- get_range_width(str2range(state$range)) + 1
    df <- read_excel(state$xlpath, sheet=state$sheet, range=state$range) %>% rename('Type'=1)
    TypeNames <- df$Type
    df_longer <- df %>% pivot_longer(cols=2:range_width,names_to="Year", values_to=state$unit)
    for (TypeName in TypeNames) {
        df1 <- df_longer %>% filter(Type==TypeName)
        df2 <- df1 %>% select(Year,MtCO2e)
        dfreply[[TypeName]] = df2
    }
    reply <- list('df' = dfreply, 'state' = state)
    return(list('data'=reply,'ui_method'=state$ui_method))
}

#  these include a span of unwanted rows between the year row and the data which must be removed
#  using the gap between year_row and range ...

get_floating_header <- function(section, state) {
    range <- str2range(section$range)
    header_range <- str2range(state$header_row)
    range_width <- get_range_width(range) + 1
    full_range <- add_ranges(header_range, range)
    cull_depth <- range$from$row - header_range$from$row 
    range_depth <- range$to$row - range$from$row + cull_depth
    df <- read_excel(state$xlpath, sheet=state$sheet,
                     range=range2str(full_range))
#    print(df)
    df <- df %>% rename('Type'=1) %>% slice(cull_depth:range_depth)
#    print(df)
    df_longer <- df %>% pivot_longer(cols=2:range_width,names_to=state$x, values_to=state$unit)
#    reply <- df_longer %>% mutate(Year = as.Date(as.numeric(Year) , origin = "1899-12-30"))
    reply <- list('df' = df_longer, 'state' = state)
    return(list('data'=reply,'ui_method'=state$ui_method))
}

get_techannex2_loop <- function(section, state) {
    df <- read_excel(state$xlpath, sheet=state$sheet, range=state$range) %>% rename('Type'=1)
    df_longer <- df %>% pivot_longer(cols=2:30, names_to="Year", values_to="MtCO2e")
    df_wider <- df_longer %>% pivot_wider(names_from = 'Type', values_from="MtCO2e")
    df_wider_show <- df_wider %>% rename('bar'=3) %>% rename('line'=5) %>% rename('bar2'=4) %>% rename('line2'=2)
    df_done <- df_wider_show %>% mutate(Year = as.Date(as.numeric(Year) , origin = "1899-12-30"))
   return(list('data'=df_done,'ui_method'=state$ui_method))
}


get_annex_loop <- function(section, state) {
                                        # select every row from year to end of section, then slice out the bits at the end
    slice_range <- get_slice_range(str2range(state$range), str2range(state$year_row))
    my_range <- add_ranges(str2range(state$year_row), str2range(section$range))

    ex <- read_excel(state$xlpath, sheet=state$sheet, range=range2str(my_range))
 # the unamed column ..1 will be type, every other column is year
    ex <- ex %>% rename('Type' = 1)  
 # just keep the lines at the end, the first line has defined out columns
    ex_sliced <- ex %>% slice(slice_range)

 # the years (currently columns) from 2 to the end are actually year values
    colname <- names(ex_sliced)[2:length(ex_sliced)]
 # transposes the table by all the year columns, give the value a name and call the new column 'year'
    exl <- ex_sliced %>% pivot_longer(cols=all_of(colname), names_to="year", values_to="MtCH4")
 # the date excel date field needs converting
    exly <- exl %>% mutate(year = as.Date(as.numeric(year) , origin = "1899-12-30"))
return(exly)
}

get_figure1_data <- function(section) {
    df <- read_excel(section$STATE$XLpath, sheet=section$STATE$sheet, range=section$range) %>% rename('Type'=1)
    df_budget2 <- df %>% select(1,3,4,5) %>% slice(4,5,6,7,8)
    budget2_data <- df_budget2 %>% pivot_longer(cols=2:4, names_to="version", values_to="reductions")
    return(c('data'=budget2_data,'ui_method'=section$STATE$ui_method))
}

get_figure2_data <- function(section) {
    df <- read_excel(section$STATE$XLpath, sheet=section$STATE$sheet, range=section$range) %>% rename('Type'=1)
    df_longer <- df %>% pivot_longer(cols=2:30, names_to="Year", values_to="MtCO2e")
    df_wider <- df_longer %>% pivot_wider(names_from = 'Type', values_from="MtCO2e")
    df_wider_show <- df_wider %>% rename('bar'=3) %>% rename('line'=5) %>% rename('bar2'=4) %>% rename('line2'=2)
    df_done <- df_wider_show %>% mutate(Year = as.Date(as.numeric(Year) , origin = "1899-12-30"))
    return(c('data'=budget2_data,'ui_method'=section$STATE$ui_method))
}

# TODO - currently unused
get_dataset_loop1 <- function(path, section) {
    cols <- section$from:section$to
    names(cols) <- section$colnames 
    ex <- read_excel(path, sheet=section$sheet, range=section$range) %>% rename('Year'=1) %>% slice(7:9)
    transposed_ex <- ex %>% column_to_rownames("Year") %>%  t() %>% as_tibble(rownames = "Year")
    tp2 <- transposed_ex # %>% rename(cols)
#    tp3 <- tp2 %>% rename(cols)
    return(tp2)
}

                                        # --------------- GRAPHS
                                        # --------------- GRAPHS

get_range_columns <- function(range) {return(c(range$from, range$to))}

makerange <- function(from,to,fig, data) {
    rangecolor <- 'rgba(255, 0, 0, 0.2)'
    fig <- fig %>%
        add_trace(y = ~get(from), type = 'scatter', mode = 'lines', 
                  name = from, line = list(color = rangecolor)) %>%
        add_trace(y = ~get(to), type = 'scatter', mode = 'lines', 
                  name = to, line = list(color = rangecolor),
                  fill = 'tonexty', 
                  fillcolor = rangecolor) # Controls fill color and transparency (alpha)
    return(fig)
}

apex_graph_lines_with_ranges <- function(data) {
    df <- data$df
    state <- data$state
    range_cols <- unlist(lapply(state$ranges, get_range_columns))

    other_cols <- df %>%
        distinct(Type) %>%
        filter(!Type %in% range_cols) %>%
        filter(!Type == 'NA')
    
    other_list <- as.list(other_cols$Type)

    df_wider <- df |> pivot_wider(names_from = Type, values_from = MtCO2e)
    df_middle <- df_wider %>%
        pivot_longer(cols=any_of(unlist(other_list)), names_to = 'lines')
    df_middle <- df_middle %>% filter(Year > 2023)

    
    for(range in state$ranges) {
        return(
            renderApexchart({
                apex(
                    data = df_wider,
                    mapping = aes(
                        x = Year,
                        ymin = .data[[range$from]],
                        ymax = .data[[range$to]]),
                    type = "rangeArea",
                    stroke = "monotoneCubic",
                    serie_name = "Temperature Range"
                )}))
    return(fig)
    }
}

graph_lines_with_ranges <- function(data) {
    df <- data$df
    state <- data$state
    range_cols <- unlist(lapply(state$ranges, get_range_columns))
    other_cols <- df %>% distinct(Type) %>% filter(!Type %in% range_cols) %>% filter(!Type == 'NA')
    other_list <- as.list(other_cols$Type)

    df_wider <- df |> pivot_wider(names_from = Type, values_from = MtCO2e)
    df_middle <- df_wider %>% pivot_longer(cols=any_of(unlist(other_list)), names_to = 'lines')
    fig <- ggplot(df_middle, aes(x=Year, y=value, color=lines)
                  ) + geom_line(linewidth = 1) + geom_point() + theme_minimal()    

    for(range in state$ranges) {
        fig <- makerange(range$from, range$to, fig, df_middle)
    }
    return(fig)
}

graph_stacked_bar <- function(data) {
    df <- data$df
    state <- data$state
    return(
        plot_ly(df, x = ~get(state$x),y = ~get(state$unit), color = ~Type, type = 'bar') %>% layout(barmode='stack')
)
}
                                        # --------------- GRAPHS
graph_stacked_bar_transposed <- function(data) {
    df <- data$df
    state <- data$state
    return(
        plot_ly(df, color = ~get(state$x),y = ~get(state$unit), x = ~Type, type = 'bar') %>% layout(barmode='stack') 
    )
}

apex_line_chart_indicators <- function(data) {
    df <- data$df
    state <- data$state
    all_cols <- df %>% distinct(Type) %>% filter(!Type %in% c(state$x, state$unit))
    col_numbers <- nrow(all_cols)
    count <- nrow(df) / col_numbers
    base_group <- rep(c("Forecast"), each=col_numbers)
    base_group[[1]] = "Actual"
    group = rep(base_group, each = count)
    print(df)
    glimpse(df)
    if (state$x == 'Month') {
        df <- df %>% mutate(Month = as.Date(as.numeric(Month), origin = "1899-12-30"))
    }
    if (state$x == 'Year') {
        df <- df %>% mutate(Year = as.Date(as.numeric(Year), origin = "1899-12-30"))
    }
    if (state$unit == 'Percent') {
        df <- df %>% mutate(Measured = as.integer(Measured * 100))
    }

    return(
        renderApexchart({
            apex(	
                data=df,
                type='line',
                curve='smooth',
                mapping = aes(
                    x=.data[[state$x]], #Year,
                    y=.data[[state$unit]], color = Type) #MtCO2e)) %>%
            ) 
           }))
}

apex_graph_stacked_bar_transposed <- function(data) {
    df <- data$df
    state <- data$state
    return(
        renderApexchart({        
            apex(
                data=df,
                type="bar",
                mapping = aes(
                    y=.data[[state$unit]],
                    x=.data[[state$x]], #Year,
                    fill=Type, #Type
                )) %>%
                    ax_yaxis(decimalsInFloat = 2) %>% 
                    ax_chart(stacked = TRUE) %>%
                    ax_yaxis(max = 4, tickAmount = 4) 
            })
        )
}

apex_graph_stacked_bar_sideways_transposed <- function(data) {
    df <- data$df
    state <- data$state
    return(
        renderApexchart({        
            apex(
                data=df,
                type="bar",
                mapping = aes(
                    y=.data[[state$unit]],
                    fill=.data[[state$x]], #Year,
                    x=Type, #Type
                )) %>%
                    ax_chart(stacked = TRUE)
        })
    )
}


graph_stacked_bar_sideways <- function(data) {
    df <- data$df
    state <- data$state
    return(
        plot_ly(df, color = ~get(state$x),x = ~get(state$unit), y = ~Type, type = 'bar', orientation='h') %>% layout(barmode='stack', yaxis=(list=(title=list(title=state$unit))), xaxis=(list=(title=list(title=state$x))))
    )
}

graph_stacked_bar_sideways_transposed <- function(data) {
    df <- data$df
    state <- data$state
    return(
        plot_ly(df, x = ~get(state$x),y = ~get(state$unit), color = ~Type, type = 'bar', orientation='h') %>% layout(barmode='stack', yaxis=(list=(title=list(title=state$unit))), xaxis=(list=(title=list(title=state$x))))
    )
}


apex_graph_lines_stacked <- function(data) {
    df <- data$df
    state <- data$state
    line_rows <- state$line_rows   # line_rows are not stacked
    unit <- state$unit
    x <- state$x
    
    bar_cols = df %>% distinct(Type) %>% filter(!Type %in% line_rows) |> pull(1)

    df_wide <- df %>% pivot_wider(names_from = Type, values_from = unit)
    glimpse(df_wide)
    df_middle <- df_wide %>%  pivot_longer(bar_cols, names_to= 'bar', values_to = unit)
    glimpse(df_middle)
    return(
        renderApexchart({
        apex(	
            data=df_middle,
            type='area',
            curve='smooth',
            mapping = aes(
                x=.data[[state$x]], #Year,
                y=.data[[state$unit]], color = bar) #MtCO2e)) %>%
        ) 
    }))
}


graph_lines_stacked <- function(data) {
    df <- data$df
    state <- data$state
    line_rows <- state$line_rows   # line_rows are not stacked
    unit <- state$unit
    x <- state$x
    
    bar_cols = df %>% distinct(Type) %>% filter(!Type %in% line_rows) |> pull(1)

    df_wide <- df %>% pivot_wider(names_from = Type, values_from = unit)
    glimpse(df_wide)
    df_middle <- df_wide %>%  pivot_longer(bar_cols, names_to= 'bar', values_to = unit)
    glimpse(df_middle)


    gg_plot <- plot_ly(df_middle, x = ~get(x), y = ~get(unit), color = ~bar, type = 'scatter',mode='none',stackgroup='one') %>%
        layout(yaxis=(list=(title=list(title=state$unit))), xaxis=(list=(title=list(title=state$x))))
    return(gg_plot)
}

# some lines are projections and we want to make them dotted
graph_lines_projections <- function(data) {
    dashed = list(dash = 'dash', width = 2)
    solid = list(dash = 'solid', width = 2)

    df <- data$df
    state <- data$state
    df <- df %>% mutate(Year = as.Date(as.numeric(Year) , origin = "1899-12-30"))
    df_wide <- df %>% pivot_wider(names_from = Type, values_from = state$unit)

    dotted_cols <- df %>% distinct(Type) %>% filter(str_detect(Type, 'projection')) |> pull(1)
    all_cols <- df %>% distinct(Type) %>% filter(!Type %in% c(state$x, state$unit)) |> pull(1)
    
    fig_lines <- plot_ly(df_wide, x=~get(state$x))


    for (col_name in all_cols) {
        linetype <- solid
        if(col_name %in% dotted_cols) {
            linetype <- dashed
        }
        fig_lines <- fig_lines %>% add_trace(y = ~get(col_name), type='scatter',
                                             mode = 'lines+markers', line = linetype, name=col_name)
    }
    return(fig_lines)

}



                                        #   add_trace works if you start with lines (range and non range then follow with stacked bars)
graph_lines_ranges_bar <- function(data) {
    range_str <- function(r) {return(c(r$from,r$to))}
    df <- data$df
    state <- data$state

    rangecolor <- 'rgba(255, 0, 0, 0.2)'
    range_cols <- c()
    if (!is.null(state$ranges)) {
        range_cols <- unlist(lapply(state$ranges, range_str))
    } 
    all_names_cols <- range_cols
    if (!is.null(state$line_rows)) {
        all_names_cols <- c(range_cols, state$line_rows)
    } 
    
    tape <- state$first_col_name
    unit <- state$unit
    range1 <- state$ranges[[1]]

    line_cols = df %>% distinct(Type) %>% filter(Type %in% state$line_rows)
    bar_cols = df %>% distinct(Type) %>% filter(!Type %in% all_names_cols) |> pull(1)
    

     df_wide <- df %>% pivot_wider(names_from = Type, values_from = unit)
    df_middle <- df_wide %>%  pivot_longer(bar_cols, names_to= 'bar', values_to = unit)
    
    
    if (!is.null(state$ranges)) {
        fig_bars <- plot_ly(df_middle, x = ~get(state$x), y=~get(range1$from), type='scatter', mode='lines', name=range1$from, line = list(color = rangecolor))
        fig_bars <- fig_bars  %>% add_trace(y=~get(range1$to), line = list(color = rangecolor), fill='tonexty', fillcolor=rangecolor, name=range1$to)
        fig_bars <- fig_bars %>% add_trace(y = ~get(state$line_rows[[1]]), type = 'scatter', mode="lines", line = list(dash = 'dash', color = 'blue', width = 2), name=state$line_rows[[1]])
    } else {
        for (line_row in state$line_rows) {
            fig_bars <- plot_ly(df_middle, x = ~get(state$x), y = ~get(line_row), type = 'scatter',
                                mode="lines", line = list(dash = 'dash', color = 'blue', width = 2), name=line_row)
        }
    }
    fig_bars <- fig_bars %>% add_trace(y = ~get(unit), color = ~bar, name=~bar, type = 'bar')  %>%
        layout(barmode='stack', yaxis=(list=(title=list(title=state$unit))), xaxis=(list=(title=list(title=state$x))))
    return(fig_bars)
    
}
graph_bars_and_lines_2_3 <- function(data) {
    df <- data$df
    state <- data$state
    line_cols = df %>% distinct(Type) %>% filter(Type %in% state$line_rows)
    bar_cols = df %>% distinct(Type) %>% filter(!Type %in% state$line_rows)

    
    df_wider <- df |> pivot_wider(names_from = Type, values_from = state$unit)
    df_middle <- df_wider %>% pivot_longer(!any_of(c(state$line_rows,'Year',state$unit)), names_to = 'bars')

    return(
        ggplot(df_middle, aes(x=Year, y=value, fill = bars)) + geom_col(position = "stack") + geom_line(data=df_middle, aes(linetype='Net Emissions', x=Year, y=`Net emissions`, group=1)) + geom_line(data=df_middle, aes(linetype='Gross Emissions', x=Year, y=`Gross emissions`, group=1)) + labs(title="") + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
}

apex_graph_bars_and_lines <- function(data) {
    df <- data$df
    state <- data$state
    range_cols <- unname(unlist(state$ranges))
    line_cols <- unlist(state$line_rows)

    bar_cols <- df %>% distinct(Type) %>%
        filter(!Type %in% line_cols) %>%
        filter(!Type %in% range_cols)

    df_wider <- df |> pivot_wider(names_from = Type, values_from = state$unit)
    df_middle <- df_wider %>% pivot_longer(cols=all_of(bar_cols[[1]]), names_to='bars', values_to='value')
    
    print(bar_cols)
    print(df_wider)
    glimpse(df_wider)
    
    glimpse(df_middle)
    print(df_middle)
    ax <- apex(data = df_middle,
                 aes(x = Year,
                     y = value,
                     fill = bars),
               type = "column") %>%
        ax_chart(stacked = TRUE)
    for(line_col in line_cols) {
        ax <- add_line(ax, aes(
                               data=df_middle,
                               x=Year,
                               y = line_col),
                       type = "line") %>%
            ax_markers(size = 6)
        }
                                        #%>%
#        add_line(aes(y = `2030 methane goal`)) 
    
#                 add_line(aes(y = temperature)) %>%
#  ax_yaxis(title = list(text = "Precipitation (in mm)")) %>%
#  ax_yaxis2(
#    opposite = TRUE, 
#    title = list(text = "Temperature (°C)")
#  )
    return(renderApexchart({ax}))
}


graph_bars_and_lines <- function(data) {
    df <- data$df
    state <- data$state
    line_cols = df %>% distinct(Type) %>% filter(Type %in% state$line_rows)
    bar_cols = df %>% distinct(Type) %>% filter(!Type %in% state$line_rows)

    
    df_wider <- df |> pivot_wider(names_from = Type, values_from = state$unit)
    df_middle <- df_wider %>% pivot_longer(!any_of(c(state$line_rows,'Year',state$unit)), names_to = 'bars')
#    df_middle2 <- df_middle %>% pivot_longer(any_of(line_list), names_to = 'lines')

    return(
        ggplot(df_middle, aes(x=Year, y=value, fill = bars)) +
			  geom_col(position = "stack") +
			  geom_line(data=df_middle, aes(linetype='Net Emissions', x=Year,
			  y=`Net emissions`, group=1)) +
			  geom_line(data=df_middle, aes(linetype='Gross Emissions', x=Year,
			  y=`Gross emissions`, group=1)) +
			  geom_line(data=df_middle, aes(linetype='Removals', x=Year,
			  y=`Removals (forests)`, group=1)) + labs(title="") +
			  theme(axis.text.x = element_text(angle = 45, hjust = 1)))
}


graph_bars_and_lines_2_4 <- function(data) {
    df <- data$df
    state <- data$state
    line_cols = df %>% distinct(Type) %>% filter(Type %in% state$line_rows)
    bar_cols = df %>% distinct(Type) %>% filter(!Type %in% state$line_rows)

    
    df_wider <- df |> pivot_wider(names_from = Type, values_from = state$unit)
    df_middle <- df_wider %>% pivot_longer(!any_of(c(state$line_rows,'Year',state$unit)), names_to = 'bars')

    return(
        ggplot(df_middle, aes(x=Year, y=value, fill = bars)) + geom_col(position = "stack") + labs(title="") + theme(axis.text.x = element_text(angle = 45, hjust = 1)))
}



graph_byyear_multiline <- function(data) {
    df <- data$df
    state <- data$state
    gg <- ggplot(df, aes(x = Year, y = .data[[state$unit]],
                           color = Type)) + geom_line() +
        geom_point() + theme_minimal()
    return(ggplotly(gg))
}

graph_techannex1 <- function(data) {
    return (plot_ly(data = data,x = ~version,y = ~reductions,color = ~Type,type = 'bar') %>% layout(barmode = 'stack', yaxis = list(title = 'Budgets')))
}

# TODO hardcoding and include fig6
graph_bars_subplots <- function(data) {
    df <- data$df
    state <- data$state
    figs <- list()
    for (sub in df) {
        figs <- append(figs,plot_ly(data=sub, y=~MtCO2e, x=~Year, type='bar'))
    }
    fig1 <- plot_ly(data=df[[1]], y=~MtCO2e, x=~Year, type='bar', name='Transport')
    fig2 <- plot_ly(data=df[[2]], y=~MtCO2e, x=~Year, type='bar', name='Energy and industry')
    fig3 <- plot_ly(data=df[[3]], y=~MtCO2e, x=~Year, type='bar', name='Agriculture')
    fig4 <- plot_ly(data=df[[4]], y=~MtCO2e, x=~Year, type='bar', name='Waste')
    fig5 <- plot_ly(data=df[[5]], y=~MtCO2e, x=~Year, type='bar', name='F-gases')
    fig6 <- plot_ly(data=df[[6]], y=~MtCO2e, x=~Year, type='bar', name='Gross emissions')
    fig <- subplot(fig1,fig2,fig3,fig4,fig5,shareY=TRUE) 
    return(fig)
}

graph_byyear_stackedbar <- function(data) {
    df <- data$df
    state <- data$state
    return(
        plot_ly(df, 
                x = ~Year, 
                y = ~MtCO2e, 
                color = ~Type, 
                type = 'scatter', 
                mode = 'bars', 
                stackgroup = 'one') %>%
        layout(title = "Stacked Line/Area Chart in R",
               xaxis = list(title = state$x),
               yaxis = list(title = state$unit))
    )
}

# TODO - functions appear twice
#line_chart <- function(data) {
#    print("XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX")
#    return(
#        ggplot(
#            data,
#            aes(
#                x = Year,
#                y = MtCO2e,
#                fill = Type)
#        ) + geom_line(color = 'black', size = 5) )
#}



#   make the first line solid, all the rest dashed
line_chart_indicators <- function(data) {
    df <- data$df
    state <- data$state
    all_cols <- df %>% distinct(Type) %>% filter(!Type %in% c(state$x, state$unit))
    col_numbers <- nrow(all_cols)
    count <- nrow(df) / col_numbers
    base_group <- rep(c("Forecast"), each=col_numbers)
    base_group[[1]] = "Actual"
    group = rep(base_group, each = count)
    print(df)
    glimpse(df)
    if (state$x == 'Month') {
        df <- df %>% mutate(Month = as.Date(as.numeric(Month), origin = "1899-12-30"))
    }
    if (state$x == 'Year') {
        df <- df %>% mutate(Year = as.Date(as.numeric(Year), origin = "1899-12-30"))
    }
    if (state$unit == 'Percent') {
        df <- df %>% mutate(Measured = as.integer(Measured * 100))
    }

    return(plot_ly(df, x = ~get(state$x),
                   y = ~get(state$unit),
                   color = ~Type,
                   type = 'scatter',
                   linetype=~group,
                   connectgaps=TRUE,
                   mode = 'lines+markers')
           %>% layout(
                   xaxis = list(rangemode = "tozero"),
                   yaxis = list(rangemode = "tozero")
               )
           )
#           %>% layout(
#                   yaxis=(list=(rangemode = "tozero")),
#                   xaxis=(list(rangemode ="tozero"))))
}

    
                                        #                                title=state$unit)),
#                               title=list(title=state$x))))

#   make the first line solid, all the rest dashed
line_chart <- function(data) {
    df <- data$df
    state <- data$state
    all_cols <- df %>% distinct(Type) %>% filter(!Type %in% c(state$x, state$unit))
    base_group <- c()
    for (col in as.list(all_cols$Type)) {
        if (str_detect(col, "projected")) {
            base_group <- append(base_group, "Forecast")
        } else {
            base_group <- append(base_group, "Actual")
        }

    }
    col_numbers <- nrow(all_cols)
    count <- nrow(df) / col_numbers
    group = rep(base_group, each = count)
    
    return(plot_ly(df, x = ~get(state$x), y = ~get(state$unit), color = ~Type, type = "scatter", linetype=~group, mode = "lines+markers")
 %>% layout(yaxis=(list=(title=list(title=state$unit))), xaxis=(list=(title=list(title=state$x))))
           )
}


