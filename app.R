#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(geofacet)
# library(extrafont)
# library(hrbrthemes)
# loadfonts(device = "win", quiet = T)
library(ggtext)
library(shinythemes)


# prep --------------------------------------------------------------------

# > theme -----------------------------------------------------------------

theme_post <- function(){ 
    # hrbrthemes::theme_ipsum_ps() +
        theme_minimal()+
        theme(
            plot.margin=margin(l=0, unit="cm"),
            plot.title = element_markdown(color="grey20",
                                          margin=margin(l=0, unit="cm"),
                                          size=18), #13
            plot.title.position = "plot",
            plot.subtitle = element_text(color="grey50",
                                         margin=margin(t=0.1, b=0.3, unit="cm"),
                                         size=15), #9
            plot.caption = element_text(color = "grey50", 
                                        margin=margin(t=1, unit="cm"),
                                        size=10, #7
                                        hjust=c(0, 1)),
            plot.caption.position = "panel",
            axis.title.x = element_blank(),
            axis.text.x = element_text(size = 10), #7
            axis.title.y=element_blank(),
            axis.text.y = element_text(size = 10), #7
            panel.grid.minor.x = element_blank(),
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.spacing = unit(0.25, "cm"),
            panel.spacing.y = unit(0.25, "cm"),
            strip.text = element_text(angle = 0,
                                      size= 14, #9,
                                      hjust=0,
                                      # vjust = 1,
                                      face="bold"),
            legend.title = element_text(color="grey30",
                                        face="bold",
                                        # vjust=1,
                                        size=12), #7
            legend.text = element_text(size=12,  #7
                                       color="grey30"),
            legend.position = "bottom",
            legend.justification = "left",
            legend.box = "vertical",  #arrangement of multiple legends
            legend.direction = "horizontal",
            legend.margin = margin(l=0, t=0, unit="cm"),
            legend.spacing.y = unit(.2, units="cm"),
            legend.text.align = 0,
            legend.box.just = "left",
            legend.key.height = unit(0.2, "line"),
            legend.key.width = unit(0.5, "line"),
            text=element_text(size=5))
}


# > holidays --------------------------------------------------------------

date.start.lockdown <- tibble(date=as.Date("2020-03-14"), event="start of lockdown")
date.opening.shops.small <- tibble(date=as.Date("2020-04-14"), event="small shops reopen")
date.opening.shops.all <- tibble(date=as.Date("2020-05-04"), event="all shops reopen")
date.schools <- tibble(date=as.Date("2020-05-18"), event="schools reopen")
date.restaurants <- tibble(date=as.Date("2020-05-15"), event="restaurants reopen")

df_dates <- bind_rows(date.start.lockdown, date.opening.shops.all, date.opening.shops.small,
                      date.schools, date.restaurants) %>%
    mutate(label=paste0(event, " (", format(date, "%d %b"), ")")) %>% 
    mutate(label.2=paste0(format(date, "%d %b"), ", ", event)) %>% 
    arrange(date) %>% 
    mutate(label=fct_inorder(label))

df_holidays <- jsonlite::fromJSON("https://date.nager.at/api/v2/publicholidays/2020/AT ") %>% 
    mutate(date=lubridate::ymd(date))        



# > get data --------------------------------------------------------------


file_link <- "https://www.gstatic.com/covid19/mobility/Global_Mobility_Report.csv"

df_global <- readr::read_csv(file=file_link, col_types = "ccccDdddddd")

df_AT <- df_global %>% 
    filter(country_region_code=="AT") %>% 
    mutate(sub_region_1=case_when(is.na(sub_region_1) ~ "State level",
                                  TRUE ~ as.character(sub_region_1)))

df_AT_long <- df_AT %>% 
    pivot_longer(cols=contains("baseline"),
                 names_to="type",
                 values_to="value") %>% 
    mutate(week.day=lubridate::wday(date, label=T), .after=date) %>% 
    mutate(sub_region_1=forcats::as_factor(sub_region_1))

df_AT_long <- df_AT_long %>% 
    mutate(place.description=case_when(str_detect(type, "retail") ~ "‘places like restaurants, cafes, shopping centers, theme parks, museums, libraries, and movie theaters.’",
                                       str_detect(type, "grocery") ~ "’places like grocery markets, food warehouses, farmers markets, specialty food shops, drug stores, and pharmacies.’",
                                       str_detect(type, "park") ~ "‘places like local parks, national parks, public beaches, marinas, dog parks, plazas, and public gardens.’",
                                       str_detect(type, "transit") ~ "‘places like public transport hubs such as subway, bus, and train stations.’",
                                       str_detect(type, "work") ~ "‘places of work’",
                                       str_detect(type, "residential") ~ "‘places of residence’",
                                       TRUE ~ as.character("missing"))) %>% 
    mutate(day.indicator=case_when(week.day %in% c("Sat", "Sun") ~ "weekend",
                                   date %in% df_holidays$date ~ "public holiday",
                                   TRUE ~ as.character("business day"))) %>% 
    mutate(type=str_remove(type, "_percent.*$") %>% str_replace_all(., "_", " ") %>% 
               str_to_title(.))




# UI ----------------------------------------------------------------------
# ui <- fluidPage(theme=shinytheme("lumen"),
# 
#     # Application title
#     titlePanel("Changes in mobility patterns during Corona crisis: Google Mobility Reports"),
# 
#     # Sidebar with a slider input for number of bins 
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(
#                 inputId = "type_of_place",
#                 label = "Type of Place:",
#                 choices = c("Grocery and Pharmacy", "Parks", "Residential", "Retail and Recreation", "Transit stations", "Workplaces"),
#                 multiple = FALSE,
#                 selected = "Grocery and Pharmacy"
#             )
#         ),
#          
#         # Show a plot of the generated distribution
#         mainPanel(
#            plotOutput("plot_map", height = "700px")
#         )
#     )
# )


ui <- fluidPage(theme=shinytheme("yeti"),
 
                fluidRow(
                    selectInput(
                        inputId = "type_of_place",
                        label = "Type of Place:",
                        choices = c("Grocery and Pharmacy", "Parks", "Residential", "Retail and Recreation", "Transit stations", "Workplaces"),
                        multiple = FALSE,
                        selected = "Grocery and Pharmacy"),
 
                plotOutput("plot_map", height = "700px")
                )
)
                


# SERVER ------------------------------------------------------------------

server <- function(input, output) {

    output$plot_map <- renderPlot({


# > plot ------------------------------------------------------------------

        
        AUT_states_grid <- data.frame(
            code = c("4", "3", "9", "8", "7", "5", "6", "1", "2"),
            # name_de = c("Oberösterreich", "Niederösterreich", "Wien", "Vorarlberg", "Tirol", "Salzburg", "Steiermark", "Burgenland", "Kärnten"),
            name_en=c("Upper Austria", "Lower Austria", "Vienna", "Vorarlberg", "Tyrol", "Salzburg", "Styria", "Burgenland", "Carinthia"),
            row = c(1, 1, 1, 2, 2, 2, 2, 2, 3),
            col = c(3, 4, 5, 1, 2, 3, 4, 5, 4))
        
        txt_explain <- "Values for each day are compared to a baseline value for that day of the week: The baseline is the median value, for the corresponding day of the week, during the 5-week period Jan 3–Feb 6, 2020."
        
        
        type_select <- input$type_of_place
        
        df_AT_long_type <- df_AT_long %>% 
            filter(str_detect(type, regex(type_select, ignore_case=T))) %>% 
            filter(!str_detect(sub_region_1, "State")) 
        
        data_as_of <- format(max(df_AT_long_type$date), "%d %b %Y")
        
        txt_subtitle <- str_wrap(paste0(txt_explain, " Data as of ", data_as_of, "."), 130)
        
        df_AT_long_type %>% 
            ggplot()+
            labs(title=glue::glue("Changes in Mobility during Covid-19 crisis: 
                   <span style = 'color:red'>{unique(df_AT_long_type$type)}</span>"),
                 subtitle = txt_subtitle,
                 caption=c("data: Google Mobility Reports", " graph: Roland Schmidt | @zoowalk | www.werk.statt.codes"))+
            geom_hline(yintercept = 0,
                       color="black")+
            geom_vline(data=df_dates,
                       aes(xintercept=date,
                           linetype=label),
                       key_glyph=draw_key_path, 
                       alpha=0.4,
                       size=0.5)+
            geom_point(aes(x=date,
                           color=day.indicator,
                           fill=day.indicator,
                           y=value),
                       key_glyph=draw_key_rect, 
                       size=.5)+
            geom_label(data=df_AT_long_type %>% 
                           filter(day.indicator=="business day") %>% 
                           filter(date==max(.$date)),  
                       aes(x=max(df_AT_long_type$date)+lubridate::days(2),
                           y=value,
                           label=scales::percent(value, accuracy=1,
                                                 suffix = "%",
                                                 scale=1)),
                       size=5,
                       fill="steelblue",
                       fontface="bold",
                       color="white",
                       label.padding = unit(0.1, "lines"),
                       label.r=unit(0, "cm"),
                       nudge_x=1,
                       hjust=0)+
            scale_x_date(labels = scales::label_date_short(),
                         expand=expansion(mult=c(0, 0.2)),
                         breaks=df_dates %>% filter(!str_detect(event, "restaurants reopen")) %>% pull(date)) +
            scale_y_continuous(labels=scales::label_percent(accuracy = 1,
                                                            scale=1),
                               limits=c(-100, 100))+
            scale_fill_manual(values=c("business day"="steelblue",
                                       "public holiday"="green3",
                                       "weekend"="orange"))+ 
            scale_color_manual(values=c("business day"="steelblue",
                                        "public holiday"="green3",
                                        "weekend"="orange"))+
            # theme_minimal()+
            # hrbrthemes::theme_ipsum_es()+
            theme_post()+
            facet_geo(~sub_region_1, 
                      grid = AUT_states_grid)+
            guides(fill=guide_legend(override.aes=list(size=2), 
                                     title="type of days",
                                     title.position = "left",
                                     keyheight= 0.5,
                                     label.hjust = 0), 
                   color="none",
                   linetype=guide_legend(label.hjust = 0, 
                                         nrow = 1,
                                         keyheight= 0.5,
                                         title ="dates",
                                         title.position = "left"))
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
