list.of.packages <- c("data.table","ggplot2","scales","plotly","htmlwidgets","showtext","listviewer")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)

setwd("~/git/grand-bargain-plotly/")

font_add(family = "Roboto", "font/Roboto/Roboto-Regular.ttf")

blue = "#32b8ff"
dark_green = "#008181"
green = "#61b8ab"
light_green = "#25ccc7"
yellow = "#ffb137"
orange = "#ff924c"
red = "#ff5657"
grey = "#747474"

di_style = theme_bw() +
  theme(
    panel.border = element_blank()
    ,panel.grid.major.x = element_blank()
    ,panel.grid.minor.x = element_blank()
    ,panel.grid.major.y = element_line(colour = grey)
    ,panel.grid.minor.y = element_blank()
    ,panel.background = element_blank()
    ,plot.background = element_blank()
    ,axis.line.x = element_line(colour = "black")
    ,axis.line.y = element_blank()
    ,axis.ticks = element_blank()
    ,legend.position = "bottom"
    ,text=element_text(family="Roboto", size=14)
  )

donut_style = di_style +
  theme(
    panel.grid.major.y = element_blank()
    ,axis.line.x = element_blank()
    ,axis.text = element_blank()
  )

rotate_x_text_45 = theme(
  axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)
)
rotate_x_text_90 = theme(
  axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
)

roboto_font = list(
  family = "Roboto"
)

signatory_data = data.frame(
  `Signatory group` = c("Member states", "International NGOs", "UN agencies", "RCRC Movements", "Inter-governmental organisations")
  ,`Number of members` = c(25, 25, 12, 2 ,2)
  ,color = c(green, dark_green, blue, orange, yellow)
  ,check.names = F
  )

signatory_data$`Signatory group` = factor(signatory_data$`Signatory group`, levels = signatory_data$`Signatory group`)

# Bar
bar = ggplot(signatory_data,aes(x=`Signatory group`,y=`Number of members`, fill=color)) +
  geom_bar(stat="identity") +
  scale_fill_identity() +
  scale_y_continuous(expand = c(0, 0)) + # Force y-grid to start at x-axis
  di_style +
  theme(legend.position = "none") +
  rotate_x_text_45 + # Or chose _90 or remove for horizontal
  labs(
    y="Signatories",
    x="",
    fill=""
  )
bar_html = ggplotly(bar, tooltip = c("Signatory group", "Number of members")) %>% plotly::layout(font=roboto_font, autosize=T)
saveWidget(bar_html, file = "output/gb_signatory_bar.html", selfcontained=F)

funding_data = fread("input/funding.csv")
funding_data$group = factor(funding_data$group, levels = unique(funding_data$group))
names(funding_data) = c(
  "Type",
  "Year",
  "Value"
)

# Line
line = ggplot(funding_data,aes(x=Year,y=Value,group=Type,color=Type)) +
  geom_line(linewidth=1.3) +
  geom_point() + # Points on line
  scale_color_manual(values=c(red, green)) +
  scale_y_continuous(expand = c(0, 0)) +
  expand_limits(y=c(0, 0)) +
  scale_x_continuous(breaks=c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  expand_limits(y=c(0, max(funding_data$Value*1.1))) +
  di_style +
  labs(
    y="in Billions (USD)",
    x="",
    color=""
  )

line_html = ggplotly(line, tooltip = c("Year", "Value")) %>% plotly::layout(
  font=roboto_font,
  autosize=T,
  legend=list(x=0,
              y=-0.5,
      xanchor='left',
      yanchor='bottom',
      orientation='h')
  )
saveWidget(line_html, file = "output/gb_funding_line.html", selfcontained=F)


test_dropdown_data = data.frame(
  Year=rep(c(2001:2005), 2),
  Value=runif(10, min=20, max=50),
  Donor=c(rep("United States", 5), rep("United Kingdom", 5)),
  check.names=F
)

create_buttons <- function(df, filter_variable) {
  unique_values = unique(df[,filter_variable])
  lapply(
    unique_values,
    FUN = function(unique_value, df) {
      df_sub = df[which(df[,filter_variable]==unique_value),]
      button <- list(
        method = 'update',
        args = list(list(
          y = list(df_sub$Value),
          x = list(df_sub$Year)
          )),
        label = paste0(filter_variable, ': ', unique_value)
      )
    },
    df
  )
}

# Line
unique_donors = unique(test_dropdown_data$Donor)
line_dropdown = ggplot(subset(test_dropdown_data, Donor==unique_donors[1]),aes(x=Year,y=Value)) +
  geom_line(linewidth=1.3, color=red) +
  geom_point(color=red) + # Points on line
  di_style +
  labs(
    y="in Billions (USD)",
    x="",
    color=""
  )


line_dropdown_html = ggplotly(line_dropdown, tooltip = c("Year", "Value")) %>% plotly::layout(
  font=roboto_font,
  autosize=T,
  yaxis = list(
    autodomain=T,
    autorange=T,
    tickmode="auto"
  ),
  updatemenus = list(
    list(
      buttons = create_buttons(test_dropdown_data, 'Donor'),
      x=0,
      xanchor="left",
      y=0.95,
      yanchor="bottom"
    )
  )
)
# plotly_json(line_dropdown_html) 
saveWidget(line_dropdown_html, file = "output/gb_funding_line_dropdown.html", selfcontained=F)