#########------------------  Contact and database information------------------#########
##  Title:                                                                          
#   "Public water arsenic concentrations in counties and community water systems    
#   across the United States, 2006-2011                                             
#   Contact:                                                                      
#   Anne Nigra, ScM                                                                 
#   Columbia University Mailman School of Public Health                             
#   Environmental Health Sciences                                                   
#   aen2136@cumc.columbia.edu 
#
#### Some packages  ######
library(Hmisc)
library(reshape)
library(ggplot2)
library(plyr)
library(reshape2)
library(RColorBrewer)
library(colorspace)
library(openxlsx)
library(plotly)
library(stringr)

## Pull JSON file
#url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
#counties <- rjson::fromJSON(file=url)
#save(counties, file="~/Google Drive/Research/EPAAsTrajectories/countiesRJSON.RData")
load("~/Google Drive/Research/EPAAsTrajectories/countiesRJSON.RData")
county<-read.xlsx("~/Google Drive/Research/EPAAsTrajectories/March2020/CountyAsSYR3.xlsx")

# Assign group10 
county$group10<-0
county$group10[county$WeightedAs20062008<=10&county$WeightedAs20092011<=10]<-1 #low/low
county$group10[county$WeightedAs20062008<=10&county$WeightedAs20092011>10]<-2 #low/high
county$group10[county$WeightedAs20062008>10&county$WeightedAs20092011<=10]<-3 #high/low
county$group10[county$WeightedAs20062008>10&county$WeightedAs20092011>10]<-4 #high/high

county$group10[which(county$group10==0)]<-NA
county$glabel<-NA
county$glabel[which(county$group10==1)]<-"Low/Low"
county$glabel[which(county$group10==2)]<-"Low/High"
county$glabel[which(county$group10==3)]<-"High/Low"
county$glabel[which(county$group10==4)]<-"High/High"

# Fix how FIPS names will appear on map
county$Full.Name[which(county$CountyFIPS=="02201")]<- "AK, Prince of Wales-Outer Ketchikan"
county$Full.Name[which(county$CountyFIPS=="02232")]<- "AK, Skagway-Hoonah-Angoon"
county$Full.Name[which(county$CountyFIPS=="02280")]<- "AK, Wrangell-Petersburg"
# Add spaces between words
county$CoName<-gsub("([a-z])([A-Z])", "\\1 \\2", county$Full.Name)

# Keep on necessary vars
myvars<-c("CountyFIPS","WeightedAs20062008","WeightedAs20092011","WeightedAs20062011","CoName","glabel")
df<-county[myvars]
colnames(df)[colnames(df)=="CountyFIPS"] <- "fips"


# How to finalize our text hover issues, using text, hoverinfo, hovertemplate, etc:
#https://plot.ly/r/reference/
# Notes:
# hovertext is same as text; putting text in "" keeps only the info in df$hover appearing
# hovertemplate overrides hoverinfo


#### Main plot: legend text bottom margin, + SRP logo 
# custom color palette to match static maps, but in continuous scale
vector1 <- c(0,0.2,0.8,1)
vector2 <- c("#C3FA9F", "#F6F1A9","#FDC030","#F08080")
cp <- array(c(vector1,vector2),dim = c(4,2))
print(cp)

# Set custom hover text
df$hover <- with(df, paste(
  "<b>County:</b>", CoName, '<br>',  
  "<b>Water arsenic averages:</b>", '<br>',
  "<i>  2006-2008:</i>", WeightedAs20062008,"<i>µg/L</i>", '<br>',
  "<i>  2009-2011:</i>", WeightedAs20092011,"<i>µg/L</i>", '<br>',
  "<i>  2006-2011:</i>", WeightedAs20062011,"<i>µg/L</i>", '<br>',
  "<b>MCL compliance category:</b>", glabel))

m <- list(
  l = 2,
  r = 2,
  b = 100,
  t = 120,
  pad = 4
)

a <- list(text ="<i>Based on over 230,000 water arsenic monitoring records from over 36,000 community water systems, collected by the US EPA in the Six Year Review of Contaminant Occurrence database.
          The contribution of water arsenic from each community water system serving a county is weighted by the number of people served by each system. MCL= maximum contaminant level (10 µg/L).
          MCL compliance category indicates whether the average arsenic concentration was below (Low) or above (High) the 10 µg/L MCL in each time period (2006-2008 versus 2009-2011). Blank/missing
          counties did not submit adequate data. Six-year estimated averages are for descriptive purposes only; for detailed description of data analysis see Nigra et al. 2020.</i>",
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          x = 0.6,
          y = -0.2,
          bordercolor="white", borderwidth=1
)

# Annotation for SRP link
srplink <- list(text ='<a href="https://www.mailman.columbia.edu/research/columbia-superfund-research-program"><b>Columbia University\nSuperfund Research Program</b></a>',
          xref = "paper",
          yref = "paper",
          showarrow = FALSE,
          x = 1.2,
          y = 0.3,
          bordercolor="white", borderwidth=1
)

fig <- plot_ly() 
fig <- fig %>% add_trace(
  type="choroplethmapbox",
  geojson=counties,
  locations=df$fips,
  z=df$WeightedAs20062011,
  # name=df$CoName,
  showscale=TRUE,
  text = ~df$hover,
  hoverinfo="text", # use "text" to only display that appering in the text (df$hover) string
  colorscale=cp, #YlGnBu, etc.
  colorbar=list(thickness=30, tickmode="array",tickvals=c(0,2,4,6,8,10),
                ticktext=c("0 µg/L","2 µg/L","4 µg/L","6 µg/L","8 µg/L","≥ 10 µg/L"),
                title=list(text="<b>Water arsenic concentrations,\n              2006-2011\n</b> <br>",size=8,side="top")),
  zmin=0, zmax=10,
  marker=list(line=list(width=0.2, color="antiquewhite4"),  #width of county-lines
              opacity=0.9) # opacity of color within county
)

fig <- fig %>% layout(
  mapbox=list(style="carto-positron",zoom =3, center=list(lon= -95.71, lat=37.09)),
  title=list(text="<b>County-level, weighted average arsenic concentrations (µg/L) in community water systems, 2006-2011</b><br><br><br>",
             family="Arial",size=12),
  margin=m,
  annotations=a)


# Add SRP and CU EHS logos 
fig <- fig %>%
  layout(
    images = list(
      list(href="https://www.mailman.columbia.edu/research/columbia-superfund-research-program",
           source = "https://annenigra.github.io/SRPlogo.png",
           xref = "paper",
           yref = "paper",
           x= 1,
           y= 0.3,
           sizex = 0.24,
           sizey = 0.24,
           opacity = 0.8 
           )
      )
    )

fig<- fig%>%
  layout(annotations=srplink)

fig

# Save as .html
htmlwidgets::saveWidget(as_widget(fig), "~/Google Drive/Research/EPAAsTrajectories/March2020/ColumbiaArsenicMap.html")
htmlwidgets::saveWidget(as_widget(fig), "~/Desktop/annenigra.github.io/ColumbiaArsenicMap.html")








### Notes on changing the color scale:
# here's how we specificy/change the color scale:
#colorscale
#Parent: data[type=choroplethmapbox]
#Type: colorscale
#Sets the colorscale. The colorscale must be an array containing arrays
#mapping a normalized value to an rgb, rgba, hex, hsl, hsv, or named color string.
#At minimum, a mapping for the lowest (0) and highest (1) values are required. 
#For example, `[[0, 'rgb(0,0,255)'], [1, 'rgb(255,0,0)']]`. To control the bounds of
#the colorscale in color space, use`zmin` and `zmax`. Alternatively, `colorscale`
#may be a palette name string of the following list: Greys,YlGnBu,Greens,YlOrRd,
#Bluered,RdBu,Reds,Blues,Picnic,Rainbow,Portland,Jet,Hot,Blackbody,Earth,Electric,Viridis,Cividis.

### Things that DO work:
## This does work because it must be a 2-D array:
# # Create two vectors of different lengths.
# vector1 <- c(0,1)
# vector2 <- c("rgb(0,0,255)", "rgb(255,0,0)")
# test <- array(c(vector1,vector2),dim = c(2,2))
# print(test)
# 
# ## Does this work with # colors? Yes!
# vector1 <- c(0,1)
# vector2 <- c("#a6cee3", "#1f78b4")
# test2 <- array(c(vector1,vector2),dim = c(2,2))
# print(test2)
# 
# ## There is a built in fxn that should convert lists to the correct array:
# x<-list(list(0, 0.5, 1), list("red", "white", "blue")) # this also works!
# 
# ### Custom color palette
# vector1 <- c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1)
# vector2 <- c("#a6cee3", "#1f78b4","#b2df8a","#33a02c" ,"#fb9a99","#e31a1c","#fdbf6f","#ff7f00","#cab2d6","#6a3d9a","#6a7d9b")
# result <- array(c(vector1,vector2),dim = c(11,2))
# print(result)
# 
# 
# ### Trying to match static maps:
# vector1 <- c(0,0.2,0.8,1)
# vector2 <- c("#C3FA9F", "#F6F1A9","#FDC030","#F08080")
# cp <- array(c(vector1,vector2),dim = c(4,2))
# print(cp)


