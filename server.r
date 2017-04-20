#install.packages("shiny")
#install.packages("ggplot2")
#install.packages("plotly")
#install.packages("leaflet")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("readr")
#install.packages("geojsonio")
#install.packages("plyr")
#install.packages("GGally")
#install.packages("scales")
#install.packages("viridis")
#install.packages("shinythemes")

library(shiny)
library(ggplot2)
library(plotly)
library(leaflet)
library(dplyr)
library(tidyr)
library(readr)
library(geojsonio)
library(plyr)
library(GGally)
library("scales")
library("viridis")
library(shinythemes)

state_data <- read_csv("EDAV-State-Data.csv")
mapdt <- geojson_read("http://eric.clst.org/wupl/Stuff/gz_2010_us_040_00_500k.json",what = "sp")
#For more detailed map data use the 5 MB line: http://eric.clst.org/wupl/Stuff/gz_2010_us_040_00_5m.json

#Clean up dataframe and match with map geojson order

Title<-c("State","Population","Nation_Pct","Density","Inc","Pct_College",
         "Poverty_Rate","Gini","Benefits_Vs_Tax","Gun_Owner_Pct","Pct_White",
         "Pct_White_Evangelical","Pct_Nonreligious", "Pct_Urban",
         "Pct_Obese","Life_Exp","Pct_Union","RTW","Region","Democratic_Vote",
         "Pct_Biz_Owner","Pct_Smoker","Pct_Highschoolbelow","Pct_Somecollege",
         "Pct_College_Graduate","Pct_Post_Graduate","No_money_for_healthcare",
         "No_money_for_food")
colnames(state_data) <- Title
formap <- data.frame(State = c("District of Columbia","Puerto Rico"))
maptemp <- rbind.fill(state_data,formap)
Order <- mapdt$NAME
map_data<-state_data[match(Order,state_data$State),]
mapdt@data <- data.frame(mapdt@data,maptemp[match(mapdt@data[,"NAME"],maptemp[,"State"]),])


shinyServer(
  function(input, output){
    fancy_scientific <- function(l) {
      # turn in to character string in scientific notation
      l <- format(l, scientific = TRUE)
      # quote the part before the exponent to keep all the digits
      l <- gsub("^(.*)e", "'\\1'e", l)
      # turn the 'e+' into plotmath format
      l <- gsub("e", "%*%10^", l)
      # return this as an expression
      parse(text=l)
    }
    
    # Report Part
    output$hist1 <- renderPlotly({
      ggplot(data = state_data, aes(x=reorder(State,Population), y= Population))+
        geom_bar(aes(fill = state_data$Democratic_Vote),stat = "identity")+scale_fill_gradientn(colours = c("red","indianred2", "purple", "mediumblue", "navyblue"))+
        coord_flip()+
        scale_y_continuous(labels = comma)+
        ylab("Population")+
        xlab("State")+
        ggtitle("General View for Population on state level")
      
    })
    
    output$h_com1<-renderText({
      print("Despite the talk of America's 'small state bias' helping the Republican Party, 
            population in and of itself does not play a huge role in a state's partisanship.
            California is very Democratic and is the biggest state, but Texas is the second-biggest
            state and is very Republican. Of the ten smallest states, only 3 are solidly Republican. 
            Of the ten largest states, only 3 are solidly Democratic.")
    })
    
    output$hist2 <- renderPlotly({
      state_data$Urban = state_data$Population * state_data$Pct_Urban
      state_data$Rural = state_data$Population * (1 - state_data$Pct_Urban)
      state_data_urban2 <- state_data[,c("State","Rural","Urban")]

      m <- list(l = 100,
                r = 50,
                b =50,
                t =10,
                pad = 1)
      p<-plot_ly(data = state_data_urban2,x=~Urban, y=~reorder(State,Urban),type = 'bar',name  = "Urban") %>%
        add_trace(x = ~Rural,name = 'Rural')  %>%
        layout(yaxis = list(title = "",tickfont=list(size = 8)),xaxis = list(title=""),barmode = 'stack',margin = m)
    })
    
    output$h_com2 <- renderText({
      print("Here, we can see how percentages are somewhat misleading. Despite many states being 
            heavily urban, if the state is large, the rural population can still be quite substantial. 
            Texas has the largest rural population in number, despite being in a distant 2nd place to 
            California. But since Senate races and presidential elections are decided solely by who wins 
            states, this is not as relevant as it may seem.")
    })
    
    output$hist3 <- renderPlotly({
      state_data$Democratic = state_data$Democratic_Vote * state_data$Population
      state_data$Republican = (1 - state_data$Democratic_Vote) * state_data$Population
      state_data_DR2 <- state_data[, c("State","Democratic","Republican")]
      m <- list(l = 100,
                r = 50,
                b =50,
                t =10,
                pad = 1)
      p<-plot_ly(data = state_data_DR2,x=~Democratic, y=~reorder(State,Democratic),type = 'bar',name  = "Democratic",
                 marker = list(color = 'rgb(26, 118, 255)')) %>%
        add_trace(x = ~Republican,name = 'Republican',marker = list(color = 'rgb(205, 61, 61)'))  %>%
        layout(yaxis = list(title = "",tickfont=list(size = 8)),xaxis = list(title=""),barmode = 'stack',margin = m)

    })
    
    output$h_com3 <- renderText({
      print("While not every American votes, each person does give a state more electoral power.
            Using this, we can see just how many votes each side gets from each state, and how 
            even the most lopsided states has hundreds of thousands, or even millions of supporters
            for the losing party. In a winner-take-all system, it can be easy to forget that no state
            is unanimous.")
    })
    
    corrdata = state_data[,c("Population", "Poverty_Rate", "Pct_College_Graduate",
                             "Gun_Owner_Pct", "Pct_White", "Pct_White_Evangelical",
                             "Pct_Nonreligious", "Pct_Urban", "Pct_Obese", "Life_Exp", 
                             "Pct_Union", "Pct_Smoker", "Democratic_Vote")]
    colnames(corrdata) = c("pop", "poverty", "college", "gunowner", "white", "whiteeva", 
                           "nonrelig", "urban", "obese", "LifeExp.", "union", "smoker", "DVote")
    corrdata$Vote_Result[corrdata$DVote >= 0.5]<-"Democratic"
    corrdata$Vote_Result[corrdata$DVote <= 0.5]<-"Republican"
    
    output$cor1 <- renderPlot({
      ggcorr(corrdata, label = TRUE, label_round =2, digits = 3, label_size = 3.5, nudge_x = -0.5, layout.exp = 2)
    })
    
    output$cor2 <- renderPlot({
      ggpairs(corrdata[,c(2,3,4,6,7,8,13)], aes(colour = "blue"))
    })
    

    
    output$com_c1 <- renderText({
      print("The strongest relationships between all the chosen variables involve life expectancy.
High life expectancy is postively correlated with college education, and is negatively correlated
with obesity, smoking, poverty, white evangelicism, and gun ownership. The first three seem 
obvious as to why life expectancy would be lower, although the other ones are less 
clear...although most states with high obesity, smoking, and poverty (the South) also tend 
to own a lot of guns and are heavily evangelical.
            
            With Democratic voting, Democratic states on average to have larger urban populations,
            longer life expectancies, lower smoking rates and obesity, stronger labor unions, 
            fewer evangelicals, ESPECIALLY fewer gun owners, more college-educated, 
            and less impoverished.")
    })

    
    output$com_c2 <- renderText({
      print("While we already have the correlations, we can see the distributions of the selected variables.
            We can see that the average state has approximately a 12% poverty rate, 16% college graduates, 
            a 40% gun ownership rate (with a huge dip outside that basic range; a few states own either very 
            few guns or a lot of them), 25% white evangelicals (again with a similar pattern to gun ownership), 
            15% nonreligious, around 70% urban (almost all states are above 60%), and about 50% Democratic. 
            The latter point shows how those other points can potentially be the tipping point for how a state votes. 
            However, there are almost always outliers.")
    })
    
    pal <- colorNumeric(
      palette = "Blues",
      domain = map_data$Population
    )
    
    output$str <- renderTable({
      state_data
    })
    
    ### map part
    bins <- c(0,0.3,0.35,0.40,0.45,0.49,0.51,0.55,0.60,0.65,0.70,1)
    cpal <- colorBin(palette = "RdBu",domain = mapdt$Democratic_Vote,bins = bins)
    clabels <-sprintf("%s Vote Lean:\n %g",mapdt$NAME,mapdt$Democratic_Vote)
    
    output$map <- renderLeaflet({
      leaflet(mapdt) %>% 
        addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = TRUE))%>%
        setView(-100.00, 40.00, zoom = 4) %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.9,
                    fillColor = ~cpal(mapdt$Democratic_Vote),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    label = clabels, 
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px")) %>%
        addLegend(position= "bottomleft",pal = cpal, opacity = 0.7, values = mapdt$Democratic_Vote ,title = "Vote")
    })
    
    selectdata <- reactive({mapdt@data[input$vars]}) #
    
    observe({
      varBy <- input$vars
      proxy <- leafletProxy("map", data = mapdt)
      pal <- colorNumeric(palette = "Greens",domain = selectdata())
      labels <-sprintf("%s with %s:\n %g",mapdt$NAME,varBy, as.numeric(unlist(selectdata()))) #(selectdata()/1e6,digits = 2)
      proxy %>% 
        clearShapes() %>% clearControls() %>%
        addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                    opacity = 1.0, fillOpacity = 0.7,
                    fillColor = ~pal(selectdata()),
                    highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                    label = labels, 
                    labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px")) %>%
        addLegend(position= "bottomleft",pal = pal, opacity = 0.7, values =  as.numeric(unlist(selectdata())),title = "Scale")
    })
    mj <- list(l = 25, r = 0.5,t =5,b=40)
    output$small <- renderPlotly({
      #ggplot(selectdata())+geom_point(aes(x = as.numeric(unlist(selectdata())), y= mapdt$Democratic_Vote))+xlab(input$vars)+ylab("Democratic Lean")#+scale_color_gradient2(low ="red",high = "blue",midpoint = 0.5 ) , color = mapdt$Democratic_Vote
      plot_ly(x = as.numeric(unlist(selectdata())), y= mapdt$Democratic_Vote,color = mapdt$Democratic_Vote,colors = cpal,text =mapdt$State) %>%
        layout(xaxis = list(title=input$vars),yaxis = list(title="",tickangle=-30),margin = mj)
    })
    
    observe({
      proxy <- leafletProxy("map", data = mapdt)
      pal <- colorNumeric(palette = "Greens",domain = selectdata())
      labels <-sprintf("%s with %s:\n %g",mapdt$NAME,input$vars, as.numeric(unlist(selectdata())))
      
      if(input$BR){
        proxy %>% 
          addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.5,
                      fillColor = ~cpal(mapdt$Democratic_Vote),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      label = labels, 
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px"))}
      else{
        proxy %>% clearShapes() %>% 
          addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5,
                      opacity = 1.0, fillOpacity = 0.7,
                      fillColor = ~pal(selectdata()),
                      highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE),
                      label = labels, 
                      labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"),textsize = "15px"))
      }
    })

    # Report Part
    output$scatter3 <- renderPlotly({
      ggplot(state_data, aes(x = Poverty_Rate, y = Democratic_Vote, label = State, color = 100*Pct_Urban)) +
        geom_point() + 
        geom_smooth(method = 'lm', se = FALSE) + 
        scale_colour_gradientn("% urban", colours = c('green', 'blue')) + 
        xlab("Poverty rate")
    })
    output$com_s3 <- renderText({
      print("Despite poor people tending to vote Democratic on average, the states they are most concentrated in tend to be very Republican. 
            Also, despite the talk of 'inner city hell' and 'horrific poverty'
            in our nation's biggest cities', the poorest parts of America are mostly rural. 
            As seen, the most Democratic states have lower poverty rates and 
            are more urban (with the two large exceptions of Maine and Vermont). 
            This is possibly because more Democratic states have more generous safety nets.")
    })
    
    output$scatter4 <- renderPlotly({
      ggplot(state_data, aes(x = Pct_Obese, y = Democratic_Vote, label = State)) + 
        geom_point(aes(color = 100*state_data$Life_Exp)) + 
        geom_smooth(method = 'lm', se = FALSE) + 
        scale_colour_gradientn("Life Expectancy (years)", colours = c('red', 'purple','blue'))
    })
    
    output$com_s4 <- renderText({
      print("Similar to the other graph, obesity is correlated with Republican voting, as well as lower life expectancy.
While there are many Republican states with higher life expectency, they are the less obese ones.
Above a rate of 31%, the only states left are heavily Republican southern states with low life expectancy 
(with two Midwestern exceptions).")
    })
    
    output$scatter5 <- renderPlotly({
      ggplot(state_data, aes(x = state_data$Pct_Smoker, y = Democratic_Vote, label = State)) +
        geom_point(aes(color = Region)) +
        geom_smooth(method = 'lm', se = FALSE) +
        xlab("Smoking Rate")+
        ylab("Democratic Vote")
    })

    output$com_s5 <- renderText({
      print("Smoking rate tends to be correlated with a state being more Republican. In fact, 
it has almost the exact same pattern as obesity: It is heaviest among the poor rural Republican states and the South. 
Smoking laws tend to be much more lax in the South than in the North. Of course, that region has a history of growing tobacco,
so that may play a part.")
    })
    output$scatter6 <- renderPlotly({
      ggplot(state_data, aes(x = Pct_Urban, y = Democratic_Vote, label = State)) + 
        geom_point(aes(color = Gun_Owner_Pct)) + 
        geom_smooth(method = 'lm', se = FALSE) + 
        xlab("% Urban") + 
        ylab("Democratic Vote") + 
        scale_colour_gradientn("Gun Ownership (%)", colours = c('blue', 'purple','red')) + 
        facet_wrap(~Region)
    })
    output$com_s6 <- renderText({
      print("As shown earlier, urban populations and greatly determines a state's partisanship...
            but the strength of the relationship depends on the region. In the northeast,
            gun ownership and urban population has a minimal relationship with partisanship,
            and if anything, is slightly negatively correlated...although that is possibly caused
            by the outliers of Maine and Vermont being FAR more Democratic than typical rural,
            white states. The Midwest and West have a rather strong relationship between urban
            population and Democratic strength, and the South has a lesser one.
            Gun ownership (except in the Northeast) puts a large damper on Democratic voting,
            but since gun ownership and urban population are negatively correlated regardless of votes,
            which has a stronger effect on Democratic voting is hard to isolate.
            ")
    })
    output$heat1 <- renderPlotly({
      ggplot(state_data, aes(x = RTW, y = State)) + 
        geom_tile(aes(fill = Democratic_Vote)) + 
        scale_fill_gradientn(colours = c("red", "red", "white", "blue", "blue"))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_rect(fill = 'grey', colour = 'grey')) + ylim(rev(state_data$State))
    })
    output$com_h1 <- renderText({
      print("As we can see, states that are not right-to-work (a state that has legislation forbidding labor unions
from forcing dues on members of a unionized workplace who are not union members) tend to be more Democratic, while 
right-to-work states tend to be more Republican. Since many of these states have been right-to-work for decades, 
even in times of American history when unions were far more influential and powerful, it is unknown if unions are weak 
because they are so Republican, or they are so Republican because unions are so weak.")
    })
    
    output$heat2 <- renderPlotly({
      ggplot(state_data, aes(x = Region, y = State)) + 
        geom_tile(aes(fill = Democratic_Vote)) + 
        scale_fill_gradientn(colours = c("red", "red", "white", "blue", "blue"))+ ylim(rev(state_data$State))+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_rect(fill = 'grey', colour = 'grey')) 
    })
    
    output$com_h2 <- renderText({
      print("The Northeast votes solidly Democratic, the South tends to vote solidly Republican (except for a few swing states),
            and the other two regions tend to split their votes, at least by state. This tends to match statistics and anecdotes
            of how areas and states vote, and what types of swing states people talk about during every election cycle.")
    })
    
    # More Part
    output$team1 <- renderTable({
      team_members <- data.frame(Name = c("Jun Guo", "Jieyu Yao", "Matt Dawidowicz"),
                                 Email = c("jg3555@columbia.edu","jy2806@columbia.edu",
                                           "mjd2211@columbia.edu"),
                                 Uni = c("jg3555","jy2806","mjd2211"))
      team_members 
    })

    output$datasource <- renderTable({
      References <- data.frame(References = c("Population, % of nation","Density", "Income", "% College grad","Poverty rate", "Gini","% Benefits to Tax Ratio","Gun owner %", "% white", 
                                               "% white evangelical", "% nonreligious","% urban", "% obese", "Life expectancy", "% union","Right-to-work","Democratic Vote", "All other columns"))
      Links <- data.frame(Links = c(" https://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population","https://en.wikipedia.org/wiki/List_of_U.S._states_by_population_density","https://en.wikipedia.org/wiki/Household_income_in_the_United_States","https://en.wikipedia.org/wiki/List_of_U.S._states_by_educational_attainment","https://en.wikipedia.org/wiki/List_of_U.S._states_by_poverty_rate","https://en.wikipedia.org/wiki/List_of_U.S._states_by_Gini_coefficient","http://www.motherjones.com/politics/2011/11/states-federal-taxes-spending-charts-maps","https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state",
                "https://en.wikipedia.org/wiki/List_of_U.S._states_by_non-Hispanic_white_population","https://en.wikipedia.org/wiki/List_of_U.S._states_by_non-Hispanic_white_population","https://en.wikipedia.org/wiki/Irreligion_in_the_United_States","https://en.wikipedia.org/wiki/Urbanization_in_the_United_States","http://stateofobesity.org/adult-obesity/", "https://en.wikipedia.org/wiki/List_of_U.S._states_by_life_expectancy","https://en.wikipedia.org/wiki/Union_affiliation_by_U.S._state","https://en.wikipedia.org/wiki/Right-to-work_law","https://en.wikipedia.org/wiki/United_States_presidential_election,_2016",
                "https://analyticscampus.gallup.com/"))
      total = cbind(References,Links)
      total
    })
    
    output$ds_com1 <- renderText({
      print(" Many different things play into how a state votes: its demographics, its industries, its culture, its economy, and many other aspects. 
While finding a perfect way to predict how people vote is likely impossible, many factors are highly discussed (college education, 
race, religion), some are only sometimes mentioned (urban population, gun ownership, union power/right-to-work, and poverty), 
and some are rarely mentioned (obesity, smoking rate, and life expectancy).\n")
    })
    
    
    output$ds_com2 <- renderText({
      print("Most of these variables are visualized and compared with each other, as well as the state's lean towards the Democratic Party.
What this means is that the lean of each state relative to the national vote (2-party vote only) and then the lean is averaged together and given a percentage.
For example, if a state leaned 2, 3, 4, and 5 points to the Democratic Party relative to the nation over 4 elections, the average is 3.5, and the data point will be 53.5%, or 0.535.")
    })
    
    output$try <- renderPrint({
      writeLines(c("", " -------------- ", " Hello, World ", " --------------", "    \\",
              "      \\", "        \\", "            |\\___/|", "          ==) ^Y^ (==",
              "            \\  ^  /", "             )=*=(", "            /     \\",
              "            |     |", "           /| | | |\\", "           \\| | |_|/\\",
              "           //_// ___/", "               \\_)", "  "))
    })

    }
  
  
)