
# Bevor diese App hier läuft, muss man die zwei R-Files
# "sleeper_leagues.R" und "sleeper_matchups.R" laufen lassen. 
# Diese beiden R-Files generieren jeweils eine CSV-Datei, die 
# hier in dieser App.R-Datei geladen wird. 
# 
# Du musst Dich für das "publishen" deiner App bei https://shiny.rstudio.com/
# anmelden und den Account mit RStudio verknüpfen. Das geht ziemlich easy
# aber ich kann Dir dazu nicht mehr sagen, weil ich meine Zugangsdaten
# vergessen habe :-D





#############################################
#############################################

# https://matmu.shinyapps.io/Upsidebowl/

#############################################
#############################################


# libraries:

library(tidyverse)
library(jsonlite)
library(httr)
library(tidyjson)
library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(DT)


# League IDs des Upsidebowl

# Pizza Pals <- 597038861834567680
# Nacho Nascher <- 597041380413464576
# Hot Dog Dudes <- 597040014550945792
# Bud Light Buddies <- 597034773214453760
# Codeword Cheetos <- 597043384380612608
# Cupcake Comrades <- 597041707346874368
# Chicken Wings Channel <- 597040711145152512 
# Chili Chicks <- 597041969868361728
# Dr. Pepper Division <- 597038205220470784 
# Burrito Barbaren <- 597037608735920128
# Burger Boys <- 597040374485143552
# Twinkie Town <- 597038475849555968

leagues <- c("597038861834567680", "597041380413464576",
             "597040014550945792", "597034773214453760",
             "597043384380612608", "597041707346874368",
             "597040711145152512", "597041969868361728",
             "597038205220470784", "597037608735920128",
             "597040374485143552", "597038475849555968")

###################################
# Users
################################### 


datalist = list()

# url:
url <- "https://api.sleeper.app/v1/league/"

for (i in leagues) {
    
    # Sleeper API Befehle
    users <- httr::GET(paste0(url,i,"/users")) 
    
    # Wir benoetigen den "JSON"-Text:
    jsontext <- content(users, as = "text")
    
    # Bauen wir eine Pipeline um die OwnerIDs und PlayerIDs zu ziehen:
    users_df <- jsontext %>%
        gather_array %>%              # collapses a JSON array into index-value pairs
        spread_values(ownerID = jstring("user_id"),
                      leagueID = jstring("league_id"),
                      user = jstring("display_name"),
                      avatar = jstring("avatar")) %>%     
        enter_object("metadata") %>% 
        spread_values(teamname = jstring("team_name"))
    
    # clean up:
    users_df <- as.data.frame(users_df) %>%
        select(-document.id, - array.index, -..JSON)
    
    datalist[[i]] <- users_df
    
}

users_df <- dplyr::bind_rows(datalist)
users_df$avatar <- paste0("https://sleepercdn.com/avatars/thumbs/",users_df$avatar)
users_df$avatar <- paste0("<img src=", users_df$avatar, "></img>")



###################################
# Standings
###################################  

datalist = list()


# url:
url <- "https://api.sleeper.app/v1/league/"

for (i in leagues) {
    
    # Sleeper API Befehle
    standings <- httr::GET(paste0(url,i,"/rosters")) 
    
    # Wir benoetigen den "JSON"-Text:
    jsontext <- content(standings, as = "text")
    
    standings_df <- jsontext %>%
        gather_array %>%
        spread_values(leagueID = jstring("league_id"),
                      ownerID = jstring("owner_id")) %>%
        enter_object("settings") %>%
        spread_all() %>%
        select(leagueID, ownerID, wins, ties, losses, fpts)
    
    # clean up:
    standings_df <- as.data.frame(standings_df) %>%
        select(-..JSON)
    
    datalist[[i]] <- standings_df
    
}

standings_df <- dplyr::bind_rows(datalist)



##########################
# Combining:
##########################

standings_df <- left_join(users_df, standings_df, by=c("leagueID", "ownerID"))



# id_pizza <- 597038861834567680
pizza <- standings_df %>%
    filter(leagueID == "597038861834567680") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
pizza$avatar[str_detect(pizza$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# id_nacho <- 597041380413464576
nacho <- standings_df %>%
    filter(leagueID == "597041380413464576") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
nacho$avatar[str_detect(nacho$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# id_hotdog <- 597040014550945792
hotdog <- standings_df %>%
    filter(leagueID == "597040014550945792") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
hotdog$avatar[str_detect(hotdog$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# id_bud <- 597034773214453760
bud <- standings_df %>%
    filter(leagueID == "597034773214453760") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
bud$avatar[str_detect(bud$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# id_cheetos <- 597043384380612608
cheeto <- standings_df %>%
    filter(leagueID == "597043384380612608") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
cheeto$avatar[str_detect(cheeto$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# id_cupcakes <- 597041707346874368
cupcake <- standings_df %>%
    filter(leagueID == "597041707346874368") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties)) 
cupcake$avatar[str_detect(cupcake$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'

# id_chicken <- 597040711145152512 
chickenwing <- standings_df %>%
    filter(leagueID == "597040711145152512") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties)) 
chickenwing$avatar[str_detect(chickenwing$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# id_chili <- 597041969868361728
chili <- standings_df %>%
    filter(leagueID == "597041969868361728") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
chili$avatar[str_detect(chili$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'


# id_pepper <- 597038205220470784 
drpepper <- standings_df %>%
    filter(leagueID == "597038205220470784") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
drpepper$avatar[str_detect(drpepper$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'

# id_burrito <- 597037608735920128 
burrito <- standings_df %>%
    filter(leagueID == "597037608735920128") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
burrito$avatar[str_detect(burrito$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'

# id_burger <- 597040374485143552 
burger <- standings_df %>%
    filter(leagueID == "597040374485143552") %>%
    select(avatar, user, wins, ties, losses, fpts) %>%
    arrange(desc(wins, ties))
burger$avatar[str_detect(burger$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'

# id_twinkies <-  597038475849555968
   twinkie <- standings_df %>%
       filter(leagueID == "597038475849555968") %>%
       select(avatar, user, wins, ties, losses, fpts) %>%
       arrange(desc(wins, ties))
   twinkie$avatar[str_detect(twinkie$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'

# Zwischenrunde aktuell:
   quali <- standings_df %>%
       group_by(leagueID) %>%
       arrange(desc(wins, ties)) %>%
       slice(1)  %>%
       select(avatar, user, wins, ties, losses, fpts)
   quali$leagueID <- NULL
   quali$avatar[str_detect(twinkie$avatar, "NA")] <- '<img src="Upside Bowl_II.png" height="50"></img>'   

###################################
# Load ADP-Data:

#adp_df <- load("adp.Rda")
adp_df <- read.csv("adp.csv", header = T, fileEncoding = "UTF-8")

###################################
# Load Matchup-Data:
matchups_df <- read.csv2("matchups.csv", header = T)

# need factors for dropdown
matchups_df$league <- as.factor(matchups_df$league)
matchups_df$wk <- as.factor(matchups_df$wk)
matchups_df$matchup_id <- NULL
matchups_df <- matchups_df %>%
    mutate(result = paste(fpts1, fpts2, sep = " : ")) %>%
    select(league, wk, avatar1, user1, result, user2, avatar2)
matchups_df$fpts1 <- NULL
matchups_df$fpts2 <- NULL


################################################################################



# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    
    # Application title
    #titlePanel(title = div(img(src="Upside Bowl_II.png"), "Upsidebowl")),
    titlePanel(title = div(img(src="logo.jpg", height = "200px")), "Upsidebowl"),
    uiOutput("twitter"),
    uiOutput("patreon"),
    uiOutput("podigee"),
    uiOutput("discord"),
    uiOutput("leadblogger"),

    
    
    # Navigation Bar:
    navbarPage(
        title = "Upsidebowl",
        
        # News - Reiter mit Tweets:
        tabPanel("News",                  fluidRow(column(width=12,
                                                               tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    
                                                                                     [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id))
                                                                                     {js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";
                                                                                     fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                                                               column(width=6, box(width=NULL, height=NULL,
                                                                                   a("Tweets by @Upsidefantasy", class="twitter-timeline"
                                                                                     , href = "https://twitter.com/Upsidefantasy")
                                                               ))
                  ))),

        
        
        # Zwischenrunde - Reiter mit dem aktuellen Stand:
        tabPanel("Zwischenrunde",            DT::dataTableOutput("quali")),
        
        # Qualifikationsligen - Dropdown-Menü aller Ligen:
        navbarMenu("Qualifikationsligen",
            tabPanel("Pizza Pals",           DT::dataTableOutput("Pizza")),
            tabPanel("Nacho Nascher",        DT::dataTableOutput("Nacho")),
            tabPanel("Hot Dog Dudes",        DT::dataTableOutput("HotDog")),
            tabPanel("Bud Light Buddies",    DT::dataTableOutput("Bud")),
            tabPanel("Codeword Cheetos",     DT::dataTableOutput("Cheetos")),
            tabPanel("Cupcakes Comrades",    DT::dataTableOutput("Cupcakes")),
            tabPanel("Chicken Wings Channel",DT::dataTableOutput("Chickenwings")),
            tabPanel("Chili Chicks",         DT::dataTableOutput("Chili")),
            tabPanel("Dr Pepper Division",   DT::dataTableOutput("DrPepper")),
            tabPanel("Burrito Barbaren",     DT::dataTableOutput("Burrito")),
            tabPanel("Burger Boys",          DT::dataTableOutput("Burger")),
            tabPanel("Twinkie Town",         DT::dataTableOutput("Twinkie"))
            ),
        
        # Matchups - Reiter der Paarungen mit Dropdown (Liga/Week)
        tabPanel("Matchups",         
                 
                 sidebarLayout(
                     sidebarPanel(
                         selectInput("league",
                                     label = "Choose a League",
                                     choices = c("Pizza Pals",
                                                 "Nacho Nascher",
                                                 "Hot Dog Dudes",
                                                 "Bud Light Buddies",
                                                 "Codeword Cheetos",
                                                 "Cupcake Comrades",
                                                 "Chicken Wings Channel",
                                                 "Chili Chicks",
                                                 "Dr Pepper Division",
                                                 "Burrito Barbaren",
                                                 "Burger Boys",
                                                 "Twinkie Town"
                                     )
                         ),
                         selectInput("week",
                                     label = "Choose a Week",
                                     choices = c("1",
                                                 "2",
                                                 "3",
                                                 "4",
                                                 "5",
                                                 "6",
                                                 "7",
                                                 "8",
                                                 "9",
                                                 "10",
                                                 "11",
                                                 "12",
                                                 "13",
                                                 "14",
                                                 "15",
                                                 "16"))
                         
                     ),                 DT::dataTableOutput("Matchups")
                 )
        ),
        
        
        # ADP-Draft - Reiter für die Draft-Auswertungen:
        tabPanel("ADP - Draft",            DT::dataTableOutput("ADP")),
        
        # Draftboards - Dropdown der Draftboards:
        navbarMenu("Draftboards",
                   tabPanel("Pizza Pals",           img(src="https://user-images.githubusercontent.com/39851341/92972210-26acb500-f482-11ea-897b-a02b246ca86f.JPG")),
                   tabPanel("Nacho Nascher",        img(src="https://user-images.githubusercontent.com/39851341/92972297-4d6aeb80-f482-11ea-9201-2e4b38e09e8f.JPG")),
                   tabPanel("Hot Dog Dudes",        img(src="https://user-images.githubusercontent.com/39851341/92972321-5956ad80-f482-11ea-9d68-e18f9efe3699.JPG")),
                   tabPanel("Bud Light Buddies",    img(src="https://user-images.githubusercontent.com/39851341/92972592-c407e900-f482-11ea-942f-f355451c2aa9.JPG")),
                   tabPanel("Codeword Cheetos",     img(src="https://user-images.githubusercontent.com/39851341/92972467-9de24900-f482-11ea-892c-6d3487fd3af1.JPG")),
                   tabPanel("Cupcakes Comrades",    img(src="https://user-images.githubusercontent.com/39851341/92972384-74292200-f482-11ea-83b4-3ba1f995b9be.JPG")),
                   tabPanel("Chicken Wings Channel",img(src="https://user-images.githubusercontent.com/39851341/92972444-928f1d80-f482-11ea-947b-b5947977ca41.JPG")),
                   tabPanel("Chili Chicks",         img(src="https://user-images.githubusercontent.com/39851341/92972415-84410180-f482-11ea-9065-ad956757fb94.JPG")),
                   tabPanel("Dr Pepper Division",   img(src="https://user-images.githubusercontent.com/39851341/92972360-696e8d00-f482-11ea-9932-566e3404cf76.JPG")),
                   tabPanel("Burrito Barbaren",     img(src="https://user-images.githubusercontent.com/39851341/92972506-aa66a180-f482-11ea-9c5e-7950e37d0351.JPG")),
                   tabPanel("Burger Boys",          img(src="https://user-images.githubusercontent.com/39851341/92972561-b5b9cd00-f482-11ea-8718-43d20e78e251.JPG")),
                   tabPanel("Twinkie Town",         img(src="https://user-images.githubusercontent.com/39851341/92971728-4ee7e400-f481-11ea-88e5-e4a32e5ba1c4.JPG"))
        )


))



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
# Links zu den Upside-Kanälen:
    
    url1 <- a("@Upsidefantasy", href="https://twitter.com/Upsidefantasy")
    output$twitter <- renderUI({
        tagList("Twitter:", url1)
    })
    
    url2 <- a("Upsidefantasy", href="https://www.patreon.com/upsidefantasy/posts")
    output$patreon <- renderUI({
        tagList("Patreon:", url2)
    })
    
    url3 <- a("Upsidefantasy", href="https://upsidefantasy.podigee.io/")
    output$podigee <- renderUI({
        tagList("Podigee:", url3)
    })
    
    url4 <- a("Lead-Blogger", href="https://lead-blogger.de/Kategorie/fantasy/")
    output$leadblogger <- renderUI({
        tagList("Lead-Blogger:", url4)
    })
    
    url5 <- a("Upsidefantasy", href=" https://discord.gg/RxHJV9W.")
    output$discord <- renderUI({
        tagList("Discord:", url5)
    })
    
# Standings:
    
    output$Pizza <- DT::renderDataTable({
        DT::datatable(pizza,
                      caption = "Pizza Pals",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(pizza), dom = 't'))
    })
    
    output$Nacho <- DT::renderDataTable({
        DT::datatable(nacho,
                      caption = "Nacho Nascher",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(nacho), dom = 't'))
    })
    
    output$HotDog <- DT::renderDataTable({
        DT::datatable(hotdog,
                      caption = "Hot Dog Dudes",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(hotdog), dom = 't'))
    })
    
    output$Bud <- DT::renderDataTable({
        DT::datatable(bud,
                      caption = "Bud Light Buddies",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(bud), dom = 't'))
    })
    
    output$Cheetos <- DT::renderDataTable({
        DT::datatable(cheeto,
                      caption = "Codeword Cheetos",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(cheeto), dom = 't'))
    })
    
    output$Cupcakes <- DT::renderDataTable({
        DT::datatable(cupcake,
                      caption = "Cupcakes Comrades",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(cupcake), dom = 't'))
    })
    
    output$Chickenwings <- DT::renderDataTable({
        DT::datatable(chickenwing,
                      caption = "Chicken Wings Channel",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(chickenwing), dom = 't'))
    })
    
    output$Chili <- DT::renderDataTable({
        DT::datatable(chili,
                      caption = "Chili Chicks",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(chili), dom = 't'))
    })
    
    output$DrPepper <- DT::renderDataTable({
        DT::datatable(drpepper,
                      caption = "Dr. Pepper Division",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(drpepper), dom = 't'))
    })
    
    output$Burrito <- DT::renderDataTable({
        DT::datatable(burrito,
                      caption = "Burrito Barbaren",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(burrito), dom = 't'))
    })
    
    output$Burger <- DT::renderDataTable({
        DT::datatable(burger,
                      caption = "Burger Boys",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(burger), dom = 't'))
    })
    
    output$Twinkie <- DT::renderDataTable({
        DT::datatable(twinkie,
                      caption = "Twinkie Town",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(burrito), dom = 't'))
    })
    
    
# Zwischenrunde aktueller Stand:    
    output$quali <- DT::renderDataTable({
        DT::datatable(quali,
                      caption = "Zwischenrunde aktueller Stand",
                      colnames = c("", "GM", "Wins", "Ties", "Losses", "FPTS"),
                      escape = F,
                      options = list(pageLength = nrow(quali), dom = 't'))
    })
    
# ADP-Draft:    
    output$ADP <- DT::renderDataTable({
        DT::datatable(adp_df,
                      caption = "Average Draft Positions",
                      colnames = c("ADP", "Name", "Pos", "Team", "Pos.Rank", 
                                   "Low", "High", "Times drafted"))
    })
    
# Matchups:
     output$Matchups <- DT::renderDataTable({
         DT::datatable(matchups_df %>%
                           filter(league == input$league &
                                      wk == input$week),
                       colnames = c("League", "Week", "", "Team 1", "Score", "Team 2", ""),
                       escape = F,
                       rownames = F,
                       options=list(columnDefs = list(list(visible=FALSE, targets=c(0,1))),
                                    lengthChange = FALSE,
                                    searching = FALSE,
                                    paging = FALSE,
                                    pageLength = 6)
                       )
     })
     
    
    
}




# Run the application 
shinyApp(ui = ui, server = server)









