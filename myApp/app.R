library(shiny)



 ui <- fluidPage(
  tags$head(tags$style(HTML("
    body {text-align: center; background: #3C5E3E; color: #E4D7A1; font-family: 'Verdana', sans-serif;}
    .container-fluid { max-width: 800px; margin: auto; padding-top: 20px; }
    h1, h3 { text-align: center; color: #D6C17C; text-shadow: 0 0 10px rgba(246,211,101,0.5); }
    .pill-btn {
      display: block; margin: 20px auto; width: 200px; height: 200px;
      border-radius: 50%; border: none; cursor: pointer;
      font-size: 100px; background: #D6C17C;
      box-shadow: 0 8px 20px rgba(0,0,0,0.4), inset 0 -4px 10px rgba(0,0,0,0.3);
      transition: transform 0.08s;
      line-height: 200px; text-align: center; padding: 0;
    }
    .pill-btn:hover { transform: scale(1.05); }
    .cpill-btn:active { transform: scale(0.95); }
    .score { text-align: center; font-size: 2.5em; color: #D6C17C; margin: 10px 0; }
    .score-box {
      flex: 1; width: 100%; padding: 5px; margin: 6px 0; border: 1px solid #D6C17C;
      border-radius: 8px; background: #D6C17C; text-align: center;
      font-size: 1.4em; color: #3C5E3E;
    }
    .score-label { font-size: 0.6em; color: #3C5E3E; margin-bottom: 4px; }
    .stats-row { display: flex; gap: 10px; justify-content: center; margin-bottom: 20px; }
    .stat-box {
      flex: 1; max-width: 200px; padding: 5px; border: 1px solid #5F7D61;
      border-radius: 8px; background: #5F7D61; text-align: center;
      font-size: 1.4em; color: #D6C17C;
    }
    .stat-label { font-size: 0.6em; color: #D6C17C; margin-bottom: 4px; }
    .shop-title { color: #D6C17C; font-size: 1.4em; border-bottom: 2px solid #5F7D61; padding-bottom: 5px; }
    .upgrade-btn {
      width: 100%; padding: 12px 15px; margin: 6px 0; border: 1px solid #D6C17C;
      border-radius: 8px; background: #5F7D61; color: #D6C17C; cursor: pointer;
      font-size: 1em; text-align: left; transition: background 0.2s;
    }
    .upgrade-btn:hover { background: #3C5E3E; }
    .upgrade-btn:disabled { opacity: 0.4; cursor: not-allowed; }
    .upgrade-info { float: right; color: #D6C17C; font-weight: bold; }
  "))),

  h1("ClickerRx"),
  p(
    "Click the pill to produce pills, then spend them on upgrades to make even more pills!",
    tags$br(),
    "This game is made in R✨ for ",
    tags$a("CPDSE",
           href = "https://www.cpdse.dk",
           target = "_blank",
           style = "color: #A9BBAA;"),
    ".",
    style = "text-align: center; color: #E4D7A1; font-style: italic;"
  ),
  div(class = "score-box", div(class = "score-label", "You have"), textOutput("pill_count", inline = TRUE)),
  div(class = "stats-row",
      div(class = "stat-box", div(class = "stat-label", "Pills per Click"), textOutput("stat_click", inline = TRUE)),
      div(class = "stat-box", div(class = "stat-label", "Pills per Second"), textOutput("stat_pps", inline = TRUE)),
      div(class = "stat-box", div(class = "stat-label", "Total Clicks"), textOutput("stat_total", inline = TRUE))
  ),
  
  tags$button(id = "click_pill", class = "pill-btn action-button", HTML("💊")),
  
  uiOutput("phd_message"),
  
  br(),
  div(class = "shop-title", "💰 Shop"),
  
  uiOutput("shop_ui")
  
 )

 server <- function(input, output, session) {
  game_state <- reactiveValues(
    pills = 0,
    total_clicks = 0,
    click_power = 1,
    pps = 0,
    upgrades = data.frame(
      name = c(
        # Early Game
        "CPDSE", 
        "read_csv", 
        "CodeClub", 
        "ggplot2",
        
        # Mid Game
        "VLOOKUP Detox", 
        "Coding in R", 
        "R statistics Package", 
        "Degree",
        
        # Late Game
        "Degree"
      ),
      
      emoji = c(
        "🐍","📄","🤲","📊",
        "🧮","🇷","📉","💻",
        "🎓"
      ),
      
      desc = c(
        "You heard about CPDSE. Very cool!",
        "You tamed a messy dataset. Commas fear you.",
        "You join the CodeClub. There's code & cake!",
        "Mastered your first ggplot2. The Tidyverse just got a hue upgrade!",
        "You escaped Excel. There is no going back.",
        "You know some R. Colleagues think you are a wizaRd.",
        "p < 0.001. Very significant pills.",
        "You feel comfortable handling your PC. People ask you if you can also fix the printer.",
        "Finally! You are bilingual in data and drugs."
      ),
      
      base_cost = c(
        15, 50, 100, 120,   # Early
        250, 600, 1500, 5000,  # Mid
        10000  # Late
      ),
      
      pps_add = c(
        1, 0, 0, 5,
        0, 12, 40, 0,
        120
      ),
      
      click_add = c(
        0, 2, 3, 0,
        5, 0, 0, 60,
        0
      ),
      
      owned = c(0,0,0,0,0,0,0,0,0),
      stringsAsFactors = FALSE
    )
  )
  
  # raises 1.15 to the power of how many you own, creating exponential growth
  get_cost <- function(base, owned) floor(base * 1.15^owned)
  
  # Pills that are auto-generated every second
  observe({
    invalidateLater(1000, session)
    isolate({
      game_state$pills <- game_state$pills + game_state$pps
    })
  })
  
  # Pills that are generated every click
  observeEvent(input$click_pill, {
    game_state$pills <- game_state$pills + game_state$click_power
    game_state$total_clicks <- game_state$total_clicks + 1
  })
  
  # Bying upgrades
  lapply(seq_len(nrow(isolate(game_state$upgrades))), function(i) {
    observeEvent(input[[paste0("buy_", i)]], {
      cost <- get_cost(game_state$upgrades$base_cost[i], game_state$upgrades$owned[i])
      if (game_state$pills >= cost) {
        game_state$pills <- game_state$pills - cost
        game_state$upgrades$owned[i] <- game_state$upgrades$owned[i] + 1
        game_state$pps <- game_state$pps + game_state$upgrades$pps_add[i]
        game_state$click_power <- game_state$click_power + game_state$upgrades$click_add[i]
      }
    })
  })
  
  output$pill_count <- renderText({
    paste0("💊 ", formatC(floor(game_state$pills), format = "d", big.mark = ","), " pills")
  })
  
  output$stat_click <- renderText({ game_state$click_power })
  output$stat_pps <- renderText({ game_state$pps })
  output$stat_total <- renderText({ game_state$total_clicks })
  
  output$phd_message <- renderUI({
    if (game_state$upgrades$owned[9] >= 3) {
      h3("🎓 Congratulations on reaching your 3rd degree! You've earned your PhD!", 
         style = "text-align: center; color: #D6C17C; margin-top: 20px;")
    }
  })
  
  output$shop_ui <- renderUI({
    ups <- game_state$upgrades
    buttons <- lapply(1:nrow(ups), function(i) {
      cost <- get_cost(ups$base_cost[i], ups$owned[i])
      can_buy <- game_state$pills >= cost
      desc <- if (ups$pps_add[i] > 0) {
        paste0("+", ups$pps_add[i], " Pills per second")
      } else {
        paste0("+", ups$click_add[i], " Pills per click")
      }
      tags$button(
        id = paste0("buy_", i),
        class = "upgrade-btn action-button",
        disabled = if (!can_buy) "disabled" else NULL,
        HTML(paste0(
          ups$emoji[i], " ", ups$name[i],
          " <small>(owned: ", ups$owned[i], ")</small>",
          "<span class='upgrade-info'>", desc, " — Cost: ",
          formatC(cost, format = "d", big.mark = ","), " 💊</span>",
          "<br><small style='color:#E4D7A1;'>", ups$desc[i], "</small>"
        ))
      )
    })
    do.call(tagList, buttons)
  })
  
  
 }
  

# Run the application 
shinyApp(ui, server)
