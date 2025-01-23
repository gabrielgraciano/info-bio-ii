library(lubridate)
library(dplyr)
library(ggplot2)

#Dados para mapa----
shapefile_path <- "C:/Users/gabir/Documents/GitHub/Estagio-Curricular-_Gabriel-G/app_dengue/shapefile_br_2022/BR_UF_2022.shp"
uf_mapping <- data.frame(
  uf = c("Acre", "Alagoas", "Amapá", "Amazonas", "Bahia", "Ceará", "Distrito Federal", 
         "Espírito Santo", "Goiás", "Maranhão", "Mato Grosso", "Mato Grosso do Sul", 
         "Minas Gerais", "Pará", "Paraíba", "Paraná", "Pernambuco", "Piauí", 
         "Rio de Janeiro", "Rio Grande do Norte", "Rio Grande do Sul", "Rondônia", 
         "Roraima", "Santa Catarina", "São Paulo", "Sergipe", "Tocantins"),
  sigla = c("AC", "AL", "AP", "AM", "BA", "CE", "DF", "ES", "GO", "MA", "MT", "MS", 
            "MG", "PA", "PB", "PR", "PE", "PI", "RJ", "RN", "RS", "RO", "RR", "SC", 
            "SP", "SE", "TO")
)

#Server----
server <- function(input, output, session) {
  #bs_themer(gfonts = TRUE, gfonts_update = FALSE)
  
  #Levanta os dados a serem utilizados pela série temporal
  #Série temporal----
  data <- reactive({
    
    doenca_selecionada_st <- switch(input$doenca_st,
                                    "Dengue" = "dengue_ano",
                                    "Doença de Chagas" = "chagas_ano",
                                    "Esquistossomose" = "esquistossomose_ano",
                                    "Envenenamento por picada de cobra" = "envenenamento_picada_ano",
                                    "Febre chikungunya" = "febre_chikungunya_ano",
                                    "Hanseníase" = "hanseniase_ano",
                                    "Leishmaniose visceral" = "leishmaniose_visceral_ano",
                                    "Leishmaniose tegumentar americana" = "leishmaniose_tegumentar_ano",
                                    "Raiva humana" = "raiva_humana_ano")
    
    
    
    req(input$uf_st, input$sexo, input$faixa_etaria)
    req(input$update_st)
    
    # Definir o filtro de sexo com base na entrada do usuário
    sex_filter <- if ("Ambos" %in% input$sexo) {
      "'Masculino', 'Feminino'"
    } else {
      paste0("'", paste(input$sexo, collapse = "','"), "'")
    }
    
    # Definir o filtro de UF com base na entrada do usuário
    uf_filter <- paste0("'", paste(input$uf_st, collapse = "','"), "'")
    
    # Construir a consulta SQL para obter os casos de dengue (antes chamava query_dengue)
    query_st <- sprintf("
      SELECT 
          year,
          uf,
          sex,
          faixa_etaria,
          SUM(counting) AS total_count
      FROM 
          %s
      WHERE 
          uf IN (%s) AND
          sex IN (%s) AND
          faixa_etaria IN (%s)
      GROUP BY 
          year, uf, sex, faixa_etaria
    ", doenca_selecionada_st, uf_filter, sex_filter, paste0("'", paste(input$faixa_etaria, collapse = "','"), "'"))
    
    # Construir a consulta SQL para obter os dados populacionais
    query_population <- sprintf("
      SELECT 
          year,
          uf,
          sex,
          faixa_etaria,
          SUM(counting) AS total_count
      FROM 
          population
      WHERE 
          uf IN (%s) AND
          sex IN (%s) AND
          faixa_etaria IN (%s)
      GROUP BY 
          year, uf, sex, faixa_etaria
    ", uf_filter, sex_filter, paste0("'", paste(input$faixa_etaria, collapse = "','"), "'"))
    
    # Executar as consultas e buscar os dados
    st_data <- dbGetQuery(mysqlconnection, query_st)
    population_data <- dbGetQuery(mysqlconnection, query_population)
    
    # Unir os dois datasets em year, uf, sex, e faixa_etaria
    merged_data <- merge(st_data, population_data, by = c('year', 'uf', 'sex', 'faixa_etaria'), suffixes = c('_st', '_pop'))
    
    # Calcular a taxa de incidência por 100,000, agrupando por ano, uf e sexo
    incidence <- merged_data %>%
      group_by(year, uf, sex) %>%
      summarise(total_count_st = sum(total_count_st), total_count_pop = sum(total_count_pop)) %>%
      mutate(incidence_rate = round((total_count_st / total_count_pop) * 100000, 2)) %>%
      ungroup()
    
    # Calcular a taxa de incidência para "Ambos"
    if ("Ambos" %in% input$sexo) {
      total_incidence <- merged_data %>%
        group_by(year, uf) %>%
        summarise(total_count_st = sum(total_count_st), total_count_pop = sum(total_count_pop)) %>%
        mutate(sex = "Ambos",
               incidence_rate = round((total_count_st / total_count_pop) * 100000, 2)) %>%
        ungroup()
      
      incidence <- bind_rows(incidence, total_incidence)
    }
    
    return(incidence)
  })
  
  #plot da série temporal
  output$monthplot <- renderHighchart({
    st_cases <- data()
    
    # Verificar se há dados após a filtragem
    if (nrow(st_cases) == 0) {
      showNotification("Nenhum dado encontrado para os filtros selecionados.", type = "error")
      return(NULL)
    }
    
    # Criar rótulos de cor baseados na combinação de uf e sexo
    st_cases <- st_cases %>%
      mutate(label = paste(uf, sex, sep = ", "))
    
    # Debug: Verificar os dados após adicionar rótulos
    print("Dados após adicionar rótulos:")
    print(st_cases)
    
    # Criar uma paleta de cores Okabe-Ito
    okabe_ito_colors <- c(
      "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", 
      "#D55E00", "#CC79A7", "#999999"
    )
    
    # Definir formas específicas para cada sexo
    shape_map <- list(
      "Masculino" = "circle",
      "Feminino" = "diamond",
      "Ambos" = "triangle"  # Use a forma 'triangle' para 'Ambos'
    )
    
    # Filtrar os dados com base nos sexos selecionados
    filtered_cases <- st_cases %>% filter(sex %in% input$sexo)
    
    # Debug: Verificar os dados após a filtragem
    print("Dados após a filtragem:")
    print(filtered_cases)
    
    # Preparar dados para o Highcharter
    hc_data <- filtered_cases %>%
      group_by(label, sex) %>%
      do(data = list(
        name = unique(.$label),
        data = list_parse(data.frame(x = .$year, y = .$incidence_rate)),
        shape = shape_map[[unique(.$sex)]]
      )) %>%
      .$data
    
    # Debug: Verificar os dados preparados para o Highcharter
    print("Dados preparados para o Highcharter:")
    print(hc_data)
    
    # Garantir que hc_data seja uma lista, mesmo com um único elemento
    if (!is.list(hc_data)) {
      hc_data <- list(hc_data)
    }
    
    # Adicionar a primeira série com configuração específica
    hc <- highchart() %>%
      hc_title(text = sprintf("Incidência de %s para as UFs selecionadas", input$doenca_st)) %>%
      hc_xAxis(title = list(text = "Ano"), categories = unique(filtered_cases$year)) %>%
      hc_yAxis(title = list(text = "Taxa de Incidência por 100 mil pessoas")) %>%
      hc_plotOptions(
        series = list(lineWidth = 2)  # Aumenta a largura da linha
      ) %>%
      hc_add_series(name = hc_data[[1]]$name, data = hc_data[[1]]$data,
                    color = okabe_ito_colors[1], marker = list(symbol = hc_data[[1]]$shape)) %>%
      hc_colors(okabe_ito_colors[-1]) %>%  # Usar a paleta de cores sem o primeiro elemento
      hc_tooltip(crosshairs = TRUE, 
                 backgroundColor = "#FCFFC5",
                 shared = TRUE, 
                 borderWidth = 4) %>% 
      hc_legend(title = list(text = "UF, Sexo")) %>%
      hc_exporting(enabled = TRUE, 
                   buttons = list(contextButton = list(
                     menuItems = c('downloadPNG', 'downloadJPEG', 'downloadPDF', 'downloadSVG')
                   )))
    
    # Adicionar as demais séries
    if (length(hc_data) > 1) {
      for (i in 2:length(hc_data)) {
        hc <- hc %>%
          hc_add_series(name = hc_data[[i]]$name, data = hc_data[[i]]$data,
                        marker = list(symbol = hc_data[[i]]$shape))
      }
    }
    
    hc
  })
  
  
  #Pirâmide etária----
  #dados para a pirâmide etária
  dados_fx_et <- reactive({
    req(input$update_pe)
    
    doenca_selecionada_pe <- switch(input$doenca_pe,
                                    "Dengue" = "dengue_piramide_etaria",
                                    "Doença de Chagas" = "chagas_piramide_etaria",
                                    "Esquistossomose" = "esquistossomose_piramide_etaria",
                                    "Envenenamento por picada de cobra" = "envenenamento_picada_piramide_etaria",
                                    "Febre chikungunya" = "febre_chikungunya_piramide_etaria",
                                    "Hanseníase" = "hanseniase_piramide_etaria",
                                    "Leishmaniose visceral" = "leishmaniose_visceral_piramide_etaria",
                                    "Leishmaniose tegumentar americana" = "leishmaniose_tegumentar_piramide_etaria",
                                    "Raiva humana" = "raiva_humana_piramide_etaria")
    
    query_fx_et <- sprintf(
      "SELECT * FROM %s WHERE UF = '%s' AND year = '%s'",
      doenca_selecionada_pe, input$uf_pe, input$ano_pe
    )
    dados_fx_et <- dbGetQuery(mysqlconnection, query_fx_et)
    
    # Garantir que 'counting' seja numérico
    dados_fx_et$counting <- as.numeric(dados_fx_et$counting)
    
    levels_faixa_etaria <- c('0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39', 
                             '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74', 
                             '75-79', '80-84', '85-89', '90 ou mais')
    dados_fx_et$faixa_etaria <- factor(dados_fx_et$faixa_etaria, levels = levels_faixa_etaria, ordered = TRUE)
    
    
    # Verificar se há linhas após a consulta
    if (nrow(dados_fx_et) == 0) {
      showNotification("Nenhum dado encontrado para os parâmetros selecionados.", type = "error")
      return(NULL)
    }
    
    return(dados_fx_et)
  })
  
  # Plot da pirâmide etária
  output$piram_et <- renderPlotly({
    dados_fx_et <- dados_fx_et()
    
    # Verificar se os dados não são nulos
    req(dados_fx_et)
    
    # Verificar se há linhas após a filtragem por sexo
    dados_masculino <- dados_fx_et[dados_fx_et$sex == "Masculino", ]
    dados_feminino <- dados_fx_et[dados_fx_et$sex == "Feminino", ]
    
    if (nrow(dados_masculino) == 0 & nrow(dados_feminino) == 0) {
      showNotification("Nenhum dado encontrado para os sexos selecionados.", type = "error")
      return(NULL)
    }
    
    # Create the plotly plot directly
    piramide <- plot_ly() %>%
      add_trace(data = dados_masculino,
                x = ~-counting, y = ~faixa_etaria, type = 'bar', orientation = 'h',
                name = 'Masculino', marker = list(color = '#049899')) %>%
      add_trace(data = dados_feminino,
                x = ~-counting, y = ~faixa_etaria, type = 'bar', orientation = 'h',
                name = 'Feminino', marker = list(color = '#ed9400')) %>%
      layout(
        barmode = 'overlay',
        xaxis = list(title = 'População', showticklabels = FALSE, tickvals = seq(-max(abs(dados_fx_et$counting)), max(abs(dados_fx_et$counting)), by = 10000),
                     ticktext = abs(seq(-max(abs(dados_fx_et$counting)), max(abs(dados_fx_et$counting)), by = 10000))),
        yaxis = list(title = 'Faixa Etária'),
        title = sprintf("Pirâmide etária para o ano de %s", input$ano_pe,"em '%s'", input$uf_pe),
        legend = list(title = list(text = 'Sexo')),
        annotations = list(
          x = 1,
          y = -0.1,
          text = 'Fonte: Datasus - coleta realizada em 14/05/2024',
          showarrow = FALSE,
          xref = 'paper',
          yref = 'paper',
          xanchor = 'right',
          yanchor = 'auto',
          xshift = 0,
          yshift = 0,
          font = list(size = 10)
        )
      )
    
    piramide
  })
  
  #Barplot----
  dados_barplot <- reactive({
    req(input$uf_ed)
    req(input$update_ed)
    
    doenca_selecionada_ed <- switch(input$doenca_ed,
                                    "Dengue" = "dengue_ano",
                                    "Doença de Chagas" = "chagas_ano",
                                    "Esquistossomose" = "esquistossomose_ano",
                                    "Envenenamento por picada de cobra" = "envenenamento_picada_ano",
                                    "Febre chikungunya" = "febre_chikungunya_ano",
                                    "Hanseníase" = "hanseniase_ano",
                                    "Leishmaniose visceral" = "leishmaniose_visceral_ano",
                                    "Leishmaniose tegumentar americana" = "leishmaniose_tegumentar_ano",
                                    "Raiva humana" = "raiva_humana_ano")
    
    
    # Query
    query_barplot <- sprintf("
    select 
        year,
        uf,
        sex,
        sum(counting) as total_count
    from 
        %s
    where
        sex IN ('Masculino', 'Feminino') and
        uf = '%s' AND
        uf not in ('Ignorado')
    group by
        year, uf, sex
    order by
        year, uf, sex", doenca_selecionada_ed, input$uf_ed)
    
    dados_barplot <- dbGetQuery(mysqlconnection, query_barplot)
    
    return(dados_barplot)
  })
  
  #plot do barplot
  output$barplot_porcentagem <- renderPlot({
    dados_barplot <- dados_barplot()
    
    barplot_dados <- dados_barplot%>%
      group_by(year)%>%
      mutate(percentage = total_count/ sum(total_count) * 100)%>%
      ggplot(aes(x = factor(year), y = percentage, fill = sex))+
      geom_bar(stat = 'identity', position = 'fill')+
      scale_fill_manual(values = c('Masculino' = '#049899', 'Feminino' = '#ed9400')) + 
      scale_y_continuous(labels = scales::percent_format())+
      labs(title = paste0("Distribuição Percentual de Casos de ", input$doenca_ed, " por Ano em ", unique(dados_barplot$uf)),
           x = "Ano",
           y = "Porcentagem",
           fill = "Sexo") +
      theme_minimal()+
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1), # Rotacionar os rótulos do eixo x para melhor legibilidade
        panel.grid.major.x = element_blank(), # Remover as linhas de grade principais no eixo x
        panel.grid.minor.x = element_blank()  # Remover as linhas de grade menores no eixo x
      )
    barplot_dados
  } 
  )
  
  #Mapas---- 
  dados_mapa <- reactive({
    req(input$update_map)
    
    doenca_selecionada_map <- switch(input$doenca_map,
                                    "Dengue" = "dengue_ano",
                                    "Doença de Chagas" = "chagas_ano",
                                    "Esquistossomose" = "esquistossomose_ano",
                                    "Envenenamento por picada de cobra" = "envenenamento_picada_ano",
                                    "Febre chikungunya" = "febre_chikungunya_ano",
                                    "Hanseníase" = "hanseniase_ano",
                                    "Leishmaniose visceral" = "leishmaniose_visceral_ano",
                                    "Leishmaniose tegumentar americana" = "leishmaniose_tegumentar_ano",
                                    "Raiva humana" = "raiva_humana_ano")
    
    if (input$tipo_mapa == "Casos Absolutos") {
      query <- sprintf("
        SELECT uf, SUM(counting) AS total_count
        FROM %s 
        WHERE year = '%s' AND sex IN ('Masculino', 'Feminino')
        GROUP BY uf
      ", doenca_selecionada_map, input$ano_map)
      
      result <- dbGetQuery(mysqlconnection, query)
      
      if (nrow(result) == 0) {
        showNotification("Nenhum dado encontrado para o ano selecionado.", type = "error")
        return(NULL)
      }
      
      result <- result %>%
        left_join(uf_mapping, by = "uf") %>%
        select(sigla, total_count) %>%
        rename(SIGLA_UF = sigla)
      
      if (nrow(result) == 0) {
        showNotification("A junção dos dados falhou. Verifique os nomes das colunas.", type = "error")
        return(NULL)
      }
      
      return(result)
    } else {
      query_mapa <- sprintf("
        SELECT 
            year,
            uf,
            SUM(counting) AS total_count_mapa
        FROM 
            %s
        WHERE 
            year = '%s' AND
            sex IN ('Masculino', 'Feminino')
        GROUP BY 
            year, uf
      ", doenca_selecionada_map, input$ano_map)
      
      query_population <- sprintf("
        SELECT 
            year,
            uf,
            SUM(counting) AS total_count_pop
        FROM 
            population
        WHERE 
            year = '%s' AND
            sex IN ('Masculino', 'Feminino')
        GROUP BY 
            year, uf
      ", input$ano_map)
      
      map_data <- dbGetQuery(mysqlconnection, query_mapa)
      population_data <- dbGetQuery(mysqlconnection, query_population)
      
      merged_data <- merge(map_data, population_data, by = c('year', 'uf'), suffixes = c('_mapa', '_pop'))
      
      incidence <- merged_data %>%
        mutate(incidence_rate = round((total_count_mapa / total_count_pop) * 100000, 2)) %>%
        left_join(uf_mapping, by = "uf") %>%
        select(sigla, incidence_rate) %>%
        rename(SIGLA_UF = sigla)
      
      if (nrow(incidence) == 0) {
        showNotification("A junção dos dados falhou. Verifique os nomes das colunas.", type = "error")
        return(NULL)
      }
      
      return(incidence)
    }
  })
  
  output$mapa_incid <- renderLeaflet({
    dados <- dados_mapa()
    
    if (is.null(dados)) {
      return(NULL)
    }
    
    # Carregar shapefile das UFs do Brasil
    brasil <- st_read(shapefile_path)
    
    if (nrow(brasil) == 0) {
      showNotification("Falha ao carregar o shapefile. Verifique o caminho e o formato do arquivo.", type = "error")
      return(NULL)
    }
    
    # Simplificar o shapefile para reduzir o uso de memória (opcional)
    brasil <- ms_simplify(brasil, keep = 0.05, keep_shapes = TRUE)
    
    # Unir dados dos casos de dengue com o shapefile
    brasil_doenca <- brasil %>%
      left_join(dados, by = "SIGLA_UF")
    
    if (nrow(brasil_doenca) == 0) {
      showNotification("A junção dos dados com o shapefile falhou.", type = "error")
      return(NULL)
    }
    
    pal <- if (input$tipo_mapa == "Casos Absolutos") {
      colorBin("YlOrRd", domain = brasil_doenca$total_count, bins = 5, na.color = "transparent")
    } else {
      colorBin("YlOrRd", domain = brasil_doenca$incidence_rate, bins = 5, na.color = "transparent")
    }
    
    leaflet(data = brasil_doenca, options = leafletOptions(minZoom = 4, maxZoom = 10)) %>%
      setView(lng = -55.491477, lat = -14.235004, zoom = 4) %>%
      addTiles() %>%
      addPolygons(
        fillColor = if (input$tipo_mapa == "Casos Absolutos") {
          ~pal(total_count)
        } else {
          ~pal(incidence_rate)
        },
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = if (input$tipo_mapa == "Casos Absolutos") {
          ~paste(SIGLA_UF, ": ", total_count, " casos")
        } else {
          ~paste(SIGLA_UF, ": ", incidence_rate, " casos por 100.000 habitantes")
        },
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal, values = if (input$tipo_mapa == "Casos Absolutos") {
        ~total_count
      } else {
        ~incidence_rate
      }, opacity = 0.7, title = if (input$tipo_mapa == "Casos Absolutos") {
        "Casos"
      } else {
        "Incidência"
      },
      position = "bottomright")
  })
  
  
 
  #Barchart race----
  data_race <- reactive({
    req(input$update_br)
    
    doenca_selecionada_br <- switch(input$doenca_br,
                                    "Dengue" = "dengue_ano",
                                    "Doença de Chagas" = "chagas_ano",
                                    "Esquistossomose" = "esquistossomose_ano",
                                    "Envenenamento por picada de cobra" = "envenenamento_picada_ano",
                                    "Febre chikungunya" = "febre_chikungunya_ano",
                                    "Hanseníase" = "hanseniase_ano",
                                    "Leishmaniose visceral" = "leishmaniose_visceral_ano",
                                    "Leishmaniose tegumentar americana" = "leishmaniose_tegumentar_ano",
                                    "Raiva humana" = "raiva_humana_ano")
    
    # Query for dengue cases
    query_br <- sprintf(
      "
      SELECT year, uf, SUM(counting) AS total_count
      FROM %s
      GROUP BY year, uf
      ORDER BY year, uf;
    ", doenca_selecionada_br)
    
    dengue_data <- dbGetQuery(mysqlconnection, query_br)
    
    # Query for population data
    query_population <- "
      SELECT year, uf, SUM(counting) AS total_count
      FROM population
      GROUP BY year, uf
      ORDER BY year, uf;
    "
    
    population_data <- dbGetQuery(mysqlconnection, query_population)
    
    # Join the two datasets on year and uf
    merged_data <- merge(dengue_data, population_data, by = c("year", "uf"), suffixes = c("_dengue", "_pop"))
    
    # Calculate incidence rate per 100,000
    merged_data <- merged_data %>%
      mutate(incidence_rate = (total_count_dengue / total_count_pop) * 100000)
    
    return(merged_data)
  })
  
  #  observeEvent(input$update, {
  #   showModal(modalDialog(
  #      title = "Gerando Gráfico",
  #      "Por favor, aguarde enquanto o gráfico está sendo gerado.",
  #     footer = NULL
  #    ))
  #  })
  
  output$bar_chart_race <- renderImage({
    # Mostrar o modal de carregamento
    # runjs('$("#loading").show();')
    
    data <- data_race()
    
    # Check user input for cases or incidence
    if (input$tipo_mapa == "Casos Absolutos") {
      plot_data <- data %>%
        select(year, uf, total_count_dengue) %>%
        rename(value = total_count_dengue)
    } else {
      plot_data <- data %>%
        select(year, uf, incidence_rate) %>%
        rename(value = incidence_rate)
    }
    
    # Preparar os dados
    plot_data <- plot_data %>%
      group_by(year) %>%
      mutate(rank = rank(-value),
             Value_rel = value/value[rank==1],
             Value_lbl = paste0(" ", round(value, 0))) %>%
      ungroup()
    
    # Criar o gráfico de barras dinâmico em formato de ranking
    p <- ggplot(plot_data, aes(rank, group = uf, 
                               fill = as.factor(uf), color = as.factor(uf))) +
      geom_tile(aes(y = value/2,
                    height = value,
                    width = 0.9), alpha = 0.8, color = NA) +
      geom_text(aes(y = 0, label = paste(uf, " ")), vjust = 0.2, hjust = 1) +
      geom_text(aes(y = value, label = Value_lbl, hjust=0)) +
      coord_flip(clip = "off", expand = FALSE) +
      scale_y_continuous(labels = scales::comma) +
      scale_x_reverse() +
      guides(color = FALSE, fill = FALSE) +
      theme_minimal() +
      theme(axis.line = element_blank(),
            axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank(),
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.grid.major.x = element_line( size=.1, color="grey" ),
            panel.grid.minor.x = element_line( size=.1, color="grey" ),
            plot.title = element_text(size = 24, hjust = 0.5, face = "bold", colour = "grey", vjust = -1),
            plot.subtitle = element_text(size = 18, hjust = 0.5, face = "italic", color = "grey"),
            plot.caption = element_text(size = 8, hjust = 0.5, face = "italic", color = "grey"),
            plot.background = element_blank(),
            plot.margin = margin(2,2,2,4, "cm")) +
      transition_states(year, transition_length = 4, state_length = 1) +
      view_follow(fixed_x = TRUE)  +
      labs(title = paste0(input$tipo_mapa, '  {closest_state}'),  
           subtitle  =  "Ranking das UFs",
           caption  = "Fonte: Datasus (14/05/2024)")
    
    # Salvar a animação
    anim <- animate(p, nframes = 600, width = 800, height = 600, renderer = gifski_renderer("bar_chart_race.gif"))
    
    # Esconder o modal de carregamento
    #   runjs('$("#loading").hide();')
    removeModal()
    
    list(src = "bar_chart_race.gif", contentType = "image/gif")
  }, deleteFile = TRUE)
}


