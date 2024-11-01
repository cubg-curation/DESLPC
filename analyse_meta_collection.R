#Setup 
tic = Sys.time()
load("Data/meta_collection.rda") # A reduced version of Data/meta_collection_all_AccYear.rda only containing records which were accessioned between 1921 and 2021 (see create_meta_collection.R)
load("Data/meta_collection_all_AccYear.rda")
dir.create('Data/', showWarnings = FALSE)
dir.create('Plots/', showWarnings = FALSE)
colours = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')
run_analyses = T # Flag for whether we want to run analyses or retrieve plot data from data folder.
library(ggplot2)

#####################################################################
## FIGURE 1 - Dynamics of the meta-collection with respect to capacity, diversity, and provenance
#####################################################################
{
  #######################
  # Figure 1A - meta-collection accessions over time with regions
  #######################
  {
    if(run_analyses){
    # Reduce data to accessions.
    d = data_all
    
    ### Reorder the data such that the records that are existing are at the top and then newest item status date to oldest.
    dates = paste0(1921:2021, '-12-28')
    exist_each_year = LivingCollectionDynamics::exist_at_date(date = dates,
                                              AccessionYear = d$AccYear,
                                              ItemStatusDate = d$ItemStatusDate,
                                              ItemStatusType = d$ItemStatusType)
    
    # Extract the number of 'Items', 'Accessions', 'Taxa', 'Species', 'Genera', 'Families' in the LC each year.
    time_series_info = pbapply::pblapply(exist_each_year, function(x){
      garden_current = d[x,]
      # Number of accessions.
      # 
      unique_accessions = unique(paste0(garden_current$LC,'-', garden_current$AccNoFull))
      unique_accessions = unique_accessions[!is.na(unique_accessions)]
      no_accessions = length(unique_accessions)
      
      return( no_accessions)
    })  |> unlist() |> as.numeric()
  
    accessions_over_time = data.frame(Year = 1921:2021, accessions = time_series_info)
    save(accessions_over_time,  file = 'Data/1A.rda')
    }
    load('Data/1A.rda')
    
    p = ggplot(accessions_over_time, aes(x = Year, y = accessions)) +
      geom_point(aes(x = Year, y = accessions/10^5), col = '#f46d43', shape = 16) +
      theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      scale_x_continuous(n.breaks = 10)+
      theme(axis.text.x = element_text(angle = -45, vjust = 0.2))+
      theme(legend.position = "bottom")+
      theme(      plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),      axis.title = ggplot2::element_text(size = rel(1), face = "bold"),      axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),      legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),      legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),      strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))    )+
      theme(      axis.ticks.length=unit(0.1, "cm"),      axis.ticks.y = element_line(size=0.5,color='black'),      axis.ticks.x = element_line(size=0.5,color='black'))+
      scale_y_continuous(labels = scales::label_number())+
      labs(#title = "Change in number of accessions over time", 
        x="Year", y = "Total number of accessions  (x10*5)")
    
    ggsave(plot = p, filename = 'Plots/1A.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
    
  }
  #######################
  # Figure 1B - turnover in meta-collection
  #######################
  {
    if(run_analyses){
      data_cur = data
      data_cur$LC_AccNoFull = paste0(data_cur$LC,'-',data_cur$AccNoFull)
      unique_accessions = unique(data_cur$LC_AccNoFull)
      
      LossYear = rep(NA,nrow(data_cur))
      ItemStatusYear = as.numeric(stringr::str_extract(data_cur$ItemStatusDate,pattern = '[0-9]{4}'))
      LossYear[data_cur$ItemStatusType %in% c('NotExisting', 'Not Existing')] = ItemStatusYear[data_cur$ItemStatusType %in% c('NotExisting', 'Not Existing')]
      data_cur$LossYear = LossYear
      # Order the items where the lower indices have the most recent death date.
      LossYear_dummy = LossYear
      LossYear_dummy[is.na(LossYear_dummy)] = 4000
      ordered_items = order(LossYear_dummy,decreasing = T)
      data_cur = data_cur[ordered_items, ]
      
      #Match to the ordered accessions.
      match_to_best = match(unique_accessions,data_cur$LC_AccNoFull)
      
      #Reduce the data to unique accessions with most recent death date.
      data_cur = data_cur[match_to_best,]
      
      years = 1921:2021
      min_year = 1921 ; max_year  = 2021
      combined = data.frame(Year = years)
      
      # Items
      gain = data.frame(table(data_cur$AccYear))
      if(nrow(gain) == 0){
        gained = rep(0,nrow(combined))
        combined$gain_items = gained
      }else{
        names(gain) = c('Year', 'Items')
        gain$Year = as.numeric(as.character(gain$Year))
        gain = gain[gain$Year >=min_year & gain$Year <= max_year,]
        gained = rep(0,nrow(combined))
        gained[match(gain$Year, combined$Year)] = gain$Items
        combined$gain_items = gained
      }
      
      
      loss = data.frame(table(data_cur$LossYear))
      if(nrow(loss) == 0){
        lost = rep(0,nrow(combined))
        combined$loss_items = lost
      }else{
        names(loss) = c('Year', 'Items')
        loss$Year = as.numeric(as.character(loss$Year))
        loss = loss[loss$Year >=min_year & loss$Year <= max_year,]
        lost = rep(0,nrow(combined))
        lost[match(loss$Year, combined$Year)] = loss$Items
        combined$loss_items = lost
      }
      
      
      combined$net_items = combined$gain_items - combined$loss_items
      names(combined) = c('Year', 'gain_accessions','lost_accessions', 'net_accessions')
      meta_turnover = combined
      meta_turnover$rolling_net = zoo::rollmean(meta_turnover$net_accessions, 5, na.pad=TRUE)
      save(meta_turnover, file = 'Data/1B.rda')
    }
    load('Data/1B.rda')
  
    
    plot_data = meta_turnover[,1:3]
    plot_data = reshape(plot_data, idvar = "Year", varying = list(2:ncol(plot_data)),
                              v.names = "prop", timevar = "group", times = names(plot_data)[-1], direction = "long")
    plot_data$prop[plot_data$group == 'lost_accessions'] = plot_data$prop[plot_data$group == 'lost_accessions'] *-1
    p = ggplot(plot_data, aes(fill=group, y=prop, x=Year, color = group)) + 
      geom_bar(position="stack", stat="identity", width = 1) +
      geom_line(inherit.aes = FALSE, data = meta_turnover, aes(x = Year, y = rolling_net), linewidth = 1.05, linetype = '22', col = 'black')+  
      # geom_point(inherit.aes = FALSE, data = newNlost_net_abs, aes(x = year, y = net), size  = 1.1, col = '#848484')+
      
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      scale_x_continuous(n.breaks = 10)+
      theme(axis.text.x = element_text(angle = -45, vjust = 0.2)) +
      theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      )+
      scale_fill_manual(values = rev(c('#f46d43', '#e6e6e6'))) +
      scale_color_manual(values = rev(c('#f46d43', '#e6e6e6'))) +
      guides(color=guide_legend(title='')) + 
      labs(title="",
           x ="Year",
           y = stringr::str_wrap(paste0("Number of gained/lost/net accessions"),width = 50))+
      theme(legend.position = "none") + 
      geom_hline(yintercept=0, linetype="dashed", color = "#848484")
    
    
    ggsave(plot = p, filename = 'Plots/1B.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
  }
  #######################
  # Figure 1C - accessions and PD 
  #######################
  {
    if(FALSE){ # Slow so don't run for the example
      if(run_analyses){
        d = data_all
        dates = paste0(1921:2021, '-12-28')
        exist_each_year =LivingCollectionDynamics::exist_at_date(date = dates,
                                                                 AccessionYear = d$AccYear,
                                                                 ItemStatusDate = d$ItemStatusDate,
                                                                 ItemStatusType = d$ItemStatusType)
        
        # Extract the number of 'Items', 'Accessions', 'Taxa', 'Species', 'Genera', 'Families' in the LC each year.
        all_genera = d$POWO_taxon_name |>  stringr::str_extract('^[A-za-z]*')  |> unique()
        all_genera= all_genera[all_genera != '']
        all_genera = all_genera[!is.na(all_genera)]
        time_series_info = pbapply::pblapply(exist_each_year, function(x){
          garden_current = d[x,]
          
          genera_cur = garden_current$POWO_taxon_name |>  stringr::str_extract('^[A-za-z]*')  |> unique()
          exist_vector = rep(0,length(all_genera))
          exist_vector[all_genera %in% genera_cur] = 1
          exist_vector
        })  |> data.frame() |> t() |> data.frame()
        names(time_series_info) = all_genera
        
        pd_vals = picante::pd(time_series_info[1:10,1:10], phy)
        
        pd_over_time =data.frame(Year = 1921:2021, pd =  pd_val$PD)
        
        # Extract the number of 'Items', 'Accessions', 'Taxa', 'Species', 'Genera', 'Families' in the LC each year.
        time_series_info = pbapply::pblapply(exist_each_year, function(x){
          garden_current = d[x,]
          # Number of accessions.
          # 
          unique_accessions = unique(paste0(garden_current$LC,'-', garden_current$AccNoFull))
          unique_accessions = unique_accessions[!is.na(unique_accessions)]
          no_accessions = length(unique_accessions)
          
          return( no_accessions)
        })  |> unlist() |> as.numeric()
        
        accessions_over_time = data.frame(Year = 1921:2021, accessions = time_series_info)
        
        pd_acc_data = data.frame(Year = 1921:2021,
                                 accession = accessions_over_time$accessions/max(accessions_over_time$accessions),
                                 pd =  pd_over_time$pd/max(pd_over_time$pd))
        
        save(pd_acc_data,  file = 'Data/1C.rda')
      }
      load('Data/1C.rda')
      
      pd_acc_data_long = reshape(pd_acc_data, idvar = "Year", varying = list(2:ncol(pd_acc_data)),
                                 v.names = "prop", timevar = "group", times = names(pd_acc_data)[-1], direction = "long")
      
      p <- ggplot(data = pd_acc_data_long, aes(x=Year, y=prop, fill=group)) +
        geom_point(shape =21,size = 2)+
        theme_minimal()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        theme(axis.line = element_line())+
        scale_x_continuous(n.breaks = 10)+
        scale_fill_manual(values = c("pd"="#f46d43","accession"="#e6e6e6"))+
        theme(      plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),      axis.title = ggplot2::element_text(size = rel(1), face = "bold"),      axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),      legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),      legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),      strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))    )+
        theme(      axis.ticks.length=unit(0.1, "cm"),      axis.ticks.y = element_line(size=0.5,color='black'),      axis.ticks.x = element_line(size=0.5,color='black'))+
        theme(axis.text.x = element_text(angle = -45, vjust = 0.2)) +
        theme(legend.position = c(0.2,0.95))+
        theme(legend.title = element_blank())+
        labs(#title = "", 
          x="Year", y = "% of maximum")
      
      ggsave(plot = p, filename = 'Plots/1C.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
      
    }
    }
   
  #######################
  # Figure 1D - all accessions by provenance over time
  #######################
  {
    if(run_analyses){
      dates = paste0(1921:2021,'-12-28')
      exist_each_year = LivingCollectionDynamics::exist_at_date(date = dates,
                                                                AccessionYear = data$AccYear,
                                                                ItemStatusDate = data$ItemStatusDate,
                                                                ItemStatusType = data$ItemStatusType)
      provenance =  pbapply::pblapply(exist_each_year, function(x){
        meta_year = data[x, ] ; meta_year = meta_year[match(unique(meta_year$AccNoFull),meta_year$AccNoFull ),]
        prov_val = table(meta_year$ProvenanceCode)
        prov_val = as.numeric(prov_val[match(c('Wild','Wild-derived', 'Garden', 'Unknown'), names(prov_val))])
        prov_val[is.na(prov_val)] = 0
        prov_val/sum(prov_val)*100
      }) |> data.frame() |> t() |> data.frame()
      provenance = data.frame(year = 1921:2021, provenance)
      names(provenance) = c('Year','Wild','Wild-derived', 'Garden', 'Unknown')
      
      provenance_long = reshape(provenance, idvar = "Year", varying = list(2:ncol(provenance)),
                                v.names = "prop", timevar = "group", times = names(provenance)[-1], direction = "long")
      provenance_long$rolling = c(zoo::rollmean(provenance_long$prop[1:101], 5, na.pad=TRUE),
                                  zoo::rollmean(provenance_long$prop[102:202], 5, na.pad=TRUE), 
                                  zoo::rollmean(provenance_long$prop[203:303], 5, na.pad=TRUE),
                                  zoo::rollmean(provenance_long$prop[304:404], 5, na.pad=TRUE))
      provenance_long$group = factor(provenance_long$group, levels = c('Wild', 'Wild-derived', 'Garden', 'Unknown'))
      save(provenance, provenance_long, file = 'Data/1D.rda')
    }
    
    load('Data/1D.rda') 
    
    colours = c('#f46d43', '#848484', '#50c1dc', '#e6e6e6')
    colours_alpha = as.character(LivingCollectionDynamics::add_alpha(colours, alpha = 0.5))
    
    
    p = ggplot(provenance_long, aes(x = Year, y = rolling, group = group, color = group)) +
      geom_point(inherit.aes = FALSE,data = provenance, aes(x = Year, Wild), col = colours_alpha[1], shape = 16)+
      geom_point(inherit.aes = FALSE,data = provenance, aes(x = Year, y=`Wild-derived`), col = colours_alpha[2], shape = 16)+
      geom_point(inherit.aes = FALSE,data = provenance, aes(x = Year, Garden), col = colours_alpha[3], shape = 16)+
      geom_point(inherit.aes = FALSE,data = provenance, aes(x = Year, Unknown), col = colours_alpha[4], shape = 16)+
      geom_line(linewidth = 1.1)+
      guides(color=guide_legend(title='')) + 
      scale_color_manual(values = colours)+
      labs(title="",
           x ="Year",
           y = "% of maximum") +
      
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      scale_x_continuous(n.breaks = 10)+
      theme(axis.text.x = element_text(angle = -45, vjust = 0.2)) +
      theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.9, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3)
      )
    
    
    
    ggsave(plot = p, filename = 'Plots/1D.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
  }
  #######################
  # Figure 1E - new accessions by provenance over time
  #######################
  {
    if(run_analyses){
      # reduce to needed data.
      d = data[c('LC','AccNoFull', 'AccYear', 'ProvenanceCode')]
      d$LC_ACC = paste0(d$LC, '-', d$AccNoFull)
      d = d[match(unique(d$LC_ACC), d$LC_ACC),]
      
      # new accessions each year.
      new_provenance =  pbapply::pblapply(1921:2021, function(x){
        meta_year = d[d$AccYear == x, ]
        prov_val = table(meta_year$ProvenanceCode)
        prov_val = as.numeric(prov_val[match(c('Wild','Wild-derived', 'Garden', 'Unknown'), names(prov_val))])
        prov_val[is.na(prov_val)] = 0
        prov_val
      }) |> data.frame() |> t() |> data.frame()
      new_provenance = data.frame(year = 1921:2021, new_provenance)
      names(new_provenance) = c('Year','Wild','Wild-derived', 'Garden', 'Unknown')
      
      #Change to long format
      provenance_long = reshape(new_provenance, idvar = "Year", varying = list(2:ncol(new_provenance)),
                                v.names = "prop", timevar = "group", times = names(new_provenance)[-1], direction = "long")
      provenance_long$rolling = c(zoo::rollmean(provenance_long$prop[1:101], 5, na.pad=TRUE),
                                  zoo::rollmean(provenance_long$prop[102:202], 5, na.pad=TRUE), 
                                  zoo::rollmean(provenance_long$prop[203:303], 5, na.pad=TRUE),
                                  zoo::rollmean(provenance_long$prop[304:404], 5, na.pad=TRUE))
      provenance_long$group = factor(provenance_long$group, levels = c('Wild', 'Wild-derived', 'Garden', 'Unknown'))
      
      save(new_provenance, provenance_long, file = 'Data/1E.rda')
    }
    
    load('Data/1E.rda') 
    
    colours = c('#f46d43', '#848484', '#50c1dc', '#e6e6e6')
    colours_alpha = as.character(LivingCollectionDynamics::add_alpha(colours, alpha = 0.5))
    
    
    p = ggplot(provenance_long, aes(x = Year, y = rolling, group = group, color = group)) +
      geom_point(inherit.aes = FALSE,data = new_provenance, aes(x = Year, Wild), col = colours_alpha[1], shape = 16)+
      geom_point(inherit.aes = FALSE,data = new_provenance, aes(x = Year, y=`Wild-derived`), col = colours_alpha[2], shape = 16)+
      geom_point(inherit.aes = FALSE,data = new_provenance, aes(x = Year, Garden), col = colours_alpha[3], shape = 16)+
      geom_point(inherit.aes = FALSE,data = new_provenance, aes(x = Year, Unknown), col = colours_alpha[4], shape = 16)+
      geom_line(linewidth = 1.1)+
      guides(color=guide_legend(title='')) + 
      scale_color_manual(values = colours)+
      labs(title="",
           x ="Year",
           y = "Number of new accessions per annum") +
      scale_x_continuous(n.breaks = 10)+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(angle = -45, vjust = 0.2),
        legend.title = element_blank(),
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'),
        legend.position = c(0.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3)
      )
    
    
    ggsave(plot = p, filename = 'Plots/1E.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
  }
  #######################
  # Figure 1F - new accessions by native status over time
  #######################
  {
    if(run_analyses){
      # reduce to needed data.
      d = data[c('LC','AccNoFull', 'AccYear', 'native')]
      d$LC_ACC = paste0(d$LC, '-', d$AccNoFull)
      d = d[match(unique(d$LC_ACC), d$LC_ACC),]
      
      # new accessions each year.
      new_native =  pbapply::pblapply(1921:2021, function(x){
        meta_year = d[d$AccYear == x, ]
        native_val = table(meta_year$native)
        native_val = as.numeric(native_val[match(c('Native', 'Not Native'), names(native_val))])
        native_val[is.na(native_val)] = 0
        native_val
      }) |> data.frame() |> t() |> data.frame()
      new_native = data.frame(year = 1921:2021, new_native)
      names(new_native) = c('Year','Native', 'Not Native')
      
      #Change to long format
      native_long = reshape(new_native, idvar = "Year", varying = list(2:ncol(new_native)),
                            v.names = "number", timevar = "group", times = names(new_native)[-1], direction = "long")
      native_long$rolling = c(zoo::rollmean(native_long$number[1:101], 5, na.pad=TRUE),
                              zoo::rollmean(native_long$number[102:202], 5, na.pad=TRUE)
      )
      native_long$group = factor(native_long$group, levels = c('Native', 'Not Native'))
      
      save(new_native, native_long, file = 'Data/1F.rda')
    }
    
    load('Data/1F.rda') 
    
    colours = c('#f46d43',  '#50c1dc')
    colours_alpha = as.character(LivingCollectionDynamics::add_alpha(colours, alpha = 0.5))
    
    
    p = ggplot(native_long, aes(x = Year, y = rolling, group = group, color = group)) +
      geom_point(inherit.aes = FALSE,data = new_native, aes(x = Year, Native), col = colours_alpha[1], shape = 16)+
      geom_point(inherit.aes = FALSE,data = new_native, aes(x = Year, y=`Not Native`), col = colours_alpha[2], shape = 16)+
      geom_line(linewidth = 1.1)+
      guides(color=guide_legend(title='')) + 
      scale_color_manual(values = colours)+
      labs(title="",
           x ="Year",
           y = "Number of new accessions per annum") +
      scale_x_continuous(n.breaks = 10)+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(),
        axis.text.x = element_text(angle = -45, vjust = 0.2),
        legend.title = element_blank(),
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'),
        legend.position = c(0.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(3, 3, 3, 3)
      )
    
    colours = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')
    ggsave(plot = p, filename = 'Plots/1F.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
  }
}

#####################################################################
## FIGURE 2 - The dynamics of threatened plant collections
#####################################################################
{
  #######################
  # Figure 2A - Threatened and non-threatened over time
  #######################
  {
    # create the data
    if(run_analyses){
    data_cur = data_all
    data_cur = data_cur[!grepl('0|5|6',data_cur$taxon_type),]
    dates = paste0(1921:2021, '-12-28')
    exist_each_year = LivingCollectionDynamics::exist_at_date(date = dates,
                                              AccessionYear = data_cur$AccYear,
                                              ItemStatusDate = data_cur$ItemStatusDate,
                                              ItemStatusType = data_cur$ItemStatusType)
    
    threat_count = lapply(exist_each_year,function(year){
      d = data_cur[which(year),]
      # d = d[match(unique(d$best_name), d$best_name),]
      threatened = sum(d$threatened == 'Threatened')
      non_threatened = sum(d$threatened == 'Not Threatened')
      c(threatened,non_threatened)
    }) |> data.frame() |> t() |> data.frame()
    
    plot_data = data.frame(year = 1921:2021, threat_count)
    names(plot_data) = c('Year', 'Threatened', 'Not Threatened')
    plot_data$Threatened = plot_data$Threatened / max(plot_data$Threatened)*100
    plot_data$`Not Threatened` = plot_data$`Not Threatened` / max(plot_data$`Not Threatened`)*100
    plot_data_long = reshape(plot_data, idvar = "Year", varying = list(2:ncol(plot_data)),
                 v.names = "count", timevar = "group", times = names(plot_data)[-1], direction = "long")
    plot_data_long$group = factor(plot_data_long$group, levels = c('Threatened', 'Not Threatened'))
    save(plot_data, plot_data_long, file = 'Data/2A.rda')
    }
    load('Data/2A.rda')
    
    p <- ggplot(data = plot_data_long, aes(x=Year, y=count, color=group)) +
      geom_point(size = 2)+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      scale_x_continuous(n.breaks = 10)+
      theme(axis.text.x = element_text(angle = -45, vjust = 0.2)) +
      theme(legend.position = c(0.2,0.95))+
      theme(legend.title = element_blank())+ 
      theme(      plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),      axis.title = ggplot2::element_text(size = rel(1), face = "bold"),      axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),      legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),      legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),      strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))    )+
      theme(      axis.ticks.length=unit(0.1, "cm"),      axis.ticks.y = element_line(size=0.5,color='black'),      axis.ticks.x = element_line(size=0.5,color='black'))+
      scale_color_manual(values=colours[c(1,4)], name="")+
      labs(#title = "", 
        x="Year", y = "% of maximum")
  
    ggsave(p, filename = paste('Plots/2A.pdf'), width = 10*.75, height = 6*.75)
    
  }
  
  #######################
  # Figure 2B - Threatened, non-threatened, endemic and widespread over time
  #######################
  {
    # create the data
    if(run_analyses){
      data_cur = data_all
      data_cur = data_cur[!grepl('0|5|6',data_cur$taxon_type),]
      dates = paste0(1921:2021, '-12-28')
      exist_each_year = LivingCollectionDynamics::exist_at_date(date = dates,
                                                                AccessionYear = data_cur$AccYear,
                                                                ItemStatusDate = data_cur$ItemStatusDate,
                                                                ItemStatusType = data_cur$ItemStatusType)
      
      threat_count = lapply(exist_each_year,function(year){
        d = data_cur[which(year),]
        # d = d[match(unique(d$best_name), d$best_name),]
        threatened = sum(d$threatened == 'Threatened')
        non_threatened = sum(d$threatened == 'Not Threatened')
        endemic = sum(d$endemic == 'Endemic')
        widespread = sum(d$endemic == 'Widespread')
        c(threatened,non_threatened, endemic, widespread)
      }) |> data.frame() |> t() |> data.frame()
      
      plot_data = data.frame(year = 1921:2021, threat_count)
      names(plot_data) = c('Year', 'Threatened', 'Not Threatened', 'Endemic', 'Widespread')
      plot_data$Threatened = plot_data$Threatened / max(plot_data$Threatened)*100
      plot_data$`Not Threatened` = plot_data$`Not Threatened` / max(plot_data$`Not Threatened`)*100
      plot_data$`Endemic` = plot_data$`Endemic` / max(plot_data$`Endemic`)*100
      plot_data$`Widespread` = plot_data$`Widespread` / max(plot_data$`Widespread`)*100
      
      plot_data_long = reshape(plot_data, idvar = "Year", varying = list(2:ncol(plot_data)),
                               v.names = "count", timevar = "group", times = names(plot_data)[-1], direction = "long")
      plot_data_long$group = factor(plot_data_long$group, levels = c('Threatened', 'Not Threatened', 'Endemic', 'Widespread'))
      save(plot_data, plot_data_long, file = 'Data/2B.rda')
    }
    load('Data/2B.rda')
    
    p <- ggplot(data = plot_data_long, aes(x=Year, y=count, color=group)) +
      geom_point(size = 2)+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      scale_x_continuous(n.breaks = 10)+
      theme(axis.text.x = element_text(angle = -45, vjust = 0.2)) +
      theme(legend.position = c(0.2,0.95))+
      theme(legend.title = element_blank())+ 
      theme(      plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),      axis.title = ggplot2::element_text(size = rel(1), face = "bold"),      axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),      legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),      legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),      strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))    )+
      theme(      axis.ticks.length=unit(0.1, "cm"),      axis.ticks.y = element_line(size=0.5,color='black'),      axis.ticks.x = element_line(size=0.5,color='black'))+
      scale_color_manual(values=colours[c(1,4,2,3)], name="")+
      labs(#title = "", 
        x="Year", y = "% of maximum")
    
    ggsave(p, filename = paste('Plots/2B.pdf'), width = 10*.75, height = 6*.75)
    
  }
  #######################
  # Figure 2C - Number of threatened taxa in IUCN redlist over time and in collections
  #######################
  {
    # create the data
    if(run_analyses){
      
      excel_threatened_hist = readxl::read_xlsx('Data/RedList_threatened_history.xlsx')
      excel_threatened_hist = excel_threatened_hist[!is.na(excel_threatened_hist$`First Assessed As Threatened`),]
      years = 1978:2021
      years_for_threatened = pbapply::pblapply(excel_threatened_hist$`Threatened History`, function(year_range){
        unique(unlist(lapply(stringr::str_split(year_range, ', ')[[1]] |> stringr::str_extract_all(pattern = '[0-9]{4}'), function(x){if(length(x) == 2){x[1]:x[2]}else{x[1]}})))
      })
     
     details = pbapply::pblapply(years,function(year){
       threatened_in_year_index = unlist(lapply(years_for_threatened, function(x){year %in% x}))
       total = sum(threatened_in_year_index)
       taxa = excel_threatened_hist$`Scientific Name`[threatened_in_year_index]
       return(list(total = total, taxa = taxa))
     }) 
      names(details) = paste0('Year:', years)
    
    total_IUCN = as.numeric(unlist(lapply(details, function(x){x$total})))
    
    
    dates = paste0(1978:2021, '-12-28')
    exist_each_year = LivingCollectionDynamics::exist_at_date(date = dates,
                                              AccessionYear = data$AccYear,
                                              ItemStatusDate = data$ItemStatusDate,
                                              ItemStatusType = data$ItemStatusType)
    total_meta = rep(0, length(total_IUCN))
    for(i in 1:length(details)){
      meta_year = data[exist_each_year[,i],]
      meta_taxa_year = unique(c(meta_year$sanitised_taxon, meta_year$POWO_taxon_name, meta_year$redList_match_taxon_name))
      total_meta[i] = sum(details[[i]]$taxa %in%  meta_taxa_year)
    }
  
    plot_data = data.frame(year = years, IUCN_redList = total_IUCN, meta = total_meta)
    names(plot_data) = c('Year', 'Globally', "In LCs")
    dd = reshape(plot_data, idvar = "Year", varying = list(2:ncol(plot_data)),
                 v.names = "count", timevar = "group", times = names(plot_data)[-1], direction = "long")
    dd$group = factor(dd$group, levels = c('Globally', "In LCs"))
    save(dd, file = 'Data/2C.rda')
    }
    load('Data/2C.rda')
    
   
    # Use local regression
    p = ggplot(dd, aes(x = Year, y = count, color = group)) +
      geom_point() +
      scale_color_manual(values = colours[1:2])+
      geom_smooth(method = "loess", se = FALSE) +
      scale_y_continuous(limits = c(0,max(dd$count)))+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.2, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(2, 2, 2, 2)
      )+
      guides(color=guide_legend(title='')) + 
      labs(title="",
           x ="Year",
           y = "Number of threatened taxa")
    p
    
    ggsave(p, filename = paste('Plots/2C.pdf'), width = 10*.75, height = 6*.75)
    
    
  }
  #######################
  # Figure 2D - Accessioning surrounding first red list threatened status
  #######################
  {
    # create the data
    if(run_analyses){
      excel_threatened_hist = readxl::read_xlsx('Data/RedList_threatened_history.xlsx')
      wanted_ids = excel_threatened_hist$ID[which(excel_threatened_hist$`First Assessed As Threatened` %in% 2000:2010 & !grepl(',', excel_threatened_hist$`Threatened History`))] |> as.numeric()
      year_diff  =10
      no_species_considered = length(wanted_ids)
      
      ids = wanted_ids
      extract_wanted = lapply(ids, function(id){
        # Get the element in assessment history with matching id.
        i = match(id, excel_threatened_hist$ID)
        excel_threatened_hist$`Scientific Name`[i]
        # Get scientific name and authority.
        scientific_name = excel_threatened_hist$`Scientific Name`[i]

        # Get information from assessment_history
        first_ass_year = excel_threatened_hist$`First Assessed As Threatened`[i]

        wanted_index = unique(c(which(data$sanitised_taxon == scientific_name), 
                                which(data$POWO_taxon_name == scientific_name),
                                which(data$redList_match_taxon_name == scientific_name)
                                ))
        data_cur = data[wanted_index,]
        
        ########################
        # Use data_cur to get the number of accessions for year_diff range around assessment year.
        year_use = first_ass_year
        years = (year_use-year_diff):(year_use+year_diff)
        access_per_year = data.frame(Year = years,
                                     No_items = rep(0,length(years)))
        # Uncomment if you want Items instead of accessions
        # table_items =table(data_cur$AccYear)
        # Accessions
        data_cur_acc = data_cur[match(unique(data_cur$AccNoFull), data_cur$AccNoFull ),]
        table_items =table(data_cur_acc$AccYear)
        
        for(i in 1:nrow(access_per_year)){
          if(access_per_year$Year[i] %in% names(table_items)){
            access_per_year$No_items[i] = as.numeric(table_items[match(access_per_year$Year[i], names(table_items))])
          }
        }
        access_per_year$Year = access_per_year$Year - year_use
        ########################
        # Also return the data used to create the number of accessions per year.
        return_data = data_cur_acc[data_cur_acc$AccYear %in%years, ]
        
        return(list(access_per_year = access_per_year$No_items, data = return_data, assessment_year = first_ass_year, scientific_name = scientific_name))
      }) 

      no_accessions = lapply(extract_wanted, function(x){x$access_per_year}) |> data.frame() |> t() |> data.frame()
      rownames(no_accessions) = 1:nrow(no_accessions)
      names(no_accessions) = -year_diff:year_diff
      
      plot_data=data.frame(net_year = -year_diff:year_diff, total_accessions = colSums(no_accessions))
      save(plot_data, file = 'Data/2D.rda')
    }
    load('Data/2D.rda')
    
    fit = t.test(x = plot_data$total_accessions[1:10],
           y = plot_data$total_accessions[12:21],
           alternative = "two.sided")
    
    summary(plot_data$total_accessions[1:10]) ;  sd(plot_data$total_accessions[1:10])
    summary(plot_data$total_accessions[12:21]) ;  sd(plot_data$total_accessions[12:21])
   
   ( mean(plot_data$total_accessions[1:10]) - mean(plot_data$total_accessions[12:21]) )/ (sqrt((sd(plot_data$total_accessions[1:10])**2 + sd(plot_data$total_accessions[12:21])**2 )/ 2))
    
    mean_prior = mean(plot_data$total_accessions[plot_data$net_year < 0])
    mean_after = mean(plot_data$total_accessions[plot_data$net_year > 0])
    p = ggplot() + 
      geom_line(linewidth = 1.1, data = plot_data, aes(x=net_year, y=total_accessions, col = '#f46d43')) +
      geom_vline(xintercept=0, linetype="dashed", color = "black") +
      geom_point(data = plot_data, aes(x=net_year, y=total_accessions, col = '#f46d43'), size = 2) + # Border color
      labs(title="",
           y = paste0('Number of accessions'),
           x = "Years from first threatened assessment") +
      geom_segment(aes(x = -10, y = mean_prior, xend = 0, yend = mean_prior),col = 'black', linewidth = 1.1) +
      geom_segment(aes(x = 0, y = mean_after, xend = 10, yend = mean_after), col = 'black', linewidth = 1.1) +
      labs(caption = "") + 
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background = element_blank(), axis.line = element_line(colour = "black")) +
      theme(legend.position = "none")
    
    p
    
    ggsave(plot = p, filename = 'Plots/2D.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
  }
  #######################
  # Figure 2E - Provenance of new threatened accessions
  #######################
  {
    # create the data
    if(run_analyses){
      df = data[c('AccNoFull', 'AccYear','threatened', 'LC', 'ProvenanceCode')]
      df = df[df$threatened == 'Threatened',] ; df = df[,-3]
      df$acc_LC = paste0(df$LC,'-',df$AccNoFull)
      df = df[,c(2,4,5)]
      df2 = dplyr::distinct(df)
      df2 = df2[df2$AccYear > 1977,]
      
      # summarise by counting the number of accessions per year in each category
      df_new_acc_prov_total <- df2 |> 
        group_by(AccYear, ProvenanceCode) |> 
        count()
      df_new_acc_prov_total$AccYear = as.numeric(df_new_acc_prov_total$AccYear)
      
      Year = sort(unique(df_new_acc_prov_total$AccYear),decreasing = F)
      Garden = rep(0,length(Year)) ; Wild = rep(0,length(Year)) ; Unknown = rep(0,length(Year)) ; Wild.derived = rep(0,length(Year))
      df = as.data.frame(df_new_acc_prov_total[which(df_new_acc_prov_total$ProvenanceCode == 'Garden'),])
      Garden[match(df$AccYear, Year)] = df$n
      df = as.data.frame(df_new_acc_prov_total[which(df_new_acc_prov_total$ProvenanceCode == 'Wild'),])
      Wild[match(df$AccYear, Year)] = df$n
      df = as.data.frame(df_new_acc_prov_total[which(df_new_acc_prov_total$ProvenanceCode == 'Wild-derived'),])
      Wild.derived[match(df$AccYear, Year)] = df$n
      df = as.data.frame(df_new_acc_prov_total[which(df_new_acc_prov_total$ProvenanceCode == 'Unknown'),])
      Unknown[match(df$AccYear, Year)] = df$n
      
      Provenance = data.frame(Year, Wild, Wild.derived, Garden, Unknown )
      names(Provenance) = c('Year', 'Wild', 'Wild-derived', 'Garden', 'Unknown')
      dd = reshape(Provenance, idvar = "Year", varying = list(2:ncol(Provenance)),
                   v.names = "count", timevar = "group", times = names(Provenance)[-1], direction = "long")
      dd$group = factor(dd$group, levels = c('Wild', 'Wild-derived', 'Garden', 'Unknown'))
      
      dd$rolling = c(zoo::rollmean(Provenance$Wild, 5, na.pad=TRUE),
                     zoo::rollmean(Provenance$`Wild-derived`, 5, na.pad=TRUE), 
                     zoo::rollmean(Provenance$Garden, 5, na.pad=TRUE),
                     zoo::rollmean(Provenance$Unknown, 5, na.pad=TRUE)
      ) 
      
      Provenance_prop = Provenance
      Provenance_prop[,-1] =   Provenance[,-1]/ rowSums(Provenance[,-1])*100
      dd_prop = reshape(Provenance_prop, idvar = "Year", varying = list(2:ncol(Provenance_prop)),
                   v.names = "count", timevar = "group", times = names(Provenance_prop)[-1], direction = "long")
      dd_prop$group = factor(dd_prop$group, levels = c('Wild', 'Wild-derived', 'Garden', 'Unknown'))
      
      dd_prop$rolling = c(zoo::rollmean(Provenance_prop$Wild, 5, na.pad=TRUE),
                     zoo::rollmean(Provenance_prop$`Wild-derived`, 5, na.pad=TRUE), 
                     zoo::rollmean(Provenance_prop$Garden, 5, na.pad=TRUE),
                     zoo::rollmean(Provenance_prop$Unknown, 5, na.pad=TRUE)
      ) 
      save(dd,dd_prop, file = 'Data/2E.rda') 
    }
    load('Data/2E.rda')
    
    dd$group = factor(dd$group, levels = c('Unknown', 'Garden','Wild', 'Wild-derived'))
    
    p = ggplot(dd, aes(fill=group, y=count, x=Year)) + 
      geom_bar(position="stack", stat="identity") +
      scale_fill_manual(values=colours[c(4,2,1,3)], name="")+
      scale_x_continuous(n.breaks = 10)+
      theme_minimal()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(),
            plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
            axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
            axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
            # legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
            legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
            strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0)),
            axis.ticks.length=unit(0.1, "cm"),
            axis.ticks.y = element_line(size=0.5,color='black'),
            axis.ticks.x = element_line(size=0.5,color='black'),
            axis.text.x = element_text(angle = -45, vjust = 0.2),
            legend.position = 'top',
            legend.direction = 'horizontal',
            legend.title = element_blank()
            )+
      labs(#title = "Change in number of accessions over time", 
        x='Year', y = "Proportion of new accessions")
    
    ggsave(plot = p, filename = 'Plots/2E.pdf', device = 'pdf',width = 10*.75, height = 6*.75)
  }
  #######################
  # Figure 2F - % of new accessions that are threatened in the meta-collection since 1980.
  #######################
  {
    # Get the data for the plot.
    if(run_analyses){
      df = data[c("AccYear", "threatened", "AccNoFull", "LC")]
      
      # Only consider accessions after 1980.
      df <- df |> filter(AccYear>=1980)
      
      # keep only one item per accession per collection
      df2<-dplyr::distinct(df)
      
      # remove unnecessary columns
      df2$AccNoFull<-NULL ; df2$LC<-NULL
      
      # summarise by counting the number of accessions per year in each category
      df2_to_plot_red <- df2 %>% 
        group_by(AccYear, threatened) %>% 
        summarise(n = length(threatened))
      
      df_red_per <- df2_to_plot_red %>% reshape::cast(AccYear~threatened)
      df_red_per$`Not Threatened`[is.na(df_red_per$`Not Threatened`)] = 0
      df_red_per$Threatened[is.na(df_red_per$Threatened)] = 0
      df_red_per$total <- df_red_per$`Not Threatened` + df_red_per$Threatened
      df_red_per$pct <- df_red_per$Threatened*100/df_red_per$total
      df_red_per$AccYear = as.numeric(df_red_per$AccYear)
      
      # remove unnecessary columns
      df_red_per$`Not Threatened`<-NULL ; df_red_per$total<-NULL ; df_red_per$Threatened<-NULL
      
      # linear regression
      lm <- lm(pct~AccYear, data = df_red_per)
      summary(lm)
      lm$coefficients
      
      # save the data to replicate the plot without having to re-run the analysis
      save(df_red_per, lm, file = "Data/2F.rda")
    }
    
    # Load the data.
    load("Data/2F.rda")
    
    # Create plot
    df_red_plot = ggplot(data = df_red_per, aes(x=AccYear, y=pct)) +
      geom_point(size=2, colour = "#f46d43")+
      geom_abline(intercept = lm$coefficients[1], slope = lm$coefficients[2])+
      theme_minimal()+
      theme()+
      scale_x_continuous(n.breaks = 10)+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x = element_text(angle = -45, vjust = 0.2),
            axis.line = element_line(),
            legend.position = "bottom",
            plot.title = ggplot2::element_text(size = rel(1),
                                               face = "bold",
                                               margin = margin(0,0,5,0),
                                               hjust = 0),
            axis.title = ggplot2::element_text(size = rel(1),face = "bold"),
            axis.text = ggplot2::element_text(size = rel(0.7),face = "bold"),
            legend.title = ggplot2::element_text(size = rel(0.85),face = "bold"),
            legend.text = ggplot2::element_text(size = rel(0.85),face = "bold"),
            strip.text = ggplot2::element_text(size = rel(0.85),
                                               face = "bold",
                                               color = "white",
                                               margin = margin(5,0,5,0)),
            axis.ticks.length=unit(0.1, "cm"),
            axis.ticks.y = element_line(linewidth=0.5,color='black'),
            axis.ticks.x = element_line(linewidth=0.5,color='black')
      )+
      labs(x='Year',
           y = "% of new accessions")
    df_red_plot
    
    # save 
    ggsave(df_red_plot, filename = paste("Plots/2F.pdf"), width = 7.5, height = 4.5)
    
  }
  
}

#####################################################################
## FIGURE 3 - The quantifiable signature of effective ex-situ conservation programmes: the International Conifer Conservation Programme (ICCP) as a case study  # Can't be run for the demo as ICCP doesn't exist.
#####################################################################
if(FALSE){
  #######################
  # Figure 3A - Number of new accessions by threatened status
  #######################
  {
    # create the data
    if(run_analyses) {
      df = data[c('AccNoFull', 'AccYear','threatened', 'LC')]
      df$acc_LC = paste0(df$LC,'-',df$AccNoFull)
      df = df[,c(2,3,5)]
      df2 = dplyr::distinct(df)
      
      # summarise by counting the number of accessions per year in each category
      df2_to_plot_redO <- df2 |> 
        group_by(AccYear, threatened) |> 
        count()
      df2_to_plot_red$AccYear = as.numeric(df2_to_plot_red$AccYear)

      save(df2_to_plot_red, file = 'Data/3A.rda')  
    }
    load('Data/3A.rda')
    
    df2_plot<-ggplot(data = df2_to_plot_red, aes(x=AccYear, y=n, fill=threatened)) +
      geom_bar(stat="identity")+
      theme_minimal()+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      scale_fill_manual(values=colours[c(4,1)], name="")+
      scale_x_continuous(n.breaks = 10)+
      theme(      plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),      axis.title = ggplot2::element_text(size = rel(1), face = "bold"),      axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),      legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),      legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),      strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))    )+
      theme(      axis.ticks.length=unit(0.1, "cm"),      axis.ticks.y = element_line(size=0.5,color='black'),      axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(axis.text.x = element_text(angle = -45, vjust = 0.2)) +
      theme(legend.position = c(0.2,0.95))+
      theme(legend.title = element_blank())+
      labs(#title = "Change in number of accessions over time", 
        x='Year', y = "Number of new accessions")
    
    ggsave(plot = df2_plot, filename = 'Plots/3A.pdf', width = 7.5, height = 4.5)
  }
  #######################
  # Figure 3B - Proportion of threatened taxa in meta and ICCP
  #######################
  {
    # create the data
    if(run_analyses) {
      data_cur = data[data$ItemStatusType == 'Existing',]
      data_cur = data_cur[grep('1|2|3|4|5|6',data_cur$taxon_type),]
      data_cur = data_cur[match(unique(data_cur$best_name),data_cur$best_name), ]  
      global_threatened = sum(data_cur$threatened == 'Threatened')
      global_non_threatened = sum(data_cur$threatened == 'Not Threatened')
      
      
      
      load('Data/Enriched report for meta collection/ICCP_RBGE_enriched_report.rda')
      enriched_report = enriched_report[enriched_report$AccYear >= 1921 & enriched_report$AccYear <= 2021,]
      enriched_report = enriched_report[enriched_report$ItemStatusType == 'Existing',]
      enriched_report = enriched_report[grep('1|2|3|4|5|6',enriched_report$taxon_type),]
      # Extract threatened. 
      threat_cat = c('VU','EN','CR','EW','EX')
      threatened = rep('Not Threatened', nrow(enriched_report))
      threatened[which(enriched_report$redList_category %in% threat_cat)] = 'Threatened'
      threatened[which(enriched_report$POWO_Red_category %in% threat_cat)] = 'Threatened'
      enriched_report$threatened = threatened
      rm(threatened)
      best_name = rep(NA, nrow(enriched_report))
      best_name[!is.na(enriched_report$POWO_taxon_name)] = paste0(enriched_report$POWO_taxon_name[!is.na(enriched_report$POWO_taxon_name)])
      best_name[is.na(enriched_report$POWO_taxon_name)] = enriched_report$sanitised_taxon[is.na(enriched_report$POWO_taxon_name)]
      enriched_report$best_name = best_name
      
      enriched_report = enriched_report[match(unique(enriched_report$best_name),enriched_report$best_name), ]  
      ICCP_threatened = sum(enriched_report$threatened == 'Threatened')
      ICCP_non_threatened = sum(enriched_report$threatened == 'Not Threatened')
      
      plot_data_number = data.frame(threat = c('Threatened', 'Not Threatened'),
                                    ICCP = c(ICCP_threatened, ICCP_non_threatened), 
                                    Meta = c(global_threatened, global_non_threatened))
      
      plot_data_percent = data.frame(threat = c('Threatened', 'Not Threatened'),
                                     ICCP = c(ICCP_threatened, ICCP_non_threatened)/nrow(enriched_report), 
                                     Meta = c(global_threatened, global_non_threatened)/nrow(data_cur))
      
      save(plot_data_number, plot_data_percent, file = 'Data/3B.rda')
    }
    
    load('Data/3B.rda')
    
    dd = reshape(plot_data_percent, idvar = "threat", varying = list(2:ncol(plot_data_percent)),
                 v.names = "count", timevar = "group", times = names(plot_data_percent)[-1], direction = "long")
    dd$count = dd$count*100
    dd$threat = factor(dd$threat, levels = c("Not Threatened", "Threatened"))
    p = ggplot(dd, aes(fill=threat, y=count, x=group)) + 
      geom_bar(position="stack", stat="identity")+
      scale_fill_manual(values = c(colours[4], colours[1]))+
      guides(fill=guide_legend(title="")) +
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(linewidth=0.5,color='black'),
        axis.ticks.x = element_line(linewidth=0.5,color='black'))+
      labs(title="",
           y = paste0('% of taxa'),
           x = "") 
    
    
    ggsave(p, filename = paste('Plots/3B.pdf'), width = 10*0.75, height = 6*0.75)
    
  }
  #######################
  # Figure 3C -  Proportion of wild-origin accessions in in meta and ICCP
  #######################
  {
    # create the data
    if(run_analyses) {
      data_cur = data[data$ItemStatusType == 'Existing',]
      data_cur = data_cur[grep('1|2|3|4|5|6',data_cur$taxon_type),]
      data_cur = data_cur[match(unique(data_cur$best_name),data_cur$best_name), ]  
      global_w = sum(data_cur$ProvenanceCode == 'Wild', na.rm = T)
      global_z = sum(data_cur$ProvenanceCode == 'Wild-derived', na.rm = T)
      global_g = sum(data_cur$ProvenanceCode == 'Garden', na.rm = T)
      global_u = sum(data_cur$ProvenanceCode == 'Unknown', na.rm = T)
      
      
      load('Data/Enriched report for meta collection/ICCP_RBGE_enriched_report.rda')
      enriched_report = enriched_report[enriched_report$AccYear >= 1921 & enriched_report$AccYear <= 2021,]
      enriched_report = enriched_report[enriched_report$ItemStatusType == 'Existing',]
      enriched_report = enriched_report[grep('1|2|3|4|5|6',enriched_report$taxon_type),]
      best_name = rep(NA, nrow(enriched_report))
      best_name[!is.na(enriched_report$POWO_taxon_name)] = paste0(enriched_report$POWO_taxon_name[!is.na(enriched_report$POWO_taxon_name)])
      best_name[is.na(enriched_report$POWO_taxon_name)] = enriched_report$sanitised_taxon[is.na(enriched_report$POWO_taxon_name)]
      best_name = stringr::str_remove_all(best_name, 'xml:space="preserve">') |> stringr::str_squish()
      enriched_report$best_name = best_name
      enriched_report = enriched_report[match(unique(enriched_report$best_name),enriched_report$best_name), ]  
      ICCP_w = sum(enriched_report$ProvenanceCode == 'W')
      ICCP_z = sum(enriched_report$ProvenanceCode == 'Z')
      ICCP_g = sum(enriched_report$ProvenanceCode == 'G')
      ICCP_u = sum(enriched_report$ProvenanceCode == 'U')
      
      plot_data_number = data.frame(threat = c('Wild', 'Wild-derived', 'Garden', 'Unknown'),
                                    ICCP = c(ICCP_w, ICCP_z, ICCP_g, ICCP_u), 
                                    Meta = c(global_w, global_z, global_g, global_u))
      
      plot_data_percent = data.frame(threat =  c('Wild', 'Wild-derived', 'Garden', 'Unknown'),
                                     ICCP = c(ICCP_w, ICCP_z, ICCP_g, ICCP_u)/nrow(enriched_report), 
                                     Meta = c(global_w, global_z, global_g, global_u)/sum(plot_data_number$Meta))
      
      save(plot_data_number, plot_data_percent, file = 'Data/3C.rda')
    }
    
    load('Data/3C.rda')
    plot_data_percent_copy = plot_data_percent 
    plot_data_percent = plot_data_percent[c(1,2),]
    plot_data_percent[2,] = c('Other', sum(plot_data_percent_copy$ICCP[-1]), sum(plot_data_percent_copy$Meta[-1] ))
    plot_data_percent$ICCP = as.numeric(plot_data_percent$ICCP) ; plot_data_percent$Meta = as.numeric(plot_data_percent$Meta)                          
    dd = reshape(plot_data_percent, idvar = "threat", varying = list(2:ncol(plot_data_percent)),
                 v.names = "count", timevar = "group", times = names(plot_data_percent)[-1], direction = "long")
    dd$count = dd$count*100
    dd$threat = factor(dd$threat, levels = c("Other","Wild" ))
    p = ggplot(dd, aes(fill=threat, y=count, x=group)) + 
      geom_bar(position="stack", stat="identity")+
      scale_fill_manual(values = c(colours[4], colours[1]))+
      guides(fill=guide_legend(title="")) +
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      labs(title="",
           y = paste0('% of taxa'),
           x = "") 
    
    
    ggsave(p, filename = paste('Plots/3C.pdf'), width = 10*.75, height = 6*.75)
    
  }
  #######################
  # Figure 3D - Number of individuals per species by threatened meta and ICCP
  #######################
  {
    # create the data (Note this is done for individuals rather than accessions as the code implies)
    if(run_analyses) {
      data_cur = data[data$ItemStatusType == 'Existing',]
      data_cur = data_cur[grep('1|2|3|4|5|6',data_cur$taxon_type),]
      Accessions_per_taxa <- data_cur |>
        dplyr::group_by(.data$best_name) |>
        dplyr::summarise(Accessions = length((.data$AccNoFull)),
                         Threatened = .data$threatened[1],
                         LC = .data$LC[1]) |>
        dplyr::ungroup()
      Accessions_per_taxa$Threatened = as.character(Accessions_per_taxa$Threatened)
      Accessions_per_taxa_all = Accessions_per_taxa
      ## All bar ICCP.
      threat = table(Accessions_per_taxa$Accessions[Accessions_per_taxa$Threatened == 'Threatened'])
      threat_big_20 = sum(threat[as.numeric(names(threat))>=20])
      threat_simp = data.frame(threat[as.numeric(names(threat))<20])
      names(threat_simp) = c('Accessions', 'Taxonomic Names')
      threat_simp$Accessions = as.numeric(threat_simp$Accessions)
      threat_simp[nrow(threat_simp)+1,] = c(20, threat_big_20)
      threat_simp$density = threat_simp$`Taxonomic Names`/sum(threat_simp$`Taxonomic Names`)
      threat_cum_density = cumsum(threat)/max(cumsum(threat))
      
      non_threat = table(Accessions_per_taxa$Accessions[Accessions_per_taxa$Threatened == 'Not Threatened'])
      non_threat_big_20 = sum(non_threat[as.numeric(names(non_threat))>=20])
      non_threat_simp = data.frame(non_threat[as.numeric(names(non_threat))<20])
      names(non_threat_simp) = c('Accessions', 'Taxonomic Names')
      non_threat_simp$Accessions = as.numeric(non_threat_simp$Accessions)
      non_threat_simp[nrow(non_threat_simp)+1,] = c(20, non_threat_big_20)
      non_threat_simp$density = non_threat_simp$`Taxonomic Names`/sum(non_threat_simp$`Taxonomic Names`)
      
      non_threat_cum_density = cumsum(non_threat)/max(cumsum(non_threat))
      
      plot_data = data.frame(threat_simp,non_threat_simp )
      plot_data = plot_data[,c(1,3,6)]
      names(plot_data) = c('plot_data', 'Threatened', 'Not Threatened')
      plot_data$Threatened = plot_data$Threatened*100
      plot_data$`Not Threatened` = plot_data$`Not Threatened`*100
      plot_data_global = plot_data
      
      load('Data/Enriched report for meta collection/ICCP_RBGE_enriched_report.rda')
      enriched_report = enriched_report[enriched_report$AccYear >= 1980 & enriched_report$AccYear <= 2021,]
      enriched_report = enriched_report[enriched_report$ItemStatusType == 'Existing',]
      enriched_report = enriched_report[grep('1|2|3|4|5|6',enriched_report$taxon_type),]
      # Extract threatened. 
      threat_cat = c('VU','EN','CR','EW','EX')
      threatened = rep('Not Threatened', nrow(enriched_report))
      threatened[which(enriched_report$redList_category %in% threat_cat)] = 'Threatened'
      threatened[which(enriched_report$POWO_Red_category %in% threat_cat)] = 'Threatened'
      enriched_report$threatened = threatened
      rm(threatened)
      best_name = rep(NA, nrow(enriched_report))
      best_name[!is.na(enriched_report$POWO_taxon_name)] = paste0(enriched_report$POWO_taxon_name[!is.na(enriched_report$POWO_taxon_name)])
      best_name[is.na(enriched_report$POWO_taxon_name)] = enriched_report$sanitised_taxon[is.na(enriched_report$POWO_taxon_name)]
      best_name = stringr::str_remove_all(best_name, 'xml:space="preserve">') |> stringr::str_squish()
      enriched_report$best_name = best_name
      Accessions_per_taxa <- enriched_report |>
        dplyr::group_by(.data$best_name) |>
        dplyr::summarise(Accessions = length((.data$AccNoFull)),
                         Threatened = .data$threatened[1]) |>
        dplyr::ungroup()
      Accessions_per_taxa$Threatened = as.character(Accessions_per_taxa$Threatened)
      Accessions_per_taxa_all = Accessions_per_taxa
      ## All bar ICCP.
      threat = table(Accessions_per_taxa$Accessions[Accessions_per_taxa$Threatened == 'Threatened'])
      threat_big_20 = sum(threat[as.numeric(names(threat))>=20])
      threat_simp = data.frame(threat[as.numeric(names(threat))<20])
      names(threat_simp) = c('Accessions', 'Taxonomic Names')
      threat_simp$Accessions = as.numeric(threat_simp$Accessions)
      threat_simp[nrow(threat_simp)+1,] = c(20, threat_big_20)
      threat_simp$density = threat_simp$`Taxonomic Names`/sum(threat_simp$`Taxonomic Names`)
      missing = (1:20)[which(!(1:20) %in% threat_simp$Accessions)]
      if(length(missing)>0){
        for(i in 1:length(missing)){
          threat_simp[nrow(threat_simp)+1,] = c(missing[i], 0,0)
        }
        threat_simp = threat_simp[order(threat_simp$Accessions),]
      }
      threat_cum_density = cumsum(threat)/max(cumsum(threat))
      
      non_threat = table(Accessions_per_taxa$Accessions[Accessions_per_taxa$Threatened == 'Not Threatened'])
      non_threat_big_20 = sum(non_threat[as.numeric(names(non_threat))>=20])
      non_threat_simp = data.frame(non_threat[as.numeric(names(non_threat))<20])
      names(non_threat_simp) = c('Accessions', 'Taxonomic Names')
      non_threat_simp$Accessions = as.numeric(non_threat_simp$Accessions)
      non_threat_simp[nrow(non_threat_simp)+1,] = c(20, non_threat_big_20)
      non_threat_simp$density = non_threat_simp$`Taxonomic Names`/sum(non_threat_simp$`Taxonomic Names`)
      
      non_threat_cum_density = cumsum(non_threat)/max(cumsum(non_threat))
      
      plot_data = data.frame(threat_simp,non_threat_simp )
      plot_data = plot_data[,c(1,3,6)]
      names(plot_data) = c('plot_data', 'Threatened', 'Not Threatened')
      plot_data$Threatened = plot_data$Threatened*100
      plot_data$`Not Threatened` = plot_data$`Not Threatened`*100
      
      plot_data = data.frame(plot_data_global, ICCP_threat = plot_data$Threatened, ICP_none_threat = plot_data$`Not Threatened`)
      names(plot_data) = c("Items", "Meta-collection Threatened", "Meta-collection Not Threatened", "ICCP Threatened", "ICCP Not Threatened")
      plot_data = plot_data[,-5]
      
      save(plot_data , file = 'Data/3D.rda')
      }
    
    load('Data/3D.rda')

    
    dd = reshape(plot_data, idvar = "Items", varying = list(2:ncol(plot_data)),
                 v.names = "count", timevar = "group", times = names(plot_data)[-1], direction = "long")
    dd$group = factor(dd$group, levels = c('ICCP Threatened', 'Meta-collection Threatened', 'Meta-collection Not Threatened' ))
    p=ggplot(dd, aes(x = Items, y = count, color = group, linetype = group))+
      geom_line(aes(linetype = group), linewidth = 1.1) +
      scale_color_manual(values = c(colours[1:3]))+
      scale_linetype_manual(values = c(1,1,1,1)) +
      labs(x = 'Number of individuals per species', y = '% of species', caption = '') +
      labs(color  = "", linetype = "") +
      scale_y_continuous(label = scales::label_number(suffix = ""))+
      scale_x_continuous(
        breaks = c(1,5,10,15,20),
        labels = ~ ifelse(.x == 20, paste0(.x, '+'), .x)) + 
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.85, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      )
    
    ggsave(p, filename = paste('Plots/3D.pdf'), width = 10*.75, height = 6*.75)
    
  }
  #######################
  # Figure 3E - Number of individuals per taxon by provenance
  #######################
  {
    # create the data (Note this is done for individuals rather than accessions as the code implies)
    if(run_analyses){
      df = data
      data_cur = df[df$ItemStatusType == 'Existing',]
      data_cur = data_cur[grep('1|2|3|4|5|6',data_cur$taxon_type),]
      data_cur = data_cur[data_cur$ProvenanceCode %in% c("Unknown", "Garden", "Wild","Wild-derived"),]
      best_name = rep(NA, nrow(data_cur))
      best_name[!is.na(data_cur$POWO_taxon_name)] = paste0(data_cur$POWO_taxon_name[!is.na(data_cur$POWO_taxon_name)])
      best_name[is.na(data_cur$POWO_taxon_name)] = data_cur$sanitised_taxon[is.na(data_cur$POWO_taxon_name)]
      best_name = stringr::str_remove_all(best_name, 'xml:space="preserve">') |> stringr::str_squish()
      data_cur$best_name = best_name
      data_cur$LC_and_name = paste0(data_cur$LC, '-', data_cur$best_name)
      AA = data_cur[c("LC_and_name", 'ProvenanceCode')]
      meta <- AA %>% group_by(across(everything())) %>% count(total=n())
      meta$type = 'Meta'
      
      
      load('/Users/jakepowell/Cambridge/Enriched_reports_Jake/ICCP_RBGE_enriched_report.rda')
      enriched_report = enriched_report[enriched_report$AccYear >= 1921 & enriched_report$AccYear <= 2021,]
      enriched_report = enriched_report[enriched_report$ItemStatusType == 'Existing',]
      enriched_report = enriched_report[grep('1|2|3|4|5|6',enriched_report$taxon_type),]
      enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'G'] = 'Garden'
      enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'U'] = 'Unknown'
      enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'W'] = 'Wild'
      enriched_report$ProvenanceCode[enriched_report$ProvenanceCode == 'Z'] = 'Wild-derived'
      
      # threat_cat = c('VU','EN','CR','EW','EX')
      # threatened = rep('Not Threatened', nrow(enriched_report))
      # threatened[which(enriched_report$redList_category %in% threat_cat)] = 'Threatened'
      # threatened[which(enriched_report$POWO_Red_category %in% threat_cat)] = 'Threatened'
      # enriched_report$threatened = threatened
      # enriched_report = enriched_report[enriched_report$threatened == 'Threatened',]
      
      best_name = rep(NA, nrow(enriched_report))
      best_name[!is.na(enriched_report$POWO_taxon_name)] = paste0(enriched_report$POWO_taxon_name[!is.na(enriched_report$POWO_taxon_name)])
      best_name[is.na(enriched_report$POWO_taxon_name)] = enriched_report$sanitised_taxon[is.na(enriched_report$POWO_taxon_name)]
      best_name = stringr::str_remove_all(best_name, 'xml:space="preserve">') |> stringr::str_squish()
      enriched_report$best_name = best_name
      data_cur$LC_and_name = paste0('ICCP', '-', data_cur$best_name)
      
      AA = enriched_report[c("best_name", 'ProvenanceCode')]
      ICCP <- AA %>% group_by(across(everything())) %>% count(total=n())
      ICCP$type = 'ICCP'
      
      df = rbind(meta,ICCP)
      
      df_box<-df
      df_box$col<-ifelse(df_box$type=="ICCP", "ICCP", df_box$type)
      df_box$col<-ifelse(df_box$type=="Meta", "Meta", df_box$col)
      df_box = df_box[order(df_box$ProvenanceCode),]
      df_box$color = df_box$col
      
      df_box_w = df_box[df_box$ProvenanceCode == 'Wild',]
      df_box_z = df_box[df_box$ProvenanceCode == 'Wild-derived',]
      df_box_g = df_box[df_box$ProvenanceCode == 'Garden',]
      df_box_u = df_box[df_box$ProvenanceCode == 'Unknown',]
      
      df_box = rbind(df_box_w, df_box_z, df_box_g, df_box_u)
      # df_box$color[df_box$col == 'ICCP'] = add_alpha("#E7B800",0.1)
      # df_box$color[df_box$col == 'Meta'] = add_alpha("#00AFBB",0.1)
      
      s_df_box <- df_box %>% group_by(ProvenanceCode, col) %>%
        summarise(mean=round(mean(n), digits = 1))
      s_df_box = s_df_box[c(5,6,7,8,1,2,3,4),]
      labels <- s_df_box$mean
      
      labels_order <- s_df_box$col
      save(df_box, labels, file = 'Data/3E.rda')
    }
    
    load("Data/3E.rda")
    
   AA =  df_box |> dplyr::group_by(ProvenanceCode, col) |> dplyr::count()
    bxp<-ggpubr::ggboxplot(df_box, x = "ProvenanceCode", y = "n", 
                           add = "jitter",
                           color ="col")+
      annotate("text", x=0.8, y=0, label=labels[1], fontface = "bold")+
      annotate("text", x=1.2, y=0, label=labels[2], fontface = "bold")+
      annotate("text", x=1.8, y=0, label=labels[3], fontface = "bold")+
      annotate("text", x=2.2, y=0, label=labels[4], fontface = "bold")+
      annotate("text", x=2.8, y=0, label=labels[5], fontface = "bold")+
      annotate("text", x=3.2, y=0, label=labels[6], fontface = "bold")+
      annotate("text", x=3.8, y=0, label=labels[7], fontface = "bold")+
      annotate("text", x=4.2, y=0, label=labels[8], fontface = "bold")+
      scale_color_manual(values=c("Meta"='#50c1dc', "ICCP"="#f46d43"), name="")+
      theme_minimal()+
      theme(panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.line = element_line(),
            legend.title = element_blank(),
            plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
            axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
            axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
            # legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
            legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
            strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0)),
            axis.ticks.length=unit(0.1, "cm"), 
            axis.ticks.y = element_line(linewidth=0.5,color='black'),
            axis.ticks.x = element_line(linewidth=0.5,color='black'),
            legend.position = c(0.05, .95),
            legend.justification = c("left", "top"),
            legend.box.just = "left",
            legend.margin = margin(3, 3, 3, 3)
      )+
      labs(title="",
           x = NULL, y = "Number of individuals per taxon", caption = "")
    
    bxp
    ggsave(plot = bxp, filename = 'Plots/3E.pdf',width = 10*.75, height = 6*.75)
    ggsave(plot = bxp, filename = 'Plots/3E.png',width = 10*.75, height = 6*.75)
    
    
    
  }
  #######################
  # Figure 3F - Number of collections worldwide holding species in meta and ICCP.
  #######################
  {
    # create the data
    if(run_analyses) {
    data_cur = data[data$ItemStatusType == 'Existing',]
    data_cur = data_cur[grep('1|2|3|4|5|6',data_cur$taxon_type),]
    data_cur = data_cur[match(unique(data_cur$best_name),data_cur$best_name), ]  
    data_cur = data_cur[data_cur$threatened =='Threatened',]
    
    load('Data/Enriched report for meta collection/ICCP_RBGE_enriched_report.rda')
    enriched_report = enriched_report[enriched_report$AccYear >= 1921 & enriched_report$AccYear <= 2021,]
    enriched_report = enriched_report[enriched_report$ItemStatusType == 'Existing',]
    enriched_report = enriched_report[grep('1|2|3|4|5|6',enriched_report$taxon_type),]
    
    threat_cat = c('VU','EN','CR','EW','EX')
    threatened = rep('Not Threatened', nrow(enriched_report))
    threatened[which(enriched_report$redList_category %in% threat_cat)] = 'Threatened'
    threatened[which(enriched_report$POWO_Red_category %in% threat_cat)] = 'Threatened'
    enriched_report$threatened = threatened
    enriched_report = enriched_report[enriched_report$threatened == 'Threatened',]
    
    best_name = rep(NA, nrow(enriched_report))
    best_name[!is.na(enriched_report$POWO_taxon_name)] = paste0(enriched_report$POWO_taxon_name[!is.na(enriched_report$POWO_taxon_name)])
    best_name[is.na(enriched_report$POWO_taxon_name)] = enriched_report$sanitised_taxon[is.na(enriched_report$POWO_taxon_name)]
    best_name = stringr::str_remove_all(best_name, 'xml:space="preserve">') |> stringr::str_squish()
    enriched_report$best_name = best_name
    
    enriched_report = enriched_report[match(unique(enriched_report$best_name),enriched_report$best_name), ] 
    
    type = 1
    
    meta_no_gardens = data_cur$no_gardens
    meta_quant_rm_0 = quantile(meta_no_gardens, probs = seq(0,1,0.001), na.rm = T, type = type)
    meta_no_gardens[is.na(meta_no_gardens)] = 0
    meta_quant = quantile(meta_no_gardens, probs = seq(0,1,0.001), type = type)
    
    ICCP_no_gardens = enriched_report$no_gardens
    ICCP_quant_rm_0 = quantile(ICCP_no_gardens, probs = seq(0,1,0.001), na.rm = T, type = type)
    ICCP_no_gardens[is.na(ICCP_no_gardens)] = 0
    ICCP_quant = quantile(ICCP_no_gardens, probs = seq(0,1,0.001), type = type)
    
    
    with_rm_0 = data.frame(quantile = seq(0,1,0.001),  Meta = meta_quant_rm_0, ICCP = ICCP_quant_rm_0)
    with_0 = data.frame(quantile = seq(0,1,0.001), Meta = meta_quant, ICCP =  ICCP_quant)
    
    ThreeF_rm_0 = reshape(with_rm_0, idvar = "quantile", varying = list(2:ncol(with_rm_0)),
                 v.names = "count", timevar = "group", times = names(with_rm_0)[-1], direction = "long")
    
    ThreeF_with_0 = reshape(with_0, idvar = "quantile", varying = list(2:ncol(with_0)),
                 v.names = "count", timevar = "group", times = names(with_0)[-1], direction = "long")
    
    save(ThreeF_rm_0, ThreeF_with_0, file = 'Data/3F.rda')
    }
    load('Data/3F.rda')
    ThreeF_with_0$group[ThreeF_with_0$group == 'Meta'] = 'Meta-collection'
    p = ggplot(ThreeF_with_0, aes(y = quantile, x = count, group = group, color = group)) +
      geom_line(linewidth = 1.1) +
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      scale_x_continuous(n.breaks = 10)+
      scale_y_continuous(n.breaks = 10)+
      theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(.9, .2),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      )+
      scale_color_manual(values = c(colours[1:2])) +
      guides(color=guide_legend(title='')) + 
      labs(title="",
           x ="Number of collections holding a species worldwide",
           y = "Quantile")
    
  
    
    ggsave(p, filename = paste('Plots/3F.pdf'), width = 10*.75, height = 6*.75)
    
  }
}

#####################################################################
## FIGURE 4 - Kaplan-Meier survival curves (solid lines) with 95% confidence interval (shaded regions). 
#####################################################################
{
  #######################
  # Get survival information prior to figures.
  #######################
  {
    if(run_analyses){
      # Create a data-frame of the information we want.
      wanted_data = data.frame(data$AccYear, data$ItemStatusDate, data$ItemStatusType)
      names(wanted_data) = c('AccYear', 'ItemStatusDate', 'ItemStatusType')
      
      
      # Set accession and last item status dates for calculating survival time
      pre_date = rep(NA, length(wanted_data$AccYear))
      pre_date = paste(wanted_data$AccYear, "01", "01", sep = "-")
      post_date = data$ItemStatusDate
      post_date[which(stringr::str_length(post_date) == 4)] = paste(post_date[which(stringr::str_length(post_date) == 
                                                                                      4)], "12", "28", sep = "-")
      post_date[which(stringr::str_length(post_date) == 7)] = paste(post_date[which(stringr::str_length(post_date) == 
                                                                                      7)], "28", sep = "-")
      
      # Create the time difference between accession and ItemStatusDate.
      time = as.numeric(as.Date(post_date) - as.Date(pre_date)) /365.25
      
      # Create status to tell whether the last item status date was item death or just a check.
      status = rep(0,length(wanted_data$AccYear))
      status[which(wanted_data$ItemStatusType %in% c('NotExisting', 'Not Existing'))] = 1
      
      wanted_data = data.frame(AccessionDate = pre_date,
                               StatusDate = post_date,
                               time = time,
                               Alive_dead = status)
      
      survival_data = cbind(wanted_data, data)
      survival_data = survival_data[which(survival_data$time > 0 & survival_data$time < 101),]
      survival_data = survival_data[which(as.Date(survival_data$StatusDate) < as.Date('2024-01-01')),]
      
      save(survival_data, file = 'Data/survival_data.rda')
      
      # Get survival for accessions only
      survival_data$AccNoFull = paste0(survival_data$AccNoFull, '-', survival_data$LC)
      unique_accessions = unique(survival_data$AccNoFull)
      year = as.numeric(stringr::str_extract(survival_data$StatusDate, '^[0-9]{4}'))
      month = as.numeric(stringr::str_extract(survival_data$StatusDate, '-[0-9]{2}') |> stringr::str_remove('-'))
      day = as.numeric(stringr::str_extract(survival_data$StatusDate, '[0-9]{2}$'))
      year[survival_data$ItemStatusType %in% c('NotExisting', 'Not Existing')] = year[survival_data$ItemStatusType %in% c('NotExisting', 'Not Existing')] - 10000
      
      wanted_order = order(year, month, day, decreasing = T)
      survival_data = survival_data[wanted_order,]
      
      #Match to the ordered accessions.
      match_to_best = match(unique_accessions,survival_data$AccNoFull)
      
      survival_accessions = survival_data[match_to_best,]
      save(survival_accessions, file = 'Data/survival_accessions.rda')
      
      # Get survival for species only.
      survival_accessions$LC_taxon = paste0(survival_accessions$LC, '-', survival_accessions$best_name)
      survival_accessions = survival_accessions[order(survival_accessions$LC_taxon),]
      data_names = survival_accessions$LC_taxon
      out = data.frame(matrix(nrow = 0, ncol = 4))
      names(out) = c("LC-TaxonName", "Dates in LC", "Alive_Dead", "Exist time (years)") 
      pb = utils::txtProgressBar(min = 0, max = length(data_names), initial = 0, style = 3) 
      tic = Sys.time()
      for(i in 1:length(data_names)){
        if(i%%100 == 0){setTxtProgressBar(pb, i, title = NULL, label = NULL)}
        if(i == 1){name_pre = data_names[i] ; counters = NULL}
        name_cur = data_names[i]
        
        # If we do not have a change in the LC_taxon
        if(name_cur == name_pre){
          counters = c(counters, i)
          next
        }else{
          # Get the extract from data.
          wanted = survival_accessions[counters,]
          
          #If we only have a single record just return the name, dates and time.
          if(length(counters) == 1){
            out[nrow(out)+1,] = c(name_pre, paste0('(',wanted$AccessionDate,', ',wanted$StatusDate,')'), wanted$Alive_dead, wanted$time)
            name_pre = name_cur ; counters = i
            next
          }
          
          # Update existing to the most recent status date amongst all records.
          most_recent_status_date = sort(wanted$StatusDate, decreasing = T)[1]
          wanted$StatusDate[wanted$Alive_dead == 0] = most_recent_status_date
          
          # If the oldest record (first row) is existing just return it's values.
          if(wanted$Alive_dead[1] == 0){
            out[nrow(out)+1,] = c(name_pre, paste0('(',wanted$AccessionDate[1],', ',wanted$StatusDate[1],')'), wanted$Alive_dead[1], wanted$time[1])
            name_pre = name_cur ; counters = i
            next
          }
          
          # Now use general method to calculate time intervals.
          existing_intervals = ivs::iv_groups( ivs::iv(wanted$AccessionDate,  wanted$StatusDate))
          start = lubridate::ymd(ivs::iv_start(existing_intervals))
          end = lubridate::ymd(ivs::iv_end(existing_intervals))
          exist_times = round(as.numeric(difftime(as.Date(end), as.Date(start), unit="days"))/365.25,digits = 3)
          exist_times = paste(exist_times, collapse = ', ')
          
          existing_interval = paste('(', ivs::iv_start(existing_intervals), ', ',  ivs::iv_end(existing_intervals), ')', collapse = ':')
          
          # Work out if the end interval date is alive or dead. 
          if(any(wanted$Alive_dead == 0)){
            Alive_dead = 0
          }else{
            Alive_dead = 1
          }
          out[nrow(out)+1,] = c(name_pre, existing_interval, Alive_dead, exist_times)
          name_pre = name_cur ; counters = i
        }
      }
      toc = Sys.time()
      # Takes ~ 12 hours to run
      toc-tic
      
      no_intervals = stringr::str_count(out$`Dates in LC`,pattern = ':') +1
      out$no_intervals = no_intervals
      out$threatened = data$threatened[match(out$`LC-TaxonName`, data$LC_taxon)]
      out$prov = data$ProvenanceCode[match(out$`LC-TaxonName`, data$LC_taxon)]
      out$native = data$native[match(out$`LC-TaxonName`, data$LC_taxon)]
      out$tree = data$tree[match(out$`LC-TaxonName`, data$LC_taxon)]
      out$endemic = data$endemic[match(out$`LC-TaxonName`, data$LC_taxon)]
      out$taxon_type = data$taxon_type[match(out$`LC-TaxonName`, data$LC_taxon)]
      
      # Use all survival times.
      all_survival = out[out$no_intervals==1,]
      all_survival$`Exist time (years)` = as.numeric(all_survival$`Exist time (years)`)
      mult_intervals = out[out$no_intervals>1,]
      to_append = pbapply::pblapply(1:nrow(mult_intervals),function(x){
        cur = mult_intervals[x,]
        times = as.numeric(unlist(stringr::str_split(cur$`Exist time (years)`, ', ')))
        Alive_dead = c(rep(1,length(times)-1),cur$Alive_Dead)
        
        to_append = mult_intervals[rep(x, times = length(times)), ]
        to_append$Alive_Dead = Alive_dead
        to_append$`Exist time (years)` = times
        to_append
      })
      to_append_j = dplyr::bind_rows(to_append)
      all_survival = dplyr::bind_rows(list(all_survival, to_append_j))
      all_survival$Alive_Dead = as.numeric(all_survival$Alive_Dead)
      all_survival$AccYear = as.numeric(stringr::str_extract(all_survival$`Dates in LC`,'[0-9]{4}'))
      survival_species = all_survival
      save(survival_species, file = 'Data/survival_species.rda')
    }
    
    load('Data/survival_accessions.rda')
    load('Data/survival_species.rda')
  }
  #######################
  # Figure 4A - overall accessions
  #######################
  {
    fits_global = ggsurvfit::survfit2(survival::Surv(time, Alive_dead) ~ 1, data = survival_accessions)
    fits_global
    fits_global = fits_global |>
      ggsurvfit::ggsurvfit(linewidth = 1.1)
   
   
    
    surv_data = data.frame(time = fits_global$data$time,
                      estimate = fits_global$data$estimate,
                      high = fits_global$data$conf.high,
                      low = fits_global$data$conf.low
    )
    
    p = ggplot(surv_data, aes(x= time, y = estimate)) +
      geom_line(linewidth = 1.1, colour = '#f46d43') +
      geom_ribbon(aes(ymin=low, ymax=high), linetype=1, alpha=0.4, colour = NA, fill = '#f46d43') +
      scale_y_continuous(labels = function(x) paste0(x*100, ""))+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.85, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      ) +
      scale_x_continuous(n.breaks = 10) +
      ggplot2::labs(
        x = "Age (Years)",
        y = "Survival probability (%)"
      )
    ggsave(p, filename = paste0('Plots/4A.pdf'), width = 10*0.75, height = 6*0.75)
    
    
  }
  #######################
  # Figure 4B - Tree against non-tree accessions
  #######################
  {
    survival_accessions$tree = factor(survival_accessions$tree, levels = c('Tree', 'Not Tree'))
    fit = ggsurvfit::survfit2(survival::Surv(time, Alive_dead) ~ tree, data = survival_accessions) |>
      ggsurvfit::ggsurvfit(linewidth = 1.1)
    
    surv_data = data.frame(time = fit$data$time,
                      estimate = fit$data$estimate,
                      high = fit$data$conf.high,
                      low = fit$data$conf.low,
                      group = fit$data$strata
    )
    if(grepl('=',surv_data$group[1])){
      group_new = as.character(surv_data$group) |> stringr::str_extract(pattern = '=[A-Za-z\\- ]*') |> stringr::str_remove(pattern = '=')
      surv_data$group = factor(group_new, levels = unique(group_new))
    }
    
    p = ggplot(surv_data, aes(x= time, y = estimate, colour = group)) +
      geom_line(linewidth = 1.1) +
      geom_ribbon(aes(ymin=low, ymax=high, fill = group), linetype=1, alpha=0.4, colour = NA) +
      scale_color_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      scale_fill_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      # ggsurvfit::add_quantile(y_value = 0.5, linetype = "dotted", color = "grey30", linewidth = 0.8)+
      scale_y_continuous(labels = function(x) paste0(x*100, ""))+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.85, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      ) +
      scale_x_continuous(n.breaks = 10) +
      ggplot2::labs(
        x = "Age (Years)",
        y = "Survival probability (%)"
      ) +
      theme(legend.title=element_blank())
    
    ggsave(p, filename = paste0('Plots/4B.pdf'), width = 10*0.75, height = 6*0.75)
    
  }
  #######################
  # Figure 4C - Endemic against widespread accessions
  #######################
  {
    survival_accessions$endemic = factor(survival_accessions$endemic, levels = c("Endemic", "Widespread"))
    fit = ggsurvfit::survfit2(survival::Surv(time, Alive_dead) ~ endemic, data = survival_accessions) |>
      ggsurvfit::ggsurvfit(linewidth = 1.1)
    
    surv_data = data.frame(time = fit$data$time,
                      estimate = fit$data$estimate,
                      high = fit$data$conf.high,
                      low = fit$data$conf.low,
                      group = fit$data$strata
    )
    if(grepl('=',surv_data$group[1])){
      group_new = as.character(surv_data$group) |> stringr::str_extract(pattern = '=[A-Za-z\\- ]*') |> stringr::str_remove(pattern = '=')
      surv_data$group = factor(group_new, levels = unique(group_new))
    }
    
    p = ggplot(surv_data, aes(x= time, y = estimate, colour = group)) +
      geom_line(linewidth = 1.1) +
      geom_ribbon(aes(ymin=low, ymax=high, fill = group), linetype=1, alpha=0.4, colour = NA) +
      scale_color_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      scale_fill_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      # ggsurvfit::add_quantile(y_value = 0.5, linetype = "dotted", color = "grey30", linewidth = 0.8)+
      scale_y_continuous(labels = function(x) paste0(x*100, ""))+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.85, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      ) +
      scale_x_continuous(n.breaks = 10) +
      ggplot2::labs(
        x = "Age (Years)",
        y = "Survival probability (%)"
      ) +
      theme(legend.title=element_blank())
    
    ggsave(p, filename = paste0('Plots/4C.pdf'), width = 10*0.75, height = 6*0.75)
    
  }
  #######################
  # Figure 4D - Threatened/non-threatened accessions + species
  #######################
  {
    dd = survival_accessions[,c(3,4,6,21)]
    dd$threatened = paste0(dd$threatened, ' Accessions') 
    
    dd_species_threat = survival_species[,c(4,3,11,6)]
    dd_species_threat$threatened = paste0(dd_species_threat$threatened, ' Species') 
    # dd_species_threat$threatened = 'Threatened Species'
    names(dd_species_threat) = names(dd)
    
    dd_use = rbind(dd, dd_species_threat)
    dd_use$time = as.numeric(dd_use$time)
    dd_use$AccYear = as.numeric(dd_use$AccYear)
    dd_use$Alive_dead = as.numeric(dd_use$Alive_dead)
    dd_use$threatened = factor(dd_use$threatened, levels = c('Threatened Accessions', 'Not Threatened Accessions', 'Threatened Species', 'Not Threatened Species'))
    dd_use = dd_use[dd_use$AccYear >= 1980 & dd_use$AccYear <= 2021,]
    p = ggsurvfit::survfit2(survival::Surv(time, Alive_dead) ~ threatened, data = dd_use) |>
      ggsurvfit::ggsurvfit(linewidth = 1.1)
    
    surv_data = data.frame(time = p$data$time,
                      estimate = p$data$estimate,
                      high = p$data$conf.high,
                      low = p$data$conf.low,
                      group = p$data$strata
    )
    p = ggplot(surv_data, aes(x= time, y = estimate, colour = group)) +
      geom_line(linewidth = 1.1) + #aes(linetype = group)
      # scale_linetype_manual(values = c(1,2,1,2))+
      geom_ribbon(aes(ymin=low, ymax=high, fill = group), linetype=1, alpha=0.4, colour = NA) +
      scale_color_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      scale_fill_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      # ggsurvfit::add_quantile(y_value = 0.5, linetype = "dotted", color = "grey30", linewidth = 0.8)+
      scale_y_continuous(labels = function(x) paste0(x*100, ""))+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.85, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      ) +
      scale_x_continuous(n.breaks = 10) +
      ggplot2::labs(
        x = "Age (Years)",
        y = "Survival probability (%)"
      )+
      theme(legend.title=element_blank())
    
    
    ggsave(p, filename = paste0('Plots/4D.pdf'), width = 10*0.75, height = 6*0.75)
  }
  #######################
  # Figure 4E - Native against non-native accessions
  #######################
  {
    survival_accessions$native = factor(survival_accessions$native, levels = c('Native', 'Not Native'))
    fit = ggsurvfit::survfit2(survival::Surv(time, Alive_dead) ~ native, data = survival_accessions) |>
    ggsurvfit::ggsurvfit(linewidth = 1.1)
    
    surv_data = data.frame(time = fit$data$time,
                      estimate = fit$data$estimate,
                      high = fit$data$conf.high,
                      low = fit$data$conf.low,
                      group = fit$data$strata
    )
    if(grepl('=',surv_data$group[1])){
      group_new = as.character(surv_data$group) |> stringr::str_extract(pattern = '=[A-Za-z\\- ]*') |> stringr::str_remove(pattern = '=')
      surv_data$group = factor(group_new, levels = unique(group_new))
    }
    
    p = ggplot(surv_data, aes(x= time, y = estimate, colour = group)) +
      geom_line(linewidth = 1.1) +
      geom_ribbon(aes(ymin=low, ymax=high, fill = group), linetype=1, alpha=0.4, colour = NA) +
      scale_color_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      scale_fill_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      # ggsurvfit::add_quantile(y_value = 0.5, linetype = "dotted", color = "grey30", linewidth = 0.8)+
      scale_y_continuous(labels = function(x) paste0(x*100, ""))+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.85, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      ) +
      scale_x_continuous(n.breaks = 10) +
      ggplot2::labs(
        x = "Age (Years)",
        y = "Survival probability (%)"
      ) +
      theme(legend.title=element_blank())
    
    ggsave(p, filename = paste0('Plots/4E.pdf'), width = 10*0.75, height = 6*0.75)
    
  }
  #######################
  # Figure 4F - Biological against horticultral accessions
  #######################
  {
    tag = rep(NA, nrow(survival_accessions))
    tag[grep('5|6',survival_accessions$taxon_type)] = 'Horticultual'
    tag[!grepl('5|6',survival_accessions$taxon_type)] = 'Biological'
    survival_accessions$tag = tag
    data_cur = survival_accessions
    data_cur = data_cur[!grepl('0',data_cur$taxon_type),]
    fit = ggsurvfit::survfit2(survival::Surv(time, Alive_dead) ~ tag, data = data_cur) |>
      ggsurvfit::ggsurvfit(linewidth = 1.1)
    
    surv_data = data.frame(time = fit$data$time,
                      estimate = fit$data$estimate,
                      high = fit$data$conf.high,
                      low = fit$data$conf.low,
                      group = fit$data$strata
    )
    if(grepl('=',surv_data$group[1])){
      group_new = as.character(surv_data$group) |> stringr::str_extract(pattern = '=[A-Za-z\\- ]*') |> stringr::str_remove(pattern = '=')
      surv_data$group = factor(group_new, levels = unique(group_new))
    }
    
    p = ggplot(surv_data, aes(x= time, y = estimate, colour = group)) +
      geom_line(linewidth = 1.1) +
      geom_ribbon(aes(ymin=low, ymax=high, fill = group), linetype=1, alpha=0.4, colour = NA) +
      scale_color_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      scale_fill_manual(values = c('#f46d43', '#50c1dc', '#848484', '#e6e6e6')) +
      # ggsurvfit::add_quantile(y_value = 0.5, linetype = "dotted", color = "grey30", linewidth = 0.8)+
      scale_y_continuous(labels = function(x) paste0(x*100, ""))+
      theme_minimal()+
      theme(
        plot.title = ggplot2::element_text(size = rel(1), face = "bold", margin = margin(0,0,5,0), hjust = 0),
        axis.title = ggplot2::element_text(size = rel(1), face = "bold"),
        axis.text = ggplot2::element_text(size = rel(0.7), face = "bold"),
        legend.title = ggplot2::element_text(size = rel(0.85), face = "bold"),
        legend.text = ggplot2::element_text(size = rel(0.85), face = "bold"),
        strip.text = ggplot2::element_text(size = rel(0.85), face = "bold", color = "white", margin = margin(5,0,5,0))
      )+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(axis.line = element_line())+
      # theme(legend.title = element_blank())+
      theme(
        axis.ticks.length=unit(0.1, "cm"), 
        axis.ticks.y = element_line(size=0.5,color='black'),
        axis.ticks.x = element_line(size=0.5,color='black'))+
      theme(
        legend.position = c(0.85, .95),
        legend.justification = c("right", "top"),
        legend.box.just = "right",
        legend.margin = margin(6, 6, 6, 6)
      ) +
      scale_x_continuous(n.breaks = 10) +
      ggplot2::labs(
        x = "Age (Years)",
        y = "Survival probability (%)"
      ) +
      theme(legend.title=element_blank())
    
    ggsave(p, filename = paste0('Plots/4F.pdf'), width = 10*0.75, height = 6*0.75)
    
  }
  

 
}
toc = Sys.time()
toc-tic