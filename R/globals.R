utils::globalVariables(c('datetime', 'year', 'month', 'model', 'ghi', 'dni', 'dhi', 'ws', 'temp', #Megaframe
                         'observationtime(lst)', 'date time', # CPR
                         'ghi1', 'ghi2', 'timestamp', 'timestamp_1', #SMS
                         'ghi_ave', #sms_flagged
                         'hour', 'ghi1_reconst', 'ghi2_reconst', # Average SMS data to hourly
                         'timestep', 'sensor', 'value', '<NA>', 'metric', 'metric_value',
                         'month_hours', '.', #sum_monthly_means
                         'sms', # prepare_models_as_vars() for run_mcp()
                         'longterm_resource', 'longterm_adjustment', # MCP Results
                         'r.squared', 'iav', 'por', 'sms_por', 'sd', 'ave', 'ltra', 'rom', # Model/Longterm Metrics
                         'flag_type', 'n', 'perc', 'sms_months' #plot_functions()
                         ))
