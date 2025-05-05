





#passage history parsing

mutate(passage_type = case_when(grepl("original|or|clinical", tolower(Passage_History)) ~ "Original", 
                                grepl("/|\\+|[a-z]{1}[1-9]{1}.?[a-z]{1}[1-9]{1}", tolower(Passage_History)) ~ "Multiple",
                                
                                !grepl(";", tolower(Passage_History)) & grepl(",", tolower(Passage_History)) ~ "Multiple",
                                
                                grepl("e[1-9]{1}|egg|embryonated", tolower(Passage_History)) ~ "EGG", 
                                
                                
                                grepl("siat|s[0-9]{1}", tolower(Passage_History)) ~ "MDCK-SIAT", 
                                
                                grepl("mdck|mdck cells|m[1-9]{1}|mx", tolower(Passage_History)) ~ "MDCK", 
                                
                                
                                
                                grepl("rmk|rhmk|rii|pmk|prhmk|r[1-9]{1}", tolower(Passage_History)) ~ "RhMK", 
                                
                                
                                
                                grepl("c[:blank:]|c[1-9]{1}|cx|p[1-9]{1}|x[1-9]{1}", tolower(Passage_History)) ~ "Unknown Cell", 
                                
                                TRUE ~ tolower(Passage_History)))






