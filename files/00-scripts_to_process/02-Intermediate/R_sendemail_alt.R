library(mailR), send.mail(from = sender,
                          to = recipients,
                          subject = Test on R,
                          body = I use R send this email to you.,
                          smtp = list(host.name = smtp.gmail.com, port = 465, 
                                      user.name = chn@gmail.com,            
                                      passwd = xxx, ssl = TRUE),
                          authenticate = TRUE,
                          send = TRUE)
