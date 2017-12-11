research = function(k, sp, n)
{
  # Creazione nome file di log
  logfilename = format(Sys.time(), "%Y%m%d%H%M%S")
  logfilename = paste(logfilename,  ".txt", sep = "")

    # Lettura file parole chiave
  if(is.na(k) || !file.exists(k))
  {
    
    write.table(ls(), file = logfilename, append = FALSE, quote = FALSE, sep = ",", na = "", row.names = FALSE,
                col.names = FALSE) 
    print("File parole chiave non trovato.")
    return(1)
  }
  keynames = read.table(k, header = FALSE)
  keynames = as.matrix(keynames)
  
  # Scelta db in base all'iniziale della specie
  if(sp == "t" || sp=="T" || sp == "m" || sp == "M")
  {
    db_a = "mm10"
    db_b = "mm9"
  }else if(sp == "r" || sp == "R")
  {
    db_a = "rn6"
    db_b = "rn5"     
  }else
  {
    db_a ="hg38"
    db_b ="hg19"    
  }
  
  # Intestazioni e nome file di uscita
  outheaders = c("key", "name","name2")  
  if(is.na(n))
  {
    n = "outfile.csv"
  }
  
  # Caricamento librerie
  library(RMySQL)
  library(tibble)
  
  # Query per ogni entry del file parole chiave
    mydb = dbConnect(drv = MySQL(),user ='genome',host = 'genome-mysql.soe.ucsc.edu')
  for(i in 1:nrow(keynames))
  {
    s = keynames[i, 1]
    
    querystring = paste("use ", db_a)
    res = dbSendQuery(mydb, querystring)
    dbClearResult(res)
    
    querystring = paste("select name, name2 from refGene where name = '", s, "' or name2 = '", s,"'",
                        "group by name",
                        sep = "")
    res = dbSendQuery(mydb, querystring)
    altnames_a = dbFetch(res)
    dbClearResult(res)
    
    outkey = paste("key: ", s, " - db: ", db_a, sep = "")
    out_a = add_column(.data = altnames_a, key = outkey, .before = 1)
        
    # Scrittura risultati da db più recente
    if( i == 1 )
    {
      app = FALSE
      clnames = outheaders
    }else
    {
      app = TRUE
      clnames = FALSE
    }
    write.table(out_a, file = n, append = app, quote = FALSE, sep = ",", na = "", row.names = FALSE,
                col.names = clnames)   
        
    querystring = paste("use ", db_b)
    res = dbSendQuery(mydb, querystring)
    dbClearResult(res)
    
    querystring = paste("select name, name2 from refGene where name = '", s, "' or name2 = '", s,"'",
                        "group by name",
                        sep = "")
    res = dbSendQuery(mydb, querystring)
    altnames_b = dbFetch(res)
    dbClearResult(res)
 
    
    outkey = paste("key: ", s, " - db: ", db_b, sep = "")
    out_b = add_column(.data = altnames_b, key = outkey, .before = 1)
    
    # Scrittura risultati penultimo db più recente
    write.table(out_b, file = n, append = TRUE, quote = FALSE, sep = ",", na = "", row.names = FALSE,
                col.names = FALSE)     
     
    
  }
    dbDisconnect(mydb)

    
  # Scrittura file di log
  write.table(ls(), file = logfilename, append = FALSE, quote = FALSE, sep = ",", na = "", row.names = FALSE,
              col.names = FALSE)  
  return(0)

}

readparam = function()
{
  # Input a riga di comando parametri funzione di ricerca
  keyfile = readline(prompt = "Scrivere nome e percorso del file con le parole chiave: ")
  specie = readline(prompt = "Scrivere l'iniziale della specie da studiare: ")
  writefile = readline(prompt = "Scrivere il nome e percorso del file di output: ")
  if(is.character(keyfile) && is.character(specie) && is.character(writefile))
  {
    research(keyfile, specie, writefile)
    return(0)
  }else
  {
    print("Tipi di dato non corretti.")
    return(1)
  }  
}

readparam()
