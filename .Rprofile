options(
  # Using Posit Public Package Manager for installing packages faster using pak
  repos = c(CRAN = "https://p3m.dev/cran/__linux__/bookworm/latest"),
  tigris_use_cache = TRUE
)

source("renv/activate.R")

if (interactive()) {
  suppressMessages(require(devtools))
}

# Función R completa para check() con exclusión temporal
clean_check <- function() {
  # Crear directorio de backup
  if (!dir.exists("../temp_backup")) {
    dir.create("../temp_backup")
  }
  
  # Lista de directorios/archivos a mover temporalmente
  items_to_move <- c("output", 
                     ".dvc", 
                     ".dvcignore",
                     "raw-data.dvc",
                     ".quarto",
                     ".Rhistory",
                     ".git",
                     "raw-data",
                     "docs",
                     "investigation-phases", 
                     "figures",
                     "multicore-scripts", 
                     "renv",
                     "_quarto.yml",
                     "about.qmd",
                     "index.qmd")
  
  # Identificar qué items existen
  existing_items <- items_to_move[file.exists(items_to_move)]
  
  cat("Moviendo temporalmente:", paste(existing_items, collapse = ", "), "\n")
  
  # Mover items existentes
  for (item in existing_items) {
    file.rename(item, file.path("../temp_backup", basename(item)))
  }
  
  # Ejecutar check()
  cat("Ejecutando devtools::check()...\n")
  result <- tryCatch({
    devtools::check()
  }, error = function(e) {
    cat("Error durante check():", e$message, "\n")
    return(NULL)
  })
  
  # Restaurar archivos
  cat("Restaurando archivos...\n")
  backup_items <- list.files("../temp_backup",
                             all.files = TRUE,
                             full.names = FALSE)
  
  for (item in backup_items) {
    file.rename(file.path("../temp_backup", item), item)
  }
  
  # Limpiar directorio de backup
  unlink("../temp_backup", recursive = TRUE)
  
  cat("Proceso completado. Archivos restaurados.\n")
  return(result)
}
