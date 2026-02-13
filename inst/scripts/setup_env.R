# setup_env()
# interactive environment setup assistant for first-time configuration.
# Guides the user through installation and validation of the required
# Python (Cellpose) environment, checks R package availability,
# validates GPU support and stores the configured Python path for
# subsequent analysis sessions.
setup_env <- function() {

  # initial information
  cat("This routin checks all conditions for sucessfull analysis.\n\n")

  # helper: yes/no prompt with cancellation support
  prompt_yn <- function(q) {
    repeat{
      ans <- tolower(trimws(readline(paste0(q, " (y/n or 'stop' to cancel): "))))

      if(ans %in% c("stop", "exit", "quit")) {
        cat("The operation was canceld by the user.\n")
        return("Termination")
      }

      if(ans %in% c("y", "yes")) return(TRUE)

      if(ans %in% c ("n", "no")) return(FALSE)

      cat("Invalid input. Please enter 'y'/'yes' or 'n'/'no' (or 'stop' to cancel).\n")
    }
  }

  # helper: formatted section header
  rule <- function(txt = "") {
    line <- paste(rep("=", 72), collapse = "")
    cat("\n", line, "\n", txt, "\n", line, "\n", sep = "")
  }

  # 1) check whether setup was already performed in this session
  rule("1) This R session")

  ans <- prompt_yn("Have you already used this tool in this R session?")

  if(identical(ans, "Termination")) return(invisible(NULL))

  if(isTRUE(ans)) {
    cat("OK - Setup is skipped. You can start the analysis directly.\n")
    return(invisible(TRUE))
  }

  # 2) verify required R packages
  rule("2) Check R packages")

  required_pkgs <- c("biopixR", "EBImage", "DT", "reticulate")

  recheck_pkgs <- function() {
    missing <- required_pkgs[!sapply(required_pkgs, requireNamespace, quitly = TRUE)]

    if(length(missing) == 0) return(TRUE)

    cat("The following R packages are missing: ", paste(missing,
                                                        collpase = ", "),
        "\n", sep = "")

    cat("Please install them using:\n",
        "install.packages(c('", paste(missing,
                                      collpase = "', '"),
        "'))\n", sep = "")

    invisible(readline("Press Enter once installation is complete..."))
    FALSE
  }

  while(!recheck_pkgs()) {}

  cat("All required R packages are available.\n")

  # 3) first-time system setup
  rule("3) First-time setup?")

  ever <- prompt_yn("Have you used this tool on this system before?")

  if(identical(ever, "Termination")) return(invisible(NULL))

  if(!isTRUE(ever)) {

    # 3a) Miniforge / Miniconda installation
    rule("3a) Miniforge/ Miniconda")
    has_mini <- prompt_yn("Is Miniforge (recommended) or Minicoda already installed?")

    if(identical(has_mini, "Termination")) return(invisible(NULL))

    if(!isTRUE(has_mini)) {
      cat("\nPlease install Miniforge (recommended):\n",
          " - Windows/macOS/Linux: https://github.com/conda-forge/miniforge/releases\n", sep = "")
      invisible(readline("\nPress Enter once installation is complete... "))
    } else {
      cat("Miniforge/Miniconda installled.\n")
    }

    # 3b) create conda environment
    rule("3b) Create Connda environment")

    cat("Please open a Miniforge/Conde prompt and run: \n\n",
        "conda create -n cellpose python=3.10\n",
        "conda activate cellpose\n\n", sep = "")

    invisible(readline("Press Enter once the environment has been created/activated... "))

    # 3c) install Cellpose and optional GPU support
    rule("3c) Install Cellpose (GPU optional)")

    cat("Note: GPU support (via NVIDIA) is strongly recommended - CPU mode is significantly slower.\n\n")
    use_gpu <- prompt_yn("Would you like to use 'Cellpose' with an NVIDIA GPU?")

    if(identical(use_gpu, "Termination")) return(invisible(NULL))

    if(isTRUE(use_gpu)) {
      cat("\nInstall within the activated 'Cellpose' environment (CUDA 12.1 example):\n\n",
          "pip install torch torchvision torchaudio --index-url https://download.pytorch.org/whl/cu121\n",
          "pip install 'cellpose[all]'\n\n",
          "GPU test:\n",
          "python -c \"import torch; print('CUDA available:' torch.cuda.is_available())\"\n\n",
          "The GPU version works even without a seperatly installed CUDA Toolkit,\n",
          "as long as a modern NVIDIA driver is installed (PyTorch includes CUDA internally).\n\n",
          sep = "")
    } else {
      cat("\nCPU instalation in the activated 'cellpose' environment:\n\n",
          "pip install torch torchvision torchaudio\n",
          "pip install 'cellpose[all]'\n\n", sep = "")
    }

    cat("Optional (for DM3/DM4 metadata):\n pip install ncempy\n\n")
    invisible(readline("Press Enter once all packages are installed... "))
  } else {
      cat("Connecting to existing installation and testing it.\n")
  }

  # 4) connect R to the specified Python environment
  rule("4) Connection R <-> Python / Cellpose / ncempy / GPU")

  cat("Please provide the path to the Python executable of your 'cellpose' environment.\n",
      "Tip: 'conda env list' shows all environment paths.\n\n",
      "Examples:\n",
      " - Windows: C:/Users/<USER>/miniforge3/envs/cellpose/python.exe\n",
      " - macOS:   /Users/<USER>/miniforge3/envs/cellpose/bin/python\n",
      " - Linux:   /home/>USER>/miniforge3/envs/cellpose/bin/python\n\n", sep = "")

  python_path <- readline("Please enter the Python path (use /): ")

  if(!nzchar(python_path)) {
    cat("No Python path provided. Cancelling.\n")
    return(invisible(NULL))
  }

  if(!file.exists(python_path)) {
    cat("Path does not exist. Cancelling.\n")
    return(invisible(NULL))
  }

  # register Python with reticulate
  reticulate::use_python(python_path, required = TRUE)

  cfg_ok <- tryCatch({reticulate::py_config(); TRUE }, error = function(e) FALSE)

  if(!cfg_ok) {
    cat("Python connection failed. Please check path or installation.\n")
    return(invisible(NULL))
  }

  cat("Python detected at:\n ", reticulate::py_config()$python, "\n", sep ="")

  # verify Cellpose import
  ok_cellpose <- tryCatch({
    reticulate::py_run_string("import cellpose, sys; print('Cellpose OK, Python:', sys.executable)")
    TRUE
  }, error = function(e) FALSE)

  if(!ok_cellpose) {
    cat("Cellpose could not be imported. Please install it in the environment.\n")
    return(invisible(NULL))
  }

  cat("Cellpose import sucessfull.\n")

  # optional: check ncempy
  if(reticulate::py_module_available("ncempy")) {
    cat("ncempy found (DM3/DM4 support active).\n")
  } else {
    cat("ncempy not found - DM3/DM4 metadata will not be read automatically.\n")
  }

  # optional: check GPU via torch
  if(reticulate::py_module_available("torch")) {
    torch_msg <- tryCatch({
      reticulate::py_run_string("import torch; print('CUDA available:', torch.cuda.is_available())")
      TRUE
    }, error = function(e) FALSE)

    if(torch_msg) cat("Torch checked (see CUDA output above).\n")
    else cat("Torch could not be tested.\n")
  } else {
    cat("Torch is not installed - Cellpose will run in CPU mode.\n")
  }

  # 5) summary of detected Python modules
  rule("5) Summary of Python moduls")

  modules <- c("cellpose", "torch", "numpy", "ncempy")
  status <- sapply(modules, reticulate::py_module_available)

  for(mod in names(status)) {
    cat(if(status[[mod]]) "Yes" else "No", " ", mod,
        if(!status[[mod]]) " (not found)" else "", "\n", sep = "")
  }

  # store configured Python path for future sessions
  options(cellpose.python = python_path)
  rule("Setup complete")
  cat("The environment is fully prepared. You can start your analysis.\n")
  invisible(TRUE)
}
