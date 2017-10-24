# lookup tables: indicator code <-> indicator name
# Both UI.R and server.R will use them so we leave them here in global.R

# from code to name
codeToName <- c(
  AG.LND.AGRI.ZS = "Agricultural land (% of land area)"
)

# from name to code
nameToCode <- names(codeToName)
names(nameToCode) <- codeToName


