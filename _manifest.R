appFiles <- c(
  "app.R",
  "renv.lock",
  ".posit/publish/posit-conf-chat-3GSU.toml",
  ".posit/publish/deployments/deployment-8UGH.toml",
  "R",
  "README.md",
  "_brand.yml",
  "assets",
  "data/derived",
  "prompts",
  "skills"
)

rsconnect::writeManifest(appDir = ".", appFiles = appFiles)
