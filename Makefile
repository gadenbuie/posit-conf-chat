RSCRIPT ?= Rscript
OPENROUTER_MODEL ?= z-ai/glm-5.3-flash

.PHONY: schedule ragnar manifest preflight openrouter

schedule:
	$(RSCRIPT) data/get_schedule_zuddl.R

ragnar:
	DUCKDB_R_HOME="~/.duckdb" $(RSCRIPT) data/posit-ragnar.R

manifest:
	$(RSCRIPT) _manifest.R

openrouter:
	$(RSCRIPT) data/get_openrouter_providers.R $(OPENROUTER_MODEL)

preflight: schedule ragnar manifest
