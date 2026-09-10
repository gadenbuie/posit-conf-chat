RSCRIPT ?= Rscript

.PHONY: schedule ragnar manifest preflight

schedule:
	$(RSCRIPT) data/get_schedule_zuddl.R

ragnar:
	DUCKDB_R_HOME="~/.duckdb" $(RSCRIPT) data/posit-ragnar.R

manifest:
	$(RSCRIPT) _manifest.R

preflight: schedule ragnar manifest
