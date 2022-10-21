DATA = ../../data
TEMP = ../../gen/temp
OUTPUT = ../../gen/output

all: $(DATA)/opvallendheid_kleuren_smartphone.xlsx $(TEMP)/amazon_complete_dataset_0_1000.csv $(OUTPUT)/amazon_usa_clean.csv

$(OUTPUT)/amazon_usa_clean.csv: $(DATA)/opvallendheid_kleuren_smartphone.xlsx $(TEMP)/amazon_complete_dataset_0_1000.csv add_variables.R
	R --vanilla < add_variables.R