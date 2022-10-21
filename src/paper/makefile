OUT = ../../gen/output

all: $(OUT)/amazon_usa_clean.csv inspection_and_graphs.R models.R

$(OUT)/amazon_usa_clean.csv: inspection_and_graphs.R
	R --vanilla inspection_and_graphs.R

$(OUT)/amazon_usa_clean.csv: models.R
	R --vanilla models.R