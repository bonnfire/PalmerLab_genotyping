CREATE TABLE metadata_hsrats_n1536_corrected_v2 (
	rfid VARCHAR(15) NOT NULL, 
	runid VARCHAR(29) NOT NULL, 
	fastq_files VARCHAR(83) NOT NULL, 
	library_name VARCHAR(9) NOT NULL, 
	pcr_barcode DECIMAL NOT NULL, 
	project_name VARCHAR(32) NOT NULL, 
	barcode VARCHAR(8) NOT NULL, 
	filename DATE NOT NULL, 
	comments VARCHAR(33), 
	flag VARCHAR(12), 
	sex VARCHAR(1) NOT NULL, 
	coatcolor VARCHAR(9) NOT NULL, 
	organism VARCHAR(3) NOT NULL, 
	strain VARCHAR(18) NOT NULL, 
	dames VARCHAR(10) NOT NULL, 
	sires VARCHAR(10) NOT NULL
);
