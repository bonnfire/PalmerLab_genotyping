-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT 1

create or replace function newsample() returns trigger as $sample_info_trg$
begin
    if (NEW.rfid like '^933000') then
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'rat', 'Heterogenous stock', 'NA');
		return new;
	
	elseif (NEW.rfid like '%Plate%') then
	-- XX make more robust
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, 'NA', 'NA', TG_TABLE_SCHEMA, 'zebrafish', 'Ekkwill fish', 'NA');
		return new;
		
	elseif (NEW.rfid like '^[[:digit:]]{1,3}$') then
	-- check in with Apurva about SD rat id's
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'rat', 'Sprague Dawley', 'NA');
		return new;
		
	elseif (NEW.rfid like '^p.cal') then
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'california mouse', 'Peromyscus californicus', 'NA');
		return new;
	
	end if;
	end;
    $sample_info_trg$ language plpgsql;
    
select distinct organism, strain from sample_tracking.sample_metadata; 
select * from sample_tracking.sample_metadata where organism = 'california mouse' limit 1;
select * from sample_tracking.sample_metadata where rfid like '[0-9]{1,3}';
select * from sample_tracking.sample_metadata where rfid like '[0-9]{1,3}';


create trigger sample_info_trg after insert on u01_olivier_george_cocaine.wfu_master_subset
for each row execute procedure newsample();

create trigger sample_info_trg_another after insert on u01_olivier_george_cocaine.wfu_master_subset
for each row execute procedure newsample();
 
CREATE TABLE u01_olivier_george_cocaine.wfu_master_subset AS
SELECT * FROM u01_olivier_george_cocaine.wfu_master where rfid in (select rfid from sample_tracking.sample_metadata_subset);

select * from u01_olivier_george_cocaine.wfu_master_subset;
select * from sample_tracking.sample_metadata_subset;

insert INTO u01_olivier_george_cocaine.wfu_master_subset
SELECT * from u01_olivier_george_cocaine.wfu_master
where cohort = 'C03'
limit 5;
--limit 5 offset 5;

DELETE FROM sample_tracking.sample_metadata_subset
WHERE project_name = '%1$s';

DELETE FROM u01_olivier_george_cocaine.wfu_master_subset
WHERE cohort = 'C03';

UPDATE u01_olivier_george_cocaine.wfu_master SET coatcolor = REPLACE(coatcolor,' ','')

---- example with another schema (u01_tom_jhou)
CREATE TABLE u01_tom_jhou.wfu_master_subset AS
SELECT * FROM u01_tom_jhou.wfu_master limit 10;

create trigger sample_info_trg_jhou after insert on u01_tom_jhou.wfu_master_subset
for each row execute procedure newsample();

insert INTO u01_tom_jhou.wfu_master_subset
SELECT * from u01_tom_jhou.wfu_master
limit 5 offset 10;

select * from u01_tom_jhou.wfu_master_subset

--- example with another organism (zebrafish)
CREATE TABLE r01_su_guo.master_subset(
mother varchar(8),
father varchar(8),
rfid text not null
);

insert into r01_su_guo.master_subset
(mother, father, rfid)
values ('Z2622-F1', 'Z2622-M1', '20191209-Plate1');

create trigger sample_info_trg_zeb_larvae after insert on r01_su_guo.master_subset
for each row execute procedure newsample();

insert into r01_su_guo.master_subset
(mother, father, rfid)
values ('Z2622-F1', 'Z2622-M1', '20191209-Plate1-1D');





------ trying to figure out why 
CREATE TABLE u01_olivier_george_oxycodone.wfu_master (
	cohort VARCHAR(3) NOT NULL, 
	sires VARCHAR(8) NOT NULL, 
	dames VARCHAR(8) NOT NULL, 
	labanimalid VARCHAR(6) NOT NULL, 
	accessid VARCHAR(8) NOT NULL, 
	sex VARCHAR(1) NOT NULL, 
	rfid VARCHAR(15) NOT NULL, 
	dob DATE NOT NULL, 
	dow DATE NOT NULL, 
	shipmentdate DATE NOT NULL, 
	litternumber DECIMAL NOT NULL, 
	littersize DECIMAL NOT NULL, 
	coatcolor VARCHAR(10) NOT NULL, 
	earpunch VARCHAR(11) NOT NULL, 
	rack VARCHAR(4) NOT NULL, 
	shipmentbox DECIMAL NOT NULL, 
	shipmentage DECIMAL NOT NULL, 
	weanage DECIMAL NOT NULL, 
	comments VARCHAR(27), 
	resolution VARCHAR(33),
	CONSTRAINT olivier_oxy_master_pk UNIQUE(cohort, rfid)
);

create trigger sample_info_trg after insert on u01_olivier_george_oxycodone.wfu_master
for each row execute procedure newsample();

COPY u01_olivier_george_oxycodone.wfu_master
FROM '/home/bonnie/Desktop/Database/csv files/u01_olivier_george_oxycodone/mastertable_c01_08_olivieroxy.csv' WITH (FORMAT 'csv', HEADER, NULL 'NA'); 

END LOOP;
   RETURN NULL;

create or replace function newsample() returns trigger as $sample_info_trg$
begin
    if (NEW.rfid like '^933000') then
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'rat', 'Heterogenous stock', 'NA');
		return new;
	
	elseif (NEW.rfid like '%Plate%') then
	-- XX make more robust
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, 'NA', 'NA', TG_TABLE_SCHEMA, 'zebrafish', 'Ekkwill fish', 'NA');
		return new;
		
	elseif (NEW.rfid like '^[[:digit:]]{1,3}$') then
	-- check in with Apurva about SD rat id's
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'rat', 'Sprague Dawley', 'NA');
		return new;
		
	elseif (NEW.rfid like '^p.cal') then
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'california mouse', 'Peromyscus californicus', 'NA');
		return new;
	
	end if;
	end;
    $sample_info_trg$ language plpgsql;






CREATE TABLE u01_olivier_george_oxycodone.wfu_master_test (
	cohort VARCHAR(3) NOT NULL, 
	sires VARCHAR(8) NOT NULL, 
	dames VARCHAR(8) NOT NULL, 
	labanimalid VARCHAR(6) NOT NULL, 
	accessid VARCHAR(8) NOT NULL, 
	sex VARCHAR(1) NOT NULL, 
	rfid VARCHAR(15) NOT NULL, 
	dob DATE NOT NULL, 
	dow DATE NOT NULL, 
	shipmentdate DATE NOT NULL, 
	litternumber DECIMAL NOT NULL, 
	littersize DECIMAL NOT NULL, 
	coatcolor VARCHAR(10) NOT NULL, 
	earpunch VARCHAR(11) NOT NULL, 
	rack VARCHAR(4) NOT NULL, 
	shipmentbox DECIMAL NOT NULL, 
	shipmentage DECIMAL NOT NULL, 
	weanage DECIMAL NOT NULL, 
	comments VARCHAR(27), 
	resolution VARCHAR(33),
	CONSTRAINT olivier_oxy_master_pk UNIQUE(cohort, rfid)
);

create trigger sample_info_trg_olivier_oxy after insert on u01_olivier_george_oxycodone.wfu_master_test
for each row execute procedure newsample();

-- ERROR:  control reached end of trigger procedure without RETURN
INSERT INTO u01_olivier_george_oxycodone.wfu_master_test
values ('C01', '72350_1', '72384_2', '125', '73003_5', 'M', '933000120138488', '2018-05-11', '2018-06-01', '2018-06-18', '7', '6', 'BROWN', 'LT', 'D-A3', '11', '38', '21', 'NA', 'NA')



