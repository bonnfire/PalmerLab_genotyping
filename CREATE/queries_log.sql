-- !preview conn=DBI::dbConnect(RSQLite::SQLite())
---------------------------
-- save workspace for inserting from copy 
SELECT * FROM information_schema.tables WHERE table_schema = 'u01_olivier_george_oxycodone'
DROP TABLE u01_olivier_george_oxycodone.wfu_master;

DROP TABLE u01_olivier_george_oxycodone.cohorttoadd;

CREATE TABLE u01_olivier_george_oxycodone.cohorttoadd (
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
	CONSTRAINT olivier_oxy_rfid_unique UNIQUE(rfid)
);

create trigger addtomaster after insert on u01_olivier_george_oxycodone.cohorttoadd
for each row execute procedure addtomaster();

COPY u01_olivier_george_oxycodone.cohorttoadd
FROM '/home/bonnie/Desktop/Database/csv files/u01_olivier_george_oxycodone/mastertable_c01_08_olivieroxy.csv' WITH (FORMAT 'csv', HEADER, NULL 'NA'); 

create or replace function addtomaster() returns trigger as $$
begin
insert into u01_olivier_george_oxycodone.wfu_master(cohort, sires, dames, labanimalid, accessid, sex, rfid, dob, dow, shipmentdate, litternumber, littersize, coatcolor, earpunch, rack, shipmentbox, shipmentage, comments, resolution)
		values (NEW.cohort, NEW.sires, NEW.dames, NEW.labanimalid, NEW.accessid, NEW.sex, NEW.rfid, NEW.dob, NEW.dow, NEW.shipmentdate, NEW.litternumber, NEW.littersize, NEW.coatcolor, NEW.earpunch, NEW.rack, NEW.shipmentbox, NEW.shipmentage, NEW.comments, NEW.resolution);
		return new;		
end;
$$ language plpgsql;


select * from u01_olivier_george_oxycodone.wfu_master;


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

INSERT INTO u01_olivier_george_oxycodone.wfu_master
values ('C01', '72350_1', '72384_2', '125', '73003_5', 'M', '933000120138488', '2018-05-11', '2018-06-01', '2018-06-18', '7', '6', 'BROWN', 'LT', 'D-A3', '11', '38', '21', 'NA', 'NA')



create or replace function newsample() returns trigger as $$
begin
    if (NEW.rfid like '933000%') then
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
    $$ language plpgsql;

-------------
BEGIN;

CREATE OR REPLACE FUNCTION staging_stats_ait()
RETURNS TRIGGER AS
$$
DECLARE
    v_hike_id INTEGER;
BEGIN
INSERT INTO hiking_stats(day_walked, cal_burned, miles_walked, duration, mph, additional_weight, trekking_poles, shoe_id)
VALUES(NEW.s_day_walked, NEW.s_cal_burned, NEW.s_miles_walked, NEW.s_duration, NEW.s_mph, NEW.s_additional_weight, NEW.s_trekking_poles, NEW.s_shoe_id);
SELECT hike_id INTO STRICT v_hike_id FROM hiking_stats WHERE day_walked = NEW.s_day_walked;
INSERT INTO hiking_trail(th_id, tr_id)
VALUES(v_hike_id, NEW.s_trail_id);
BEGIN
    IF NEW.s_additional_weight IS TRUE THEN
        INSERT INTO weight(wh_id, weight_added)
        VALUES (v_hike_id, NEW.s_weight_amount);
    END IF;
    END;
    RETURN NEW;
END;
$$ LANGUAGE PLpgSQL;

CREATE TRIGGER staging_stats_ait
AFTER INSERT ON staging_stats
FOR EACH ROW
EXECUTE PROCEDURE staging_stats_ait();

SAVEPOINT first_save;

\copy staging_stats FROM '~/Practice_Data/Fitness_DB_Data/stats.csv' WITH DELIMITER ',' CSV HEADER;

SAVEPOINT second_save;

-------

CREATE OR REPLACE FUNCTION copyfromadd()
RETURNS TRIGGER AS
$$
DECLARE
    v_hike_id INTEGER;
BEGIN
INSERT INTO hiking_stats(day_walked, cal_burned, miles_walked, duration, mph, additional_weight, trekking_poles, shoe_id)
VALUES(NEW.s_day_walked, NEW.s_cal_burned, NEW.s_miles_walked, NEW.s_duration, NEW.s_mph, NEW.s_additional_weight, NEW.s_trekking_poles, NEW.s_shoe_id);
SELECT hike_id INTO STRICT v_hike_id FROM hiking_stats WHERE day_walked = NEW.s_day_walked;
INSERT INTO hiking_trail(th_id, tr_id)
VALUES(v_hike_id, NEW.s_trail_id);
BEGIN
    IF NEW.s_additional_weight IS TRUE THEN
        INSERT INTO weight(wh_id, weight_added)
        VALUES (v_hike_id, NEW.s_weight_amount);
    END IF;
    END;
    RETURN NEW;
END;
$$ LANGUAGE PLpgSQL;

CREATE TRIGGER staging_stats_ait
AFTER INSERT ON staging_stats
FOR EACH ROW
EXECUTE PROCEDURE staging_stats_ait();



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

INSERT INTO u01_olivier_george_oxycodone.wfu_master_test
values ('C01', '72350_1', '72384_2', '125', '73003_5', 'M', '933000120138488', '2018-05-11', '2018-06-01', '2018-06-18', '7', '6', 'BROWN', 'LT', 'D-A3', '11', '38', '21', 'NA', 'NA')

insert into r01_su_guo_larvae.master_subset
(mother, father, rfid)
values ('Z2622-F1', 'Z2622-M1', '20191209-Plate1-1D');





---------------------------
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



