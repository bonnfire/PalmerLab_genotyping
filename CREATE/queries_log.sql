-- !preview conn=DBI::dbConnect(RSQLite::SQLite())

SELECT 1

create or replace function newsample() returns trigger as $sample_info_trg$
begin
    if (NEW.rfid like '^933000') then
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'rat', 'Heterogenous stock', 'NA');
		return new;
	
	elseif (NEW.rfid like '%Plate%') then
		insert into sample_tracking.sample_metadata_subset(rfid, sex, coatcolor, project_name, organism, strain, comments)
		values (NEW.rfid, NEW.sex, new.coatcolor, TG_TABLE_SCHEMA, 'zebrafish', 'Ekkwill fish', 'NA');
		return new;
		
	elseif (NEW.rfid like '^[[:digit:]]{1,3}$') then
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

