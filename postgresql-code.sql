-- create a new npi table with selected fields
DROP table npi_20160710_select;
CREATE TABLE npi_20160710_select AS
SELECT npi_id, entity_type_code,
provider_organization_name_legal_business_name,
provider_first_line_business_practice_location_address,
provider_second_line_business_practice_location_address,
provider_business_practice_location_address_city_name,
provider_business_practice_location_address_state_name,
provider_business_practice_location_address_postal_code,
provider_enumeration_date,last_update_date, npi_deactivation_reason_code,
npi_deactivation_date, npi_reactivation_date, provider_gender_code,
healthcare_provider_taxonomy_code_1
from npi_20160710

-- Create a table for patient sharing data of 2013, 180 period
DROP TABLE patient_share_2013_180;
CREATE TABLE patient_share_2013_180
(
  npi1_13 varchar(10),
  npi2_13 int,
  pair_count_13 integer,
  bene_count_13 integer,
  same_day_count_13 integer
)

DROP TABLE patient_share_2013_180_ca1;
CREATE TABLE patient_share_2013_180_ca1 AS
SELECT patient_share_2013_180.*, npi_20160710_ca_full.* FROM patient_share_2013_180
INNER JOIN npi_20160710_ca_full ON patient_share_2013_180.npi1 = npi_20160710_ca_full.npi_id

