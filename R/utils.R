#'@noRd
.connect_to_db <- function(){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  db

}


#' Delete samples database
#' @noRd
.reset_samples_db <- function() {
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

  if(file.exists(db_path)) {
    file.rename(db_path, paste0(db_path, "_old"))
  }

}

#' Return metadata table for sample list
#' @noRd
.get_samplesdb_metadata <- function(){
  .check_sample_db()
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  platesdb <- DBI::dbGetQuery(db, "SELECT * FROM platesdb")
  duckdb::dbDisconnect(db, shutdown = TRUE)

  platesdb
}

.get_samplelist <- function(id){
  .check_sample_db()
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  sample_list <- DBI::dbGetQuery(db, paste0("SELECT * FROM samples WHERE list_id = ", id))
  duckdb::dbDisconnect(db, shutdown = TRUE)
  sample_list
}

# create it if not exists
.check_sample_db <- function() {

  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")

  # Check if the database file exists
  db <- duckdb::dbConnect(duckdb::duckdb(), db_path)
  # This id auto increments and is assigned to list_id above
  DBI::dbExecute(db, "
    CREATE TABLE IF NOT EXISTS platesdb (
      list_id INTEGER PRIMARY KEY, 
      date TEXT,
      assoc_plates TEXT,
      description TEXT,
      UNIQUE(list_id)
    );
  ") # id, date, assoc_plates

  DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS samples (
    file_name TEXT PRIMARY KEY,

    list_id INTEGER REFERENCES platesdb(list_id),
    plate_id INTEGER,

    inlet_method TEXT,
    row INTEGER,
    col INTEGER,
    value TEXT,
    sample_location TEXT,
    samples TEXT,
    type TEXT,
    std_rep INTEGER,
    e_rep INTEGER,
    tray TEXT,
    inj_vol REAL,

    conc_a TEXT,
    conc_b TEXT,
    conc_c TEXT,
    conc_d TEXT,
    conc_e TEXT,
    conc_f TEXT,
    conc_g TEXT,
    conc_h TEXT,
    conc_i TEXT,
    conc_j TEXT,
    conc_k TEXT,
    conc_l TEXT,
    conc_m TEXT,
    conc_n TEXT,
    conc_o TEXT,
    conc_p TEXT,

    compound_a TEXT,
    compound_b TEXT,
    compound_c TEXT,
    compound_d TEXT,
    compound_e TEXT,
    compound_f TEXT,
    compound_g TEXT,
    compound_h TEXT,
    compound_i TEXT,
    compound_j TEXT,
    compound_k TEXT,
    compound_m TEXT,
    compound_n TEXT,
    compound_l TEXT,
    compound_o TEXT,
    compound_p TEXT,

    file_text TEXT,
    a_group TEXT,
    time TEXT,
    factor TEXT,
    dil REAL,
    dose TEXT,
    II REAL,
    addl INTEGER,
    route TEXT,
    cmt TEXT,
    sex TEXT,

    UNIQUE(file_name)
  );
")


DBI::dbExecute(db, " 
  CREATE TABLE IF NOT EXISTS chromexpdb (
    exp_id INTEGER PRIMARY KEY,
    exp_name TEXT NOT NULL
  )
")

# methods tab
## method_descr: description of the method
DBI::dbExecute(db, "
CREATE TABLE IF NOT EXISTS methodstab (
  method_id INTEGER PRIMARY KEY,
  method TEXT NOT NULL,
  method_descr TEXT,
  method_gradient TEXT,
  method_column TEXT,
  UNIQUE(method_id),
  UNIQUE(method)
);" )

# chromatogram table
DBI::dbExecute(db, "
CREATE TABLE IF NOT EXISTS chroms (
  chrom_id INTEGER PRIMARY KEY,
  exp_id INTEGER NOT NULL REFERENCES chromexpdb(exp_id),
  method_id INTEGER NOT NULL REFERENCES methodstab(method_id),
  file_name TEXT NOT NULL,
  type TEXT,
  sample_location TEXT,
  inj_vol REAL,
  date TEXT,
  UNIQUE(file_name)
);")




# gradient methods table
## method_id: this will auto increment and unique number
## method_gradient: gradient of the method
## q1: q1 value
## q3: q3 value
## inlet_method: inlet method
## transition_label: q1 > q3
## transition_id: T1, T2, T3, etc
## last unique assertations might be important to avoid repeating identical method entries

DBI::dbExecute(db, "
CREATE TABLE IF NOT EXISTS transtab (
  transition_id INTEGER PRIMARY KEY,
  transition_label TEXT,
  method_id INTEGER NOT NULL REFERENCES methodstab(method_id),
  q1 REAL,
  q3 REAL,
  UNIQUE(method_id, transition_id)
);" )


# non on the three first columns are unique.
# the unqiuness is based on all method_id trans_id compound_id
# IS is a property of compound. Call get_IS_name to get the IS for a compound
DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS compoundstab (
    compound_id INTEGER NOT NULL PRIMARY KEY,
    transition_id INTEGER NOT NULL REFERENCES transtab(transition_id),
    qualifier BOOLEAN NOT NULL,
    compound TEXT,
    expected_peak_start REAL,
    expected_peak_end REAL,
    expected_rt REAL,
    IS_id TEXT,
    UNIQUE(transition_id, compound_id)
  );

")

DBI::dbExecute(db, "
  CREATE TABLE IF NOT EXISTS peakstab (
    peak_id INTEGER PRIMARY KEY,
    chrom_id INTEGER NOT NULL REFERENCES chroms(chrom_id),
    compound_id INTEGER NOT NULL REFERENCES compoundstab(compound_id),
    observed_rt REAL,
    observed_rt_start REAL,
    observed_rt_end REAL,
    observed_peak_height REAL,
    area REAL,
    manual INTEGER NOT NULL DEFAULT 0,
    date TEXT
  );

")

studydesign_db(db)


duckdb::dbDisconnect(db, shutdown = TRUE)
}



rename_db_col <- function(old, new, tablename){
  db_path <- PKbioanalysis_env$data_dir |>
    file.path("samples.db")
  db <- duckdb::dbConnect(duckdb::duckdb(), dbdir = db_path)
  DBI::dbExecute(db, paste0("ALTER TABLE ", tablename, " RENAME COLUMN ", old, " TO ", new))
  duckdb::dbDisconnect(db, shutdown = TRUE)
}


studydesign_db <- function(con){
# 1. Study
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS study (
    study_id      TEXT PRIMARY KEY,
    study_type    TEXT CHECK (study_type IN ('SAD', 'MAD', 'FE', 'BE', 'NA')),
    title         TEXT,
    design        TEXT,
    phase         TEXT,
    start_date    DATE,
    end_date      DATE
  );
  ")

# 2. Subject
# screen_number: unique number for each subject in the study
# randomization_number: unique number for each subject in the study, used for randomization
# arm: the arm the subject is assigned to, e.g., "A - 100 mg crossover"

DBI::dbExecute(con, "
  CREATE TABLE  IF NOT EXISTS subject (
    subject_id          TEXT PRIMARY KEY,
    study_id            TEXT REFERENCES study(study_id),
    screen_number       TEXT,
    randomization_number TEXT, 
    sex                 TEXT,
    age                 INTEGER,
    weight              REAL,
    arm                 TEXT, 
    UNIQUE(subject_id, study_id, screen_number, randomization_number)
  );
  ")

# 3. Cohort
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS cohort (
    cohort_id      TEXT PRIMARY KEY,
    study_id       TEXT REFERENCES study(study_id),
    cohort_label   TEXT,
    dose_mg        REAL,
    periods        INTEGER,
    food_condition TEXT CHECK (food_condition IN ('Fasted', 'Fed', 'NA')),
    n_subjects     INTEGER,
    is_sentinel    BOOLEAN, 
    UNIQUE(cohort_id, study_id, cohort_label)
  );
  ")

# 4. Subject-Cohort Map
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS subject_cohort (
    subject_id     TEXT REFERENCES subject(subject_id),
    cohort_id      TEXT REFERENCES cohort(cohort_id),
    period_number  INTEGER,
    treatment      TEXT,
    PRIMARY KEY (subject_id, cohort_id, period_number), 
    UNIQUE(subject_id, cohort_id, period_number)
  );
  ")

# 5. Dosing
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS dosing (
    subject_id     TEXT REFERENCES subject(subject_id),
    cohort_id      TEXT REFERENCES cohort(cohort_id),
    period_number  INTEGER,
    dose_time      TIMESTAMP,
    dose_amount    REAL,
    dose_unit      TEXT CHECK (dose_unit IN ('mg', 'mL', 'NA')),
    II             REAL,
    addl           INTEGER,
    route          TEXT CHECK (route IN ('PO', 'IV', 'SC', 'IM', 'IP', 'NA')),
    formulation    TEXT
  );
  ")

# 6. PK Sample
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS pk_sample (
    subject_id     TEXT REFERENCES subject(subject_id),
    cohort_id      TEXT REFERENCES cohort(cohort_id),
    period_number  INTEGER,
    analyte        TEXT,
    CMT            TEXT,
    EVENT_ID       TEXT,
    DV             REAL,
    nominal_time   REAL,
    actual_time    TIMESTAMP,
    conc_ng_mL     REAL,
    analyzed_in    TEXT REFERENCES samples(file_name)
  );
  ")

# 7. Adverse Events
  DBI::dbExecute(con, "
  CREATE TABLE IF NOT EXISTS ae (
    ae_id              TEXT PRIMARY KEY,
    subject_id         TEXT REFERENCES subject(subject_id),
    start_time         TIMESTAMP,
    end_time           TIMESTAMP,
    severity           TEXT,
    relationship_to_study_drug TEXT,
    preferred_term     TEXT
  );
  ")

# 8. Vital Signs
  dbExecute(con, "
  CREATE TABLE IF NOT EXISTS vital_sign (
    subject_id      TEXT REFERENCES subject(subject_id),
    visit           TEXT,
    vital_type      TEXT,  -- e.g., HR, BP
    value           REAL,
    collection_time TIMESTAMP
  );
  ")

}