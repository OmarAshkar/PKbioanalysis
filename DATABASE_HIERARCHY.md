# PKbioanalysis Database Structure and Hierarchy

## Overview

The PKbioanalysis package uses a DuckDB database (`samples.db`) to
manage chromatographic data, study design, samples, and quantitative
results. Below is a comprehensive diagram showing the relationships
between tables and R classes.

------------------------------------------------------------------------

## Database Schema Hierarchy

### 1. **Core Chromatography Tables**

    ┌─────────────────────────────────────────────────────┐
    │                  chromexpdb                         │
    │  ┌───────────────────────────────────────────────┐  │
    │  │ exp_id (PK)                                   │  │
    │  │ exp_name                                      │  │
    │  └───────────────────────────────────────────────┘  │
    └──────────┬──────────────────────────────────────────┘
               │
               │ 1:N
               │
    ┌──────────▼──────────────────────────────────────────┐
    │                    chroms                           │
    │  ┌───────────────────────────────────────────────┐  │
    │  │ chrom_id (PK)                                 │  │
    │  │ exp_id (FK) → chromexpdb                      │  │
    │  │ method_id (FK) → methodstab                   │  │
    │  │ file_name (UNIQUE)                            │  │
    │  │ type, sample_location, inj_vol, date         │  │
    │  └───────────────────────────────────────────────┘  │
    └──────────┬──────────────────────────────────────────┘
               │
               │ 1:N
               │
    ┌──────────▼──────────────────────────────────────────┐
    │                    peakstab                         │
    │  ┌───────────────────────────────────────────────┐  │
    │  │ peak_id (PK)                                  │  │
    │  │ chrom_id (FK) → chroms                        │  │
    │  │ compound_id (FK) → compoundstab              │  │
    │  │ observed_rt, observed_rt_start/end            │  │
    │  │ observed_peak_height, area                    │  │
    │  │ manual, date                                  │  │
    │  └───────────────────────────────────────────────┘  │
    └─────────────────────────────────────────────────────┘

### 2. **Method Configuration Tables**

    ┌─────────────────────────────────────────────────────┐
    │                   methodstab                        │
    │  ┌───────────────────────────────────────────────┐  │
    │  │ method_id (PK)                                │  │
    │  │ method (UNIQUE)                               │  │
    │  │ method_descr, method_gradient, method_column │  │
    │  └───────────────────────────────────────────────┘  │
    └──────────┬──────────────┬────────────────────────────┘
               │              │
               │ 1:N          │ 1:N
               │              │
        ┌──────▼────┐    ┌────▼─────────────────┐
        │  transtab │    │   compoundstab       │
        │ ┌────────┐│    │  ┌──────────────────┐│
        │ │transID ││    │  │ compound_id (PK) ││
        │ │method_ ││    │  │ transition_id(FK)││
        │ │ id(FK) ││    │  │ qualifier        ││
        │ │ q1, q3 ││    │  │ compound         ││
        │ │        ││    │  │ expected_peak_*  ││
        │ └────────┘│    │  │ expected_rt      ││
        └───────────┘    │  │ IS_id            ││
                         │  └──────────────────┘│
                         └──────────────────────┘

### 3. **Sample Management Tables**

    ┌─────────────────────────────────────────────────────┐
    │                    platesdb                         │
    │  ┌───────────────────────────────────────────────┐  │
    │  │ list_id (PK)                  [UNIQUE]       │  │
    │  │ date                                          │  │
    │  │ assoc_plates                                  │  │
    │  │ description                                   │  │
    │  └───────────────────────────────────────────────┘  │
    └──────────┬──────────────────────────────────────────┘
               │
               │ 1:N
               │
    ┌──────────▼──────────────────────────────────────────┐
    │                    samples                          │
    │  ┌───────────────────────────────────────────────┐  │
    │  │ file_name (PK, UNIQUE)                        │  │
    │  │ list_id (FK) → platesdb                       │  │
    │  │ injec_id (UNIQUE)                             │  │
    │  │ plate_id, row, col                            │  │
    │  │ study_id, log_id                              │  │
    │  │ inlet_method, sample_location                 │  │
    │  │ samples, type, std_rep, e_rep, tray           │  │
    │  │ inj_vol                                       │  │
    │  │ conc* (a-p), compound* (a-p)                  │  │
    │  │ file_text, a_group, factor, dil               │  │
    │  │ time, dose, dose_unit, ii, addl               │  │
    │  │ route, cmt, sex                               │  │
    │  └───────────────────────────────────────────────┘  │
    └─────────────────────────────────────────────────────┘

### 4. **Study Design Tables**

    ┌──────────────────────────────────────────────────────┐
    │                      study                           │
    │  ┌────────────────────────────────────────────────┐  │
    │  │ id (PK) [UUID]                                 │  │
    │  │ type (SD|MD|FE|BE|NA)                          │  │
    │  │ title, subject_type, pkstudy, description      │  │
    │  │ status, start_date, end_date                   │  │
    │  └────────────────────────────────────────────────┘  │
    └──────────┬──────────────┬──────────────┬─────────────┘
               │              │              │
          ┌────▼──┐    ┌──────▼──┐   ┌──────▼────┐
          │subject│    │ dosing  │   │sample_log │
          │┌──────┐│   │┌────────┐│   │┌────────┐ │
          ││ uuid_││   ││arm_id  ││   ││log_id  │ │
          ││subject│   ││study_id││   ││subject_││ │
          ││study_ ││   ││group_  ││   ││id      │ │
          ││id(FK)││   ││label   ││   ││study_id││ │
          ││subj_ ││   ││period_ ││   ││nominal_││ │
          ││id    ││   ││number  ││   ││time    │ │
          ││group ││   ││dose_*  ││   ││actual_ ││ │
          ││label ││   ││route   ││   ││time    │ │
          ││sex,  ││   ││formula ││   ││status  │ │
          ││age   ││   ││tion    ││   ││sample_ ││ │
          ││      ││   ││        ││   ││type    │ │
          │└──────┘│   │└────────┘│   │└────────┘ │
          └────────┘   └──────────┘   └───────────┘

### 5. **Quantitation Results Tables**

    ┌──────────────────────────────────────────┐
    │            quant_meta                    │
    │  ┌──────────────────────────────────┐   │
    │  │ quant_id (PK) [UUID]             │   │
    │  │ quant_date                       │   │
    │  └──────────────────────────────────┘   │
    └───────────┬────────────────────────────┘
                │
                │ 1:N
                │
    ┌───────────▼───────────────────────────────┐
    │          quant_samples                    │
    │  ┌─────────────────────────────────────┐  │
    │  │ quant_id (FK) → quant_meta          │  │
    │  │ log_id (soft FK) → sample_log       │  │
    │  │ file_name                           │  │
    │  │ compound_id (FK) → compoundstab     │  │
    │  │ concentration, conc_unit            │  │
    │  └─────────────────────────────────────┘  │
    └────────────────────────────────────────────┘

------------------------------------------------------------------------

## R Class Hierarchy

### **Plate Objects**

    ┌────────────────────────────────────────────────┐
    │              PlateObj                          │
    │  ┌────────────────────────────────────────┐   │
    │  │ plate: 96-well matrix                  │   │
    │  │ df: data.frame (metadata)              │   │
    │  │ samples_metadata: data.frame           │   │
    │  │ empty_rows: character vector           │   │
    │  │ filling_scheme: list                   │   │
    │  │ last_filled: character                 │   │
    │  │ last_modified: POSIXct                 │   │
    │  │ plate_id: character                    │   │
    │  │ descr: character                       │   │
    │  └────────────────────────────────────────┘   │
    └────────────────┬─────────────────────────────┘
                     │
                     │ inherits
                     │
    ┌────────────────▼──────────────────────────────┐
    │          RegisteredPlate                       │
    │  (extends PlateObj)                           │
    └────────────────────────────────────────────────┘

    ┌────────────────────────────────────────────────┐
    │            MultiPlate                          │
    │  ┌────────────────────────────────────────┐   │
    │  │ plates: list of PlateObj               │   │
    │  └────────────────────────────────────────┘   │
    └────────────────────────────────────────────────┘

### **Chromatography Objects**

    ┌──────────────────────────────────────────────────┐
    │          ChromResBase                            │
    │  ┌──────────────────────────────────────────┐   │
    │  │ metadata: data.frame                     │   │
    │  │ peaks: data.frame                        │   │
    │  │ transitions: data.frame                  │   │
    │  │ compounds: data.frame                    │   │
    │  │ vendor: character                        │   │
    │  │ pk_metadata: list                        │   │
    │  └──────────────────────────────────────────┘   │
    └──────────────────┬───────────────────────────────┘
                       │
                       │ inherits
                       │
    ┌──────────────────▼───────────────────────────────┐
    │             ChromRes                             │
    │  ┌───────────────────────────────────────────┐   │
    │  │ (all from ChromResBase)                   │   │
    │  │ runs: list                                │   │
    │  └───────────────────────────────────────────┘   │
    └──────────────────────────────────────────────────┘

### **Quantitation Object**

    ┌──────────────────────────────────────────────────┐
    │             QuantRes                             │
    │  ┌──────────────────────────────────────────┐   │
    │  │ samples_metadata: data.frame             │   │
    │  │ compounds_metadata: data.frame           │   │
    │  │ quanttab: list (by compound)             │   │
    │  │ linearity: list (linearity results)      │   │
    │  │ suitability: list (config & results)     │   │
    │  │ resEstim: list (residuals)               │   │
    │  │ pkdata: list (PK profiles)               │   │
    │  └──────────────────────────────────────────┘   │
    └──────────────────────────────────────────────────┘

------------------------------------------------------------------------

## Data Flow Relationships

### **From Plate to Database to Analysis**

    PlateObj
       │
       ├─► register_plate() ─► RegisteredPlate
       │                          │
       │                          └─► plates_cache/ (RDS files)
       │
       └─► build_injec_seq() ─► InjecListObj
                                  │
                                  └─► samples.db
                                       │
                                       ├─► platesdb (list metadata)
                                       ├─► samples (sample details)
                                       ├─► chroms (raw data)
                                       └─► peakstab (integrated peaks)

    Study Design
       │
       └─► study/subject/dosing/sample_log tables
            │
            └─► get_injecseq_relation() ─► maps to samples table

    Quantitation
       │
       └─► QuantRes object
            │
            ├─► linearity results ─► write_NONMEM() ─► numeric data
            └─► pkdata ─► with codebook for original IDs

------------------------------------------------------------------------

## Database Connection Pattern

    .connect_to_db() ──► DuckDB connection to samples.db
                         │
                         ├─► DBI::dbGetQuery() (READ operations)
                         ├─► DBI::dbAppendTable() (INSERT operations)
                         ├─► DBI::dbExecute() (CREATE/ALTER operations)
                         └─► DBI::dbBegin/Commit/Rollback (Transactions)
                             │
                             └─► .close_db() ──► disconnect & cleanup

------------------------------------------------------------------------

## Key Relationships Summary

| Parent Table | Child Table   | Type | Constraint         |
|--------------|---------------|------|--------------------|
| chromexpdb   | chroms        | 1:N  | exp_id (FK)        |
| methodstab   | chroms        | 1:N  | method_id (FK)     |
| methodstab   | transtab      | 1:N  | method_id (FK)     |
| transtab     | compoundstab  | 1:N  | transition_id (FK) |
| chroms       | peakstab      | 1:N  | chrom_id (FK)      |
| compoundstab | peakstab      | 1:N  | compound_id (FK)   |
| platesdb     | samples       | 1:N  | list_id (FK)       |
| study        | subject       | 1:N  | study_id (FK)      |
| study        | dosing        | 1:N  | study_id (FK)      |
| study        | sample_log    | 1:N  | study_id (FK)      |
| quant_meta   | quant_samples | 1:N  | quant_id (FK)      |
| compoundstab | quant_samples | 1:N  | compound_id (FK)   |

------------------------------------------------------------------------

## Soft References (Not Enforced in DB)

- `samples.study_id` ⟷ `study.id`
- `samples.log_id` ⟷ `sample_log.log_id`
- `quant_samples.log_id` ⟷ `sample_log.log_id`
- `sample_log.subject_id` ⟷ `subject.subject_id`

------------------------------------------------------------------------

## File System Storage

    PKbioanalysis_env$data_dir/
    ├── samples.db (main database)
    ├── plates_cache/ (RegisteredPlate RDS files)
    │   └── {plate_id}_{subid}.RDS
    └── [other data files]

------------------------------------------------------------------------

## Query Helpers

Key utility functions for database operations:

| Function                    | Purpose                            |
|-----------------------------|------------------------------------|
| `.connect_to_db()`          | Establish DuckDB connection        |
| `.close_db(db, gc)`         | Disconnect and cleanup             |
| `.check_sample_db()`        | Create tables if not exist         |
| `.get_samplesdb_metadata()` | Retrieve platesdb content          |
| `.get_samplelist(id)`       | Retrieve samples by list_id or all |
| `.reset_samples_db()`       | Archive and reset database         |
| `get_injecseq_relation()`   | Map study to injection sequences   |
| `retrieve_full_study_log()` | Retrieve complete study design     |
