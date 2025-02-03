########################################################################################
## Author:        Pablo Estrada
## Date Created:  2/03/2025
## Date Edited:   2/03/2025
## Notes:         Python file to read in HCRIS data (1996 version of forms)
########################################################################################

import pandas as pd

# Define the list of variables and locations
hcris_vars = pd.DataFrame([
    ('beds', 'S300001', '01200', '0100', 'numeric'),
    ('tot_charges', 'G300000', '00100', '0100', 'numeric'),
    ('tot_discounts', 'G300000', '00200', '0100', 'numeric'),
    ('tot_operating_exp', 'G300000', '00400', '0100', 'numeric'),
    ('ip_charges', 'G200000', '00100', '0100', 'numeric'),
    ('icu_charges', 'G200000', '01500', '0100', 'numeric'),
    ('ancillary_charges', 'G200000', '01700', '0100', 'numeric'),
    ('tot_discharges', 'S300001', '00100', '1500', 'numeric'),
    ('mcare_discharges', 'S300001', '00100', '1300', 'numeric'),
    ('mcaid_discharges', 'S300001', '00100', '1400', 'numeric'),
    ('tot_mcare_payment', 'E00A18A', '01600', '0100', 'numeric'),
    ('secondary_mcare_payment', 'E00A18A', '01700', '0100', 'numeric'),
    ('street', 'S200000', '00100', '0100', 'alpha'),
    ('city', 'S200000', '00101', '0100', 'alpha'),
    ('state', 'S200000', '00101', '0200', 'alpha'),
    ('zip', 'S200000', '00101', '0300', 'alpha'),
    ('county', 'S200000', '00101', '0400', 'alpha')
], columns=['variable', 'WKSHT_CD', 'LINE_NUM', 'CLMN_NUM', 'source'])

# Pull relevant data: v1996 of HCRIS forms run through 2011 due to lags in processing and hospital fiscal years
final_hcris_v1996 = None

for year in range(1998, 2002):
    print('Processing year:', year)
    alpha_path = f"data/input/HCRIS_v1996/HospitalFY{year}/hosp_{year}_ALPHA.CSV"
    numeric_path = f"data/input/HCRIS_v1996/HospitalFY{year}/hosp_{year}_NMRC.CSV"
    report_path = f"data/input/HCRIS_v1996/HospitalFY{year}/hosp_{year}_RPT.CSV"

    HCRIS_alpha = pd.read_csv(alpha_path, names=['RPT_REC_NUM', 'WKSHT_CD', 'LINE_NUM', 'CLMN_NUM', 'ITM_VAL_NUM'])
    HCRIS_numeric = pd.read_csv(numeric_path, names=['RPT_REC_NUM', 'WKSHT_CD', 'LINE_NUM', 'CLMN_NUM', 'ITM_VAL_NUM'])
    HCRIS_report = pd.read_csv(report_path, names=['RPT_REC_NUM', 'PRVDR_CTRL_TYPE_CD', 'PRVDR_NUM', 'NPI',
                                                   'RPT_STUS_CD', 'FY_BGN_DT', 'FY_END_DT', 'PROC_DT',
                                                   'INITL_RPT_SW', 'LAST_RPT_SW', 'TRNSMTL_NUM', 'FI_NUM',
                                                   'ADR_VNDR_CD', 'FI_CREAT_DT', 'UTIL_CD', 'NPR_DT',
                                                   'SPEC_IND', 'FI_RCPT_DT'])
    
    final_reports = HCRIS_report[['RPT_REC_NUM', 'PRVDR_NUM', 'NPI', 'FY_BGN_DT', 'FY_END_DT', 'PROC_DT',
                                  'FI_CREAT_DT', 'RPT_STUS_CD']]
    final_reports.columns = ['report', 'provider_number', 'npi', 'fy_start', 'fy_end', 'date_processed',
                             'date_created', 'status']
    final_reports['year'] = year

    for _, row in hcris_vars.iterrows():
        hcris_data = HCRIS_numeric if row['source'] == 'numeric' else HCRIS_alpha
        val = hcris_data[(hcris_data['WKSHT_CD'] == row['WKSHT_CD']) &
                         (hcris_data['LINE_NUM'] == row['LINE_NUM']) &
                         (hcris_data['CLMN_NUM'] == row['CLMN_NUM'])][['RPT_REC_NUM', 'ITM_VAL_NUM']]
        val.columns = ['report', row['variable']]
        final_reports = final_reports.merge(val, on='report', how='left')
    
    if final_hcris_v1996 is None:
        final_hcris_v1996 = final_reports
    else:
        final_hcris_v1996 = pd.concat([final_hcris_v1996, final_reports], ignore_index=True)

# Save final dataset
final_hcris_v1996.to_csv('data/output/HCRIS_v1996.csv', index=False)
