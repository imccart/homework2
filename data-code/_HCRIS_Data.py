# Meta --------------------------------------------------------------------
# Author:        Pablo Estrada
# Date Created:  2/03/2025
# Date Edited:   2/03/2025
# Notes:         Python file to build HCRIS data for Econ/HLTH 470 Hwk 2

import pandas as pd
import numpy as np


# Call individual scripts -------------------------------------------------
exec(open('data-code/H1_HCRISv1996.py').read())
exec(open('data-code/H2_HCRISv2010.py').read())



# Read and combine data ---------------------------------------------------
final_hcris_v1996 = pd.read_csv('data/output/HCRIS_v1996.csv')
final_hcris_v2010 = pd.read_csv('data/output/HCRIS_v2010.csv')

# Create missing variables for columns introduced in v2010
final_hcris_v1996['hvbp_payment'] = np.nan
final_hcris_v1996['hrrp_payment'] = np.nan

# Combine v1996 and v2010 datasets
final_hcris = pd.concat([final_hcris_v1996, final_hcris_v2010])

# Convert date columns to datetime format
for col in ['fy_end', 'fy_start', 'date_processed', 'date_created']:
    final_hcris[col] = pd.to_datetime(final_hcris[col], format='%m/%d/%Y')

# Convert to absolute values
final_hcris['tot_discounts'] = final_hcris['tot_discounts'].abs()
final_hcris['hrrp_payment'] = final_hcris['hrrp_payment'].abs()

# Extract fiscal year and sort
final_hcris['fyear'] = final_hcris['fy_end'].dt.year
final_hcris = final_hcris.sort_values(by=['provider_number', 'fyear']).drop(columns=['year'], errors='ignore')

# Count hospitals per year
hospital_counts = final_hcris.groupby('fyear').size()



# Clean data --------------------------------------------------------------

# Create count of reports by hospital fiscal year
final_hcris['total_reports'] = final_hcris.groupby(['provider_number', 'fyear'])['provider_number'].transform('count')
final_hcris['report_number'] = final_hcris.groupby(['provider_number', 'fyear']).cumcount() + 1

# Identify hospitals with one report per year
unique_hcris1 = final_hcris[final_hcris['total_reports'] == 1].drop(columns=['report', 'total_reports', 'report_number', 'npi', 'status'], errors='ignore')
unique_hcris1['source'] = 'unique reports'

# Identify hospitals with multiple reports per year
duplicate_hcris = final_hcris[final_hcris['total_reports'] > 1].copy()

# Calculate elapsed time between fy start and fy end for hospitals with multiple reports
duplicate_hcris['time_diff'] = (duplicate_hcris['fy_end'] - duplicate_hcris['fy_start']).dt.days
duplicate_hcris['total_days'] = duplicate_hcris.groupby(['provider_number', 'fyear'])['time_diff'].transform('sum')

# Hospitals where total days < 370, sum values
unique_hcris2 = duplicate_hcris[duplicate_hcris['total_days'] < 370].groupby(['provider_number', 'fyear']).agg({
    'beds': 'max', 'tot_charges': 'sum', 'tot_discounts': 'sum',
    'tot_operating_exp': 'sum', 'ip_charges': 'sum', 'icu_charges': 'sum',
    'ancillary_charges': 'sum', 'tot_discharges': 'sum', 'mcare_discharges': 'sum',
    'mcaid_discharges': 'sum', 'tot_mcare_payment': 'sum',
    'secondary_mcare_payment': 'sum', 'hvbp_payment': 'sum', 'hrrp_payment': 'sum',
    'fy_start': 'min', 'fy_end': 'max', 'date_processed': 'max', 'date_created': 'min',
    'street': 'first', 'city': 'first', 'state': 'first',
    'zip': 'first', 'county': 'first'
}).reset_index()
unique_hcris2['source'] = 'total for year'

# Hospitals with reports exceeding 370 days
duplicate_hcris2 = duplicate_hcris[duplicate_hcris['total_days'] >= 370].copy()
duplicate_hcris2['max_days'] = duplicate_hcris2.groupby(['provider_number', 'fyear'])['time_diff'].transform('max')
duplicate_hcris2['max_date'] = duplicate_hcris2.groupby(['provider_number', 'fyear'])['fy_end'].transform('max')

# Primary report selection
unique_hcris3 = duplicate_hcris2[(duplicate_hcris2['max_days'] == duplicate_hcris2['time_diff']) &
                                  (duplicate_hcris2['time_diff'] > 360) &
                                  (duplicate_hcris2['max_date'] == duplicate_hcris2['fy_end'])]
unique_hcris3 = unique_hcris3.drop(columns=['report', 'total_reports', 'report_number', 'npi', 'status', 'max_days', 'time_diff', 'total_days', 'max_date'], errors='ignore')
unique_hcris3['source'] = 'primary report'

# Remaining hospitals with reports covering more than one full year
duplicate_hcris3 = duplicate_hcris2[~duplicate_hcris2.index.isin(unique_hcris3.index)].copy()
duplicate_hcris3['time_diff'] = duplicate_hcris3['time_diff'].astype(int)
duplicate_hcris3['total_days'] = duplicate_hcris3['total_days'].astype(int)

cols_to_weight = ['tot_charges', 'tot_discounts', 'tot_operating_exp', 'ip_charges',
                  'icu_charges', 'ancillary_charges', 'tot_discharges', 'mcare_discharges',
                  'mcaid_discharges', 'tot_mcare_payment', 'secondary_mcare_payment',
                  'hvbp_payment', 'hrrp_payment']

duplicate_hcris3[cols_to_weight] = duplicate_hcris3[cols_to_weight].mul(
    duplicate_hcris3['time_diff'] / duplicate_hcris3['total_days'], axis=0)

# Weighted average
unique_hcris4 = duplicate_hcris3.groupby(['provider_number', 'fyear']).agg({
    'beds': 'max', 'tot_charges': 'sum', 'tot_discounts': 'sum',
    'tot_operating_exp': 'sum', 'ip_charges': 'sum', 'icu_charges': 'sum',
    'ancillary_charges': 'sum', 'tot_discharges': 'sum', 'mcare_discharges': 'sum',
    'mcaid_discharges': 'sum', 'tot_mcare_payment': 'sum',
    'secondary_mcare_payment': 'sum', 'hvbp_payment': 'sum', 'hrrp_payment': 'sum',
    'fy_start': 'min', 'fy_end': 'max', 'date_processed': 'max', 'date_created': 'min',
    'street': 'first', 'city': 'first', 'state': 'first',
    'zip': 'first', 'county': 'first'
}).reset_index()
unique_hcris4['source'] = 'weighted_average'

# Combine final datasets
final_hcris_data = pd.concat([unique_hcris1, unique_hcris2, unique_hcris3, unique_hcris4])
final_hcris_data = final_hcris_data.rename(columns={'fyear': 'year'}).sort_values(by=['provider_number', 'year'])



# Save final data ---------------------------------------------------------
final_hcris_data.to_csv('data/output/HCRIS_Data.csv', index=False)
