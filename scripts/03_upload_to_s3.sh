bucket_name="ui-census-pulse-survey"
endweek_padded="09"

sudo apt install awscli
aws s3 cp data/intermediate-data/pulse_puf_all_weeks.csv s3://${bucket_name}/pulse_puf_all_weeks_9.csv
aws s3 cp data/raw-data/pulse_public_use_files/pulse2020_data.dictionary_CSV_${endweek_padded}.xlsx s3://${bucket_name}/puf_week_most_recent_data_dictionary_9.xlsx
aws s3 cp data/final-data/rolling_all_to_current_week.csv s3://${bucket_name}/rolling_all_current_week_9.csv
aws s3 cp data/intermediate-data/pulse_appended_columns_data_dictionary.csv s3://${bucket_name}/appended_column_data_dictionary_9.csv