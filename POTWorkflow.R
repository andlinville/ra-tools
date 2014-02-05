# ignore data import for now -- see bootProject.R to load sample Rdata set

# generate keys for POT
payerKey <- GenPayerKey(molten, 'Commercial Insurance')
hospKey <- GenHospKey(molten)
overlapKey <- GenOverlapKey(molten, overlap_ids, OverlapStandard)

# casting the discharge data
casted <- CastDischarge(molten, payerKey, overlapKey, .(commercial==1))

# generating POT parameters
grouped_ids <- list(c('patient_zip', 'zip_city', 'zip_county', 'zip_state'),
                    c('142'),
                    c('080'),
                    c('149', '013', '054', '064', '025', '088', '204', '049'))
other_ids <- GetOtherIds(names(casted), grouped_ids)
grouped_ids <- c(grouped_ids, other_ids)
hor_summary_key <- list('none',
                        'none',
                        'none',
                        list('allbut_3', 'total', sa_group),
                        list('allbut_5', 'full_total'))