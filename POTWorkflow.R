# ignore data import for now -- see bootProject.R to load sample Rdata set

# generate keys for POT
payerKey <- GenPayerKey(molten, 'Commercial Insurance')
hospKey <- GenHospKey(molten)
grouped_ids <- list(c('149', '013', '054', '064', '025', '088', '204', '049'), c('142'), c('080'))
hor_summary_key <- list()
overlapKey <- GenOverlapKey(molten, overlap_ids, OverlapStandard)

# casting the discharge data
casted <- CastDischarge(molten, payerKey, overlapKey, .(commercial==1))