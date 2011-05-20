select gcr.date, gcr.origin_as, gcr.nets_reduced, gcr.nets_current
from gen_cidr_reports as gcr
where extract(DOY from gcr.date) < 8
and gcr.id >= 12415379
and gcr.origin_as > 0
order by gcr.date, gcr.origin_as;