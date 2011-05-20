select ecr.date, ecr.origin_as, ecr.netgain, ecr.netsnow
from email_cidr_reports as ecr
where extract(DOY from ecr.date) < 8
and ecr.origin_as > 0
order by ecr.date, ecr.origin_as;