select gcrsums.date,
gcrsums.gcr_netsnow, gcrsums.gcr_netgain,
acrsums.acr_netsnow, acrsums.acr_netgain
from
	(select gcr.date,
	sum(gcr.nets_current) as gcr_netsnow,
	sum(gcr.nets_reduced) as gcr_netgain
	from gen_cidr_reports as gcr
	where gcr.origin_as > 0
	and gcr.id >= 12415379
	group by gcr.date
	order by gcr.date)
as gcrsums,
	(select ecr.date,
	sum(gcr.nets_current) as acr_netsnow,
	sum(gcr.nets_reduced) as acr_netgain
	from email_cidr_reports as ecr,
	gen_cidr_reports as gcr
	where ecr.origin_as > 0
	and gcr.origin_as > 0
	and gcr.id >= 12415379
	and gcr.date = ecr.date
	and ecr.origin_as = gcr.origin_as
	group by ecr.date
	order by ecr.date)
as acrsums
where acrsums.date = gcrsums.date
order by gcrsums.date;