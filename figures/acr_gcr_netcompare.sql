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
	sum(ecr.netgain) as acr_netsnow,
	sum(ecr.netsnow) as acr_netgain
	from email_cidr_reports as ecr
	where ecr.origin_as > 0
	group by ecr.date
	order by ecr.date) 
as acrsums
where acrsums.date = gcrsums.date
order by gcrsums.date;