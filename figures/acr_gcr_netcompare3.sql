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
	(select gcr.date,
	sum(gcr.nets_current) as acr_netsnow,
	sum(gcr.nets_reduced) as acr_netgain
	from gen_cidr_reports as gcr
	where gcr.origin_as > 0
	and gcr.id >= 12415379
	and gcr.rank_netgain < 30
	group by gcr.date
	order by gcr.date)
as acrsums
where acrsums.date = gcrsums.date
order by gcrsums.date;