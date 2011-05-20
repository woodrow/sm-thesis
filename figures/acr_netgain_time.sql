select ecr.date, tmp.rank0, ecr1.rank1, ecr2.rank2, ecr3.rank3, ecr4.rank4, ecr9.rank9, ecr.netgain as rank14, tmp.rank29
from email_cidr_reports as ecr,
	(select date, min(netgain) as rank29, max(netgain) as rank0
	from email_cidr_reports
	where origin_as >= 0
	group by date)
as tmp,
	(select date, netgain as rank1 from email_cidr_reports where origin_as >= 0 and rank = 1) as ecr1,
	(select date, netgain as rank2 from email_cidr_reports where origin_as >= 0 and rank = 2) as ecr2,
	(select date, netgain as rank3 from email_cidr_reports where origin_as >= 0 and rank = 3) as ecr3,
	(select date, netgain as rank4 from email_cidr_reports where origin_as >= 0 and rank = 4) as ecr4,
	(select date, netgain as rank9 from email_cidr_reports where origin_as >= 0 and rank = 9) as ecr9
where ecr.origin_as >= 0
and ecr.rank = 14
and ecr.date = tmp.date
and ecr.date = ecr1.date
and ecr.date = ecr2.date
and ecr.date = ecr3.date
and ecr.date = ecr4.date
and ecr.date = ecr9.date
order by ecr.date;