select origin_as, count(origin_as) as weeks, sum(30-rank) as rankweeks, sum(netgain) as ngweeks, sum(netsnow) as nnweeks
from email_cidr_reports
where origin_as >= 1
group by origin_as
order by origin_as;
