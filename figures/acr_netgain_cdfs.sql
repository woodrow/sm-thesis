select origin_as, min(netgain), max(netgain) from email_cidr_reports
where origin_as >= 1
group by origin_as
order by origin_as;