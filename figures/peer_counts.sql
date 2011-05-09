SELECT date, peer_id, sum(prefix_count)
FROM peer_table_summaries 
GROUP BY peer_id, date 
ORDER BY date, peer_id;