cat /proc/(pgrep fish)/smaps | grep fish_history -A 4 | grep Rss | awk '{s+=$2} END {print s " KB"}'
