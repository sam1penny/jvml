cloc lib bin test benchmarking scripts \
--exclude-ext=xml,csv --fullpath --not-match-d="scripts/stackmap_gen" --not-match-d="benchmarking/programs" \
--not-match-d="benchmarking/jmh/" --not-match-d="benchmarking/results" --not-match-f="benchmarking/jmh/pom.xml"