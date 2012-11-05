rm -R dns_network_receive
rm -R dns_table_pkg
rm -R dns_types
rm -R parser_utilities
rm -R process_dns_request
rm -R tcp_dns_package
rm -R udp_dns_package
rm -R zone_file_io
rm -R zone_file_parser
rm -R rr_type
spark @files.txt
sparksimp -p 2
pogs -i