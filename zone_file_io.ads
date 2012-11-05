----------------------------------------------------------------
-- IRONSIDES - DNS SERVER
--
-- By: Martin C. Carlisle and Barry S. Fagin
--     Department of Computer Science
--     United States Air Force Academy
--
-- This is free software; you can redistribute it and/or 
-- modify without restriction.  We do ask that you please keep
-- the original author information, and clearly indicate if the
-- software has been modified.
--
-- This software is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty
-- of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  
----------------------------------------------------------------

with Spark.Ada.Text_IO;

--# inherit dns_table_pkg, dns_types, error_msgs, parser_utilities, process_first_line_of_record,
--# rr_type, rr_type.a_record_type, rr_type.aaaa_record_type, rr_type.cname_record_type,
--# rr_type.dnskey_record_type, rr_type.mx_record_type, rr_type.ns_record_type, rr_type.nsec_record_type,
--# rr_type.ptr_record_type, Rr_type.rrsig_record_type, rr_type.SOA_record_type,
--# Spark.Ada.Text_IO, unsigned_types, zone_file_parser, ada.characters.handling;

package zone_file_io
is
   procedure processzoneFile(zoneFile : in out Spark.Ada.Text_IO.File_Type;
                             success : out boolean);
   --# global in out dns_table_pkg.DNS_Table;
   --# derives dns_table_pkg.DNS_Table, success from dns_table_pkg.DNS_Table, zoneFile
   --# & zoneFile from zoneFile, dns_table_pkg.DNS_Table;
   --# declare delay;
end zone_file_io;
