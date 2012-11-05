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

WITH Dns_Types, rr_type.dnskey_record_type, rr_type.rrsig_record_type, unsigned_types, rr_type;
WITH Spark.Ada.Text_IO;

use type Rr_Type.RrItemType;
use type dns_types.Query_Type;
USE TYPE Spark.Ada.Text_IO.Exception_T;
use type Unsigned_Types.Unsigned32;

--# inherit Dns_Types, dns_table_pkg, error_msgs, parser_utilities, rr_type.a_record_type,
--# rr_type.aaaa_record_type, rr_type.cname_record_type, rr_type.dnskey_record_type, rr_type.mx_record_type,
--# rr_type.ns_record_type, rr_type.nsec_record_type, rr_type.ptr_record_type, rr_type.rrsig_record_type,
--# spark.ada.text_io, unsigned_types, rr_type, zone_file_parser;

package process_first_line_of_record is
   procedure ProcessFirstLineOfRecord (CurrentRecordType : in Dns_Types.Query_Type;
         --common to all record types
         currentOrigin : in Rr_Type.DomainNameStringType;
         currentOwner : in Rr_Type.DomainNameStringType;
         currentTTL : in unsigned_types.Unsigned32;
         currentClass : in Rr_Type.ClassType;
         currentLine : in rr_type.LineFromFileType;
         Lastpos : in Rr_Type.Linelengthindex;
         LineCount : Unsigned_Types.Unsigned32;
         --for multiline records
         InMultilineRecord : out Boolean;
         LineInRecordCtr : out Unsigned_Types.Unsigned32;
         --SOA record fields
         currentNameServer : out rr_Type.DomainNameStringType;
         CurrentEmail : out Rr_Type.DomainNameStringType;
         --DNSKEY record (if needed)
         DNSKEY_Rec : out Rr_Type.Dnskey_Record_Type.DNSKeyRecordType;
         --RRSIG record (if needed)
         RRSIG_Rec : out Rr_Type.rrsig_record_type.RRSIGRecordType;
         recordSuccessfullyInserted : out Boolean;
         Success : in out boolean);
      --# global in out dns_table_pkg.DNS_Table;
      --# derives dns_table_pkg.DNS_Table from dns_table_pkg.DNS_Table, currentRecordType,
      --#   currentOrigin, currentOwner, currentTTL, CurrentClass, currentLine, lastPos, success
      --# & inMultilineRecord, lineInRecordCtr from currentRecordType
      --# & currentNameServer, currentEmail from currentOrigin, currentRecordType, currentLine, lastPos, success
      --# & DNSKEY_Rec from currentRecordType, currentLine, lastPos, success
      --# & RRSIG_Rec from currentRecordType, currentLine, lastPos, success
      --# & recordSuccessfullyInserted from dns_table_pkg.DNS_Table, currentRecordType,
      --#    currentOrigin, currentOwner, currentLine, lastPos, success
      --# & success from currentRecordType, currentOrigin, currentLine, lastPos, success
      --# & null from lineCount;
      --# pre lastPos >= 1 and lastPos <= rr_type.lineLengthIndex'last;
end process_first_line_of_record;
