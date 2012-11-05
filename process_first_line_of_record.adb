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

WITH Dns_Table_Pkg, zone_file_parser, error_msgs;
with parser_utilities, Rr_Type.A_Record_Type, Rr_Type.Aaaa_Record_Type, Rr_Type.Cname_Record_Type,
   Rr_Type.Mx_Record_Type, Rr_Type.Ns_Record_Type, Rr_Type.Nsec_Record_Type, Rr_Type.Ptr_Record_Type;

--just in case debugging needed
--WITH Ada.Text_IO, Ada.Integer_Text_IO;

package body process_first_line_of_record is
--had to encapsulate this in a separate procedure to help the examiner
--lots of parameters because of line-oriented file processing, different
--kinds of parameters and multiline records require large amount of state
--to persist across procedure calls
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
         Success : in out boolean)
      is
         currentIpv4 : Unsigned_Types.Unsigned32;
         currentIpv6 : rr_type.aaaa_record_type.IPV6AddrType;
         currentDomainName : rr_type.DomainNameStringType;
         CurrentPref : Unsigned_Types.Unsigned16;
         RRString : rr_type.LineFromFileType;
      begin
      	 --these assignments all make bogus flow errors go away
         inMultilineRecord := false;
         lineInRecordCtr := 0;
         currentNameServer := rr_type.BlankDomainName;
         CurrentEmail := Rr_Type.BlankDomainName;
         DNSKEY_Rec := Rr_Type.Dnskey_Record_Type.BlankDNSKeyRecord;
         RRSIG_Rec := rr_type.rrsig_record_type.blankRRSIGRecord;
         RecordSuccessfullyInserted := True;

         CASE CurrentRecordType IS
            WHEN Dns_Types.A => --A records
               --next data item must be an ipv4 addr
               Zone_File_Parser.ParseIpv4(currentIpv4, CurrentLine, LastPos, Success);

               --can now build and insert a complete A record
               if Success then
                  Dns_Table_Pkg.DNS_Table.InsertARecord(
                     Rr_Type.ConvertDomainNameToWire(CurrentOwner),
                     rr_type.a_record_type.ARecordType'(
                     TtlInSeconds => CurrentTTL, Class => CurrentClass,
                     Ipv4 => CurrentIpv4), RecordSuccessfullyInserted);
               end if;

            when Dns_Types.AAAA => --AAAA records
               --next item must be an ipv6 address
               Zone_File_Parser.ParseIpv6(currentIpv6, CurrentLine, LastPos, Success);

               --can now build and insert a complete AAAA record
               if Success then
                  dns_Table_Pkg.DNS_Table.InsertAAAARecord(Rr_Type.ConvertDomainNameToWire(CurrentOwner),
                     Rr_Type.Aaaa_Record_Type.AaaaRecordType'(
                  TtlInSeconds => CurrentTTL, Class => CurrentClass,
                  ipv6 => currentIpv6), recordSuccessfullyInserted);
               end if;

            when Dns_Types.CNAME => --CNAME records
               --next item must be a domain name
               Zone_File_Parser.ParseDomainName(currentDomainName, CurrentLine, LastPos, Success);

               if success then
                  --if domain name does not end in '.', append value of $ORIGIN
                  parser_utilities.CheckAndAppendOrigin(CurrentDomainName, CurrentOrigin, CurrentLine,
                     LastPos, LineCount, Success);
               end if;

               --can now build and insert a complete CNAME record
               if success then
                  dns_table_pkg.DNS_Table.insertCNAMERecord(Rr_Type.ConvertDomainNameToWire(CurrentOwner),
                     rr_type.cname_record_type.CNAMERecordType'(
                     ttlInSeconds => currentTTL , class => currentClass,
                     CanonicalDomainName  => Rr_Type.ConvertDomainNameToWire(CurrentDomainName)),
                     RecordSuccessfullyInserted);
               end if;

	        when dns_types.MX => --MX records
               --next must come a preference value, then a domain name (mail exchanger)
               zone_file_parser.parsePrefAndDomainName(currentPref, currentDomainName,
                  CurrentLine, LastPos, Success);
               if success then
                  --if domain name does not end in '.', append value of $ORIGIN
                  parser_utilities.CheckAndAppendOrigin(CurrentDomainName, CurrentOrigin, CurrentLine,
                     LastPos, LineCount, Success);
               end if;

               if success then
                  --can now build and insert a complete MX record
                  dns_table_pkg.DNS_Table.insertMXRecord(Rr_Type.ConvertDomainNameToWire(CurrentOwner),
                     rr_type.mx_record_type.MXRecordType'(
                     ttlInSeconds => currentTTL , class => currentClass,
                     Pref => CurrentPref,
                     MailExchanger => Rr_Type.ConvertDomainNameToWire(CurrentDomainName)),
                     RecordSuccessfullyInserted);
               end if;

             WHEN Dns_Types.NS => --NS records
                --next item must be a valid host name
                Zone_File_Parser.ParseDomainName(CurrentDomainName, CurrentLine, LastPos, Success);
                parser_utilities.checkValidHostName(currentDomainName, Success);
                if success then
                   --if domain name does not end in '.', append value of $ORIGIN
                   parser_utilities.CheckAndAppendOrigin(CurrentDomainName, CurrentOrigin, CurrentLine,
                      LastPos, LineCount, Success);
                end if;

                if success then
                   --can now build and insert a complete NS record
                   dns_table_pkg.DNS_Table.insertNSRecord(Rr_Type.ConvertDomainNameToWire(CurrentOwner),
                     rr_type.ns_record_type.NSRecordType'(
                      ttlInSeconds => currentTTL , class => currentClass,
                      NameServer   => Rr_Type.ConvertDomainNameToWire(CurrentDomainName)),
                      RecordSuccessfullyInserted);
                end if;

             when Dns_Types.PTR => --PTR records
                Zone_File_Parser.ParseDomainName(CurrentDomainName, CurrentLine, LastPos, Success);
                if success then
                   --if domain name does not end in '.', append value of $ORIGIN
                   parser_utilities.CheckAndAppendOrigin(CurrentDomainName, CurrentOrigin, CurrentLine,
                      lastPos, lineCount, Success);
                end if;

                if success then
                   --can now build and insert a complete PTR record
                   dns_table_pkg.DNS_Table.insertPTRRecord(Rr_Type.ConvertDomainNameToWire(CurrentOwner),
                     rr_type.ptr_record_type.PTRRecordType'(
                      ttlInSeconds => currentTTL , class => currentClass,
                      DomainName   => Rr_Type.ConvertDomainNameToWire(CurrentDomainName)),
                     RecordSuccessfullyInserted);
                end if;

            when Dns_Types.SOA => --SOA records
               InMultilineRecord := True;
               lineInRecordCtr := 0;
               Zone_File_Parser.ParseNameServerAndEmail(CurrentNameServer, CurrentEmail,
                  CurrentLine, LastPos, Success);
               --complete SOA record is inserted later, after all the other fields parsed

               --if name server or email do not end in '.', append value of $ORIGIN
               if success then
                  parser_utilities.CheckAndAppendOrigin(CurrentNameServer, CurrentOrigin, CurrentLine,
                     LastPos, LineCount, Success);
               end if;

               --if name server or email do not end in '.', append value of $ORIGIN
               if success then
                  parser_utilities.CheckAndAppendOrigin(CurrentEmail, CurrentOrigin, CurrentLine,
                     LastPos, LineCount, Success);
               end if;

               parser_utilities.CheckValidHostName(CurrentNameServer, Success);
               --NOTE:  CurrentEmail should eventually be checked too, but under slightly
               --different rules, see pg 77 of 4th edition O'Reilly BIND book

            --DNSSEC records below
            when Dns_Types.DNSKEY => --DNSKEY records
               InMultilineRecord := True;
               lineInRecordCtr := 0;
               Zone_File_Parser.ParseDNSKeyHeader(DNSKEY_Rec, CurrentLine, LastPos, Success);
               --complete DNSKEY record will be inserted later, after key field parsed

            when Dns_Types.NSEC =>
               Zone_File_Parser.ParseDomainNameAndRRString(
                  CurrentDomainName, RRString, CurrentLine, LastPos, Success);
               parser_utilities.checkValidHostName(currentDomainName, Success);
               if success then
                  --if domain name does not end in '.', append value of $ORIGIN
                  parser_utilities.CheckAndAppendOrigin(CurrentDomainName, CurrentOrigin, CurrentLine,
                     LastPos, LineCount, Success);
               end if;
               if success then
                   --can now build and insert a complete NSEC record
                   dns_table_pkg.DNS_Table.insertNSECRecord(Rr_Type.ConvertDomainNameToWire(CurrentOwner),
                      rr_type.nsec_record_type.NSECRecordType'(
                      ttlInSeconds => currentTTL, class => currentClass, recordList => RRString,
                      DomainName => Rr_Type.ConvertDomainNameToWire(CurrentDomainName)),
                      RecordSuccessfullyInserted);
               end if;

            when Dns_Types.RRSIG =>
               InMultilineRecord := True;
               LineInRecordCtr := 0;
               Zone_File_Parser.ParseRRSigHeader(RRSIG_Rec, CurrentLine, LastPos, Success);

            when others => -- can add more supported types here if needed
               error_msgs.printUnsupportedRecordWarning(currentLine, lastPos, lineCount);
          END CASE;
      end ProcessFirstLineOfRecord;
end process_first_line_of_record;
