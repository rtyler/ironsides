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

WITH Zone_File_Io, Dns_Table_Pkg, Rr_Type.A_Record_Type, Rr_Type.AAAA_Record_Type, Rr_Type.Cname_Record_Type,
   Rr_Type.Dnskey_Record_Type, Rr_Type.MX_Record_Type, Rr_Type.NS_Record_Type, Rr_Type.NSEC_Record_Type,
   Rr_Type.PTR_Record_Type, Rr_Type.RRSIG_Record_Type, Rr_Type.SOA_Record_Type, Unsigned_Types;
WITH Ada.Text_IO, Spark.Ada.Text_IO;
USE TYPE Spark.Ada.Text_IO.Exception_T;

procedure Driver is
   pragma Priority(0);
   returnedARecords : Rr_Type.A_Record_Type.ARecordBucketType;
   returnedAAAARecords : Rr_Type.AAAA_Record_Type.AAAARecordBucketType;
   ReturnedCNAMERecords : Rr_Type.CNAME_Record_Type.CNAMERecordBucketType;
   returnedDNSKEYRecords : Rr_Type.DNSKEY_Record_Type.DNSKEYRecordBucketType;
   returnedMXRecords : Rr_Type.MX_Record_Type.MXRecordBucketType;
   ReturnedNSRecords : Rr_Type.NS_Record_Type.NSRecordBucketType;
   returnedNSECRecords : Rr_Type.NSEC_Record_Type.NSECRecordBucketType;
   ReturnedPTRRecords : Rr_Type.PTR_Record_Type.PTRRecordBucketType;
   returnedRRSIGRecords : Rr_Type.RRSIG_Record_Type.RRSIGRecordBucketType;
   returnedSOARecords : Rr_Type.SOA_Record_Type.SOARecordBucketType;
   NumFound : Natural;
   --ZoneFileName : constant String := "tmp.zonefile";
   ZoneFileName : constant String := "dfcs.usafa.edu.zonefile";
   --ZoneFileName : constant String := "db.dfcs.usafa.edu.signed";
   --ZoneFileName : constant String := "DFAN/usafa.hpc.mil.zonefile";
   --ZoneFileName : constant String := "DFAN/wedge.hpc.mil.zonefile";
   --ZoneFileName : constant String := "DFAN/wedge.iita.zonefile";
   --ZoneFileName : constant String := "DFAN/usafa.aero.zonefile";
   --ZoneFileName : constant String := "DFAN/research.usafa.edu.zonefile";
   --ZoneFileName : constant String := "DFAN/msrc.usafa.hpc.mil.zonefile";
   --ZoneFileName : constant String := "DFAN/edu.usafa.hpc.mil.zonefile";
   --ZoneFileName : constant String := "DFAN/10.in-addr.arpa.zonefile";
   --ZoneFileName : constant String := "DFAN/130.32.140.in-addr.arpa.zonefile";
   --ZoneFileName : constant String := "DFAN/castle.zonefile";
   --ZoneFileName : constant String := "DFAN/alabnet.zonefile";
   --zoneFileName : constant String := "DFAN/ipv6.usafa.edu.zonefile";
   --zoneFileName : constant String := "DFAN/alabnet.zonefile";

   function openzoneFile(fileName : in String) return boolean
   is
      ZoneFile : Spark.Ada.Text_IO.File_Type;
      success : boolean := true;
   begin
      Spark.Ada.Text_IO.Open(File => ZoneFile, Mode => Spark.Ada.Text_IO.In_File,
         Name => FileName, Form => "");
      if (Spark.Ada.Text_IO.Get_Last_Exception_File(zoneFile) /= Spark.Ada.Text_IO.No_Exception) then
         Ada.Text_IO.Put("Unable to open zone file " & ZoneFileName);
         Ada.Text_IO.New_Line;
         Ada.Text_IO.Put("Check spelling, path, permissions etc and retry.");
         Ada.Text_IO.New_Line;
         return false;
      else
         Zone_File_Io.ProcesszoneFile(ZoneFile, Success);
         return success;
      end if;
   end OpenzoneFile;

begin
   if OpenZoneFile(ZoneFileName) then
      Dns_Table_Pkg.DNS_Table.QueryARecords(
         Rr_Type.ConvertStringToWire("boleng.dfcs.usafa.edu."), ReturnedARecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " A records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("A record found, ipv4 = ");
         Ada.Text_IO.put(unsigned_types.Unsigned32'image(returnedARecords(i).ipv4));
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryAAAARecords(
         rr_type.convertStringToWire("iPV6.dfcs.usafa.EDu."),ReturnedAAAARecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " AAAA records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("AAAA record found, ipv6(1) = ");
         Ada.Text_IO.put(unsigned_types.Unsigned16'image(returnedAAAARecords(i).ipv6(1)));
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryCNAMERecords(
         rr_type.convertStringToWire("DOc.dfcs.usafa.edu."), ReturnedCNAMERecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " CNAME records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("CNAME record found, canonical domain name = ");
         Ada.Text_IO.put(returnedCNAMERecords(i).canonicalDomainName);
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryDNSKEYRecords(
         rr_type.convertStringToWire("dfcs.usafa.edu."), ReturnedDNSKEYRecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " DNSKEY records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("DNSKEY record found, key = ");
         Ada.Text_IO.put(returnedDNSKEYRecords(i).key);
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryMXRecords(
         rr_type.convertStringToWire("gibson.dfcs.USAFA.edu."), ReturnedMXRecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " MX records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("MX record found, pref/mailExchanger = ");
         Ada.text_io.put(unsigned_types.Unsigned16'image(ReturnedMXRecords(I).pref));
         Ada.Text_IO.put("   ");
         Ada.Text_IO.Put(ReturnedMXRecords(I).MailExchanger);
         ada.Text_IO.new_line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryNSECRecords(
         rr_type.convertStringToWire("dfcs.usafa.edu."), ReturnedNSECRecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " NSEC records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("NSEC record found, resource string = ");
         Ada.Text_IO.put(returnedNSECRecords(i).RecordList);
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryNSRecords(
         rr_type.convertStringToWire("dfcS.Usafa.edu."), ReturnedNSRecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " NS records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("NS record found, nameserver = ");
         Ada.Text_IO.put(returnedNSRecords(i).nameserver);
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryPTRRecords(
         rr_type.ConvertStringToWire("15.9.236.128.IN-addr.arpa."), ReturnedPTRRecords, NumFound);
      --Dns_Table_Pkg.DNS_Table.QueryPTRRecords(
         --Rr_Type.ConvertStringToWire("1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.1.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.IP6.ARPA."),
            --ReturnedPTRRecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " PTR records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("PTR record found, domain name = ");
         Ada.Text_IO.put(returnedPTRRecords(i).domainname);
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QueryRRSIGRecords(
         rr_type.convertStringToWire("dfcs.usafa.edu."), ReturnedRRSIGRecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " RRSIG records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("RRSIG record found, signature = ");
         Ada.Text_IO.put(returnedRRSIGRecords(i).signature);
         Ada.Text_IO.New_Line;
      END LOOP;

      Dns_Table_Pkg.DNS_Table.QuerySOARecords(
         rr_type.ConvertStringToWire("DFCS.usafa.EDU."), ReturnedSOARecords, NumFound);
      Ada.Text_IO.Put_Line( Integer'Image(NumFound) & " SOA records returned");
      for i in integer range 1..numFound loop
         Ada.Text_IO.Put("SOA record found, nameserver = ");
         Ada.Text_IO.put(returnedSOARecords(i).nameserver);
         Ada.Text_IO.New_Line;
      END LOOP;
   end if;
end driver;
