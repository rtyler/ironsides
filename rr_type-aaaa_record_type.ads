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

with Unsigned_Types, Rr_Type;
--# inherit Unsigned_Types, rr_type;
package RR_Type.Aaaa_Record_Type is

   MaxIPV6AddrLength: constant Integer := 8; --8 32-bit integers
   subtype IPV6AddrTypeIndex is Integer range 1.. MaxIPV6AddrLength;
   type IPV6AddrType is array (IPV6AddrTypeIndex) of Unsigned_Types.Unsigned16;
   INVALID_IPV6_ADDR : constant IPV6AddrType := IPV6AddrType'(others => 0);

   type AAAARecordType is new Rr_Type.ResourceRecordType with
      record
         Ipv6 : IPV6AddrType;
      end record;

   --placeholder for empty slots in hash table
   BlankAAAARecord : constant AAAARecordType := AAAARecordType'(
      TtlInSeconds => 0,
      Class => Rr_Type.INTERNET, Ipv6 => INVALID_IPV6_ADDR);

   --hash table (2d array) for AAAA records
   type AAAARecordBucketType is array (Rr_Type.ReturnedRecordsIndexType) of AAAARecordType;
   type AAAARecordHashTableType is array (Rr_Type.NumBucketsIndexType) of AAAARecordBucketType;

end Rr_Type.Aaaa_Record_Type;
