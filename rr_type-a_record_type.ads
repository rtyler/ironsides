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
package Rr_Type.A_Record_Type is
   type ARecordType is new Rr_Type.ResourceRecordType with
      record
         Ipv4 : Unsigned_Types.Unsigned32;
      end record;

   INVALID_IPV4_ADDR : constant Unsigned_Types.Unsigned32 := 0;

   --placeholder for empty slots in hash table
   BlankARecord : constant ARecordType := ARecordType'(
      TtlInSeconds => 0,
      Class => Rr_Type.INTERNET, Ipv4 => INVALID_IPV4_ADDR);

   --hash table (2d array) for A records
   type ARecordBucketType is array (Rr_Type.ReturnedRecordsIndexType) of ARecordType;
   type ARecordHashTableType is array (Rr_Type.NumBucketsIndexType) of ARecordBucketType;

end Rr_Type.A_Record_Type;
