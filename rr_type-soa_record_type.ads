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

with Unsigned_Types;
use type Unsigned_Types.Unsigned32;
--#inherit rr_type, unsigned_types;
package Rr_Type.Soa_Record_Type is
   type SOARecordType is new Rr_Type.ResourceRecordType with
      record
         NameServer   : Rr_Type.WireStringType;
         Email        : Rr_Type.WireStringType;
         SerialNumber : Unsigned_Types.Unsigned32;
         Refresh      : Unsigned_Types.Unsigned32;
         Retry        : Unsigned_Types.Unsigned32;
         Expiry       : Unsigned_Types.Unsigned32;
         Minimum      : Unsigned_Types.Unsigned32;
      end record;

   MAX_SERIAL_VAL : constant Long_Long_Integer := 2**32-1;
   MAX_TIME_VAL : constant Long_Long_Integer := 2**32-1;

   --placeholder for empty slots in hash table
   BlanksoaRecord : constant SoaRecordType := SoaRecordType'(
      TtlInSeconds => 0,
      Class => Rr_Type.INTERNET,
      NameServer=> "empty.soa.resource.record        " & Rr_Type.Spaces32
      & Rr_Type.Spaces32 & Rr_Type.Spaces32,
      Email => "empty.soa.resource.record        " & Rr_Type.Spaces32
      & Rr_Type.Spaces32 & Rr_Type.Spaces32,
      SerialNumber => 0,
      Refresh => 0,
      Retry => 0,
      Expiry => 0,
      Minimum => 0
      );

   --hash table (2d array) for soa records
   type SoaRecordBucketType is array (Rr_Type.ReturnedRecordsIndexType) of SoaRecordType;
   type SoaRecordHashTableType is array (Rr_Type.NumBucketsIndexType) of SoaRecordBucketType;

end Rr_Type.Soa_Record_Type;
