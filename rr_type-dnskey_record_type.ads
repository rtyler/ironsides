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
use type Unsigned_Types.Unsigned16;
use type unsigned_types.unsigned8;
--#inherit rr_type, unsigned_types;
package Rr_Type.Dnskey_Record_Type is
   maxDNSKeyLength : constant natural := (1024*4)/3;  -- =1365, * 4/3 due to Base64 expansion
   subtype keyLengthValueType is natural range 0..maxDNSKeyLength;
   subtype DNSKeyStringTypeIndex is natural range 1..maxDNSKeyLength;
   subtype DNSKeyStringType is String(DNSKeyStringTypeIndex);
   type DNSKeyRecordType is new rr_type.ResourceRecordType with
   record
         flags: unsigned_types.Unsigned16;
         protocol: unsigned_types.Unsigned8;  --must be 3 for DNSSEC
         algorithm: unsigned_types.Unsigned8; --will be 5 for RSA/SHA1
         key : DNSKeyStringType;
         keyLength : keyLengthValueType;
   end record;

   MAX_16BIT_VAL : constant Long_Long_Integer := 2**16-1;
   MAX_8BIT_VAL : constant Long_Long_Integer := 2**8-1;

   --Ugh.  But what can you do?
   Spaces64 : constant String := Rr_Type.Spaces32 & Rr_Type.Spaces32;
   Spaces128 : constant String := Spaces64 & Spaces64;
   Spaces256 : constant String := Spaces128 & Spaces128;
   Spaces512 : constant String := Spaces256 & Spaces256;
   Spaces1024 : constant String := Spaces512 & Spaces512;
   --1365 spaces, see above
   blankKey : constant String := Spaces1024 & Spaces256 & Spaces64 & "                     ";
   --placeholder for empty slots in hash table
   blankDNSKeyRecord : constant DNSKeyRecordType := DNSKeyRecordType'(
      TtlInSeconds => 0,
      --Owner => "empty.DNSKey.resource.record     " & Rr_Type.Spaces32 & rr_type.Spaces32 & rr_type.Spaces32,
      class => rr_type.INTERNET,
      flags => 0,
      protocol => 0,
      algorithm => 0,
      key => blankKey,
      keyLength => 0
   );

--hash table (2d array) for DNSKEY records
type DNSKeyRecordBucketType is array(rr_type.ReturnedRecordsIndexType) of
     DNSKeyRecordType;
type DNSKeyRecordHashTableType is array(rr_type.NumBucketsIndexType) of
   DNSKeyRecordBucketType;

end rr_type.dnskey_record_type;
