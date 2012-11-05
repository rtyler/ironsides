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

with unsigned_types, dns_types;
use type Unsigned_Types.unsigned32;
use type Unsigned_Types.unsigned16;
use type unsigned_types.unsigned8;
--#inherit rr_type, unsigned_types, dns_types;
package Rr_Type.Rrsig_Record_Type is
   TimeStringLength : constant Natural := 14;   --YYYYMMDDHHmmSS
   MAX_YEAR : constant natural := 2020;   --seems reasonable to bound this is some way
   subtype TimeStringTypeIndex is Natural range 1..TimeStringLength;
   subtype TimeStringType is String(TimeStringTypeIndex);

   maxrrsigLength : constant natural := (1024*4)/3;  -- =1365, * 4/3 due to Base64 expansion
   subtype RRSIGStringTypeIndex is natural range 1..maxRRSIGLength;
   subtype RRSIGStringType is String(RRSIGStringTypeIndex);
   type RRSIGRecordType is new rr_type.ResourceRecordType with
      record
         TypeCovered : dns_types.Query_Type;
         algorithm : Unsigned_Types.Unsigned8; --will be 5 for RSA/SHA1
         numLabels : Unsigned_Types.Unsigned8;
         origTTL: Unsigned_Types.Unsigned32;
         sigExpiration: Unsigned_Types.Unsigned32;
         SigInception: Unsigned_Types.Unsigned32;
         keyTag : unsigned_types.Unsigned16;
         signerName : rr_type.DomainNameStringType;
         signature: RRSIGStringType;
         signatureLength : RRSIGStringTypeIndex;
   end record;

--placeholder for empty slots in hash table
   blankRRSIGRecord : constant RRSIGRecordType := RRSIGRecordType'(
      TtlInSeconds => 0,
      Class => Rr_Type.INTERNET,
      TypeCovered => dns_types.ERROR,
      Algorithm => 0,
      NumLabels => 1,
      OrigTTL => 0,
      SigExpiration => 0,
      SigInception => 0,
      KeyTag => 0,
      signerName => "empty.RRSIG.resource.record     " & Rr_Type.Spaces64 & rr_type.Spaces32,
      signature => Rr_Type.Spaces1024 & Rr_Type.Spaces256 & Rr_Type.Spaces64
         & "                     ", -- =1365 spaces
      signatureLength => 1
   );

--hash table (2d array) for RRSIG records
type RRSIGRecordBucketType is array(rr_type.ReturnedRecordsIndexType) of
     RRSIGRecordType;
type RRSIGRecordHashTableType is array(rr_type.NumBucketsIndexType) of
   RRSIGRecordBucketType;

end rr_type.rrsig_record_type;
