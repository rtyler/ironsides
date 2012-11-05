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

--#inherit rr_type;
package Rr_Type.Nsec_Record_Type is
   type NSECRecordType is new rr_type.ResourceRecordType with
   record
      DomainName : Rr_Type.WireStringType;
      recordList : rr_type.LineFromFileType;
   end record;

--placeholder for empty slots in hash table
blankNSECRecord : constant NSECRecordType := NSECRecordType'(
   TtlInSeconds => 0,
   Class => Rr_Type.INTERNET,
   DomainName => "empty.NSEC.resource.record       " & rr_type.Spaces64 & rr_type.Spaces32,
   RecordList => rr_type.Spaces256);

--hash table (2d array) for NSEC records
type NSECRecordBucketType is array(rr_type.ReturnedRecordsIndexType) of NSECRecordType;
type NSECRecordHashTableType is array(rr_type.NumBucketsIndexType) of NSECRecordBucketType;

end rr_type.nsec_record_type;
