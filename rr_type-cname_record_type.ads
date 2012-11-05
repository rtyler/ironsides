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
package Rr_Type.Cname_Record_Type is
   type CNAMERecordType is new Rr_Type.ResourceRecordType with
      record
         CanonicalDomainName : Rr_Type.WireStringType;
      end record;

   --placeholder for empty slots in hash table
   BlankCNAMERecord : constant CNAMERecordType := CNAMERecordType'(
      TtlInSeconds => 0,
      Class => Rr_Type.INTERNET,
      CanonicalDomainName => "empty.CNAME.resource.record      "
      & Rr_Type.Spaces32 & Rr_Type.Spaces32 & Rr_Type.Spaces32);

   --hash table (2d array) for CNAME records
   type CNAMERecordBucketType is array (Rr_Type.ReturnedRecordsIndexType) of CNAMERecordType;
   type CNAMERecordHashTableType is array (Rr_Type.NumBucketsIndexType) of CNAMERecordBucketType;

end Rr_Type.Cname_Record_Type;
