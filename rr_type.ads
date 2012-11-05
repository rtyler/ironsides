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
--# inherit Unsigned_Types;
PACKAGE Rr_Type IS
   pragma Elaborate_Body(rr_type);
   type classType is (INTERNET, CS, CH, HS); --INTERNET because IN is a reserved word
   --the things that can appear in a resource record
   type RrItemType is (DomainNameOrTimeSpec, Number, Class, RecordIndicator, Ipv4, Ipv6,
      LParen, RParen, Control, Comment, Other);
   --maybe this should be in a separate type package?
   MaxLineLength : constant Natural := 255+1;  --256 is what we tell the user
   subtype LineLengthIndex is Integer range 1..MaxLineLength;
   subtype LineFromFileType is String(LineLengthIndex);
   BlankLine : constant LineFromFileType := LineFromFileType'(others => ' ');

   MaxDomainNameLength: constant Integer := 128;
   subtype DomainNameStringTypeIndex is
     Integer range 1.. MaxDomainNameLength;
   subtype DomainNameStringType is String(DomainNameStringTypeIndex);
   BlankDomainName : constant DomainNameStringType := DomainNameStringType'(others => ' ');
   --wire version of a domain name is one character longer
   subtype WireStringTypeIndex is Integer range 1.. MaxDomainNameLength+1;
   subtype WireStringType is String(WireStringTypeIndex);
   BlankWire : constant WireStringType := WireStringType'(others => ' ');
   Spaces32 : constant String := "                                ";

   --these constants are needed in child packages
   MAX_32BIT_VAL : constant Long_Long_Integer := 2**32-1;
   MAX_16BIT_VAL : constant Long_Long_Integer := 2**16-1;
   MAX_8BIT_VAL : constant Long_Long_Integer := 2**8-1;
   --Ugh.  But what can you do?
   Spaces64 : constant String := Spaces32 & Spaces32;
   Spaces128 : constant String := Spaces64 & Spaces64;
   Spaces256 : constant String := Spaces128 & Spaces128;
   Spaces512 : constant String := Spaces256 & Spaces256;
   Spaces1024 : constant String := Spaces512 & Spaces512;

   procedure AppendDomainNames(
      Left : in out DomainNameStringType;
      Right: in DomainNameStringType;
      Success: in out Boolean);
   --# derives Left from Left, Right, Success & Success from Left,Right,Success;

   function WireNameLength(Name : WireStringType) return WireStringTypeIndex;
   --# return Length => Length=MaxDomainNameLength+1 or (Name(Length)=Character'Val(0) and
   --#    (for all Q in DomainNameStringTypeIndex range 1..Length-1 => (Name(Q)/=Character'Val(0))));

   function DomainNameLength(Name : DomainNameStringType) return DomainNameStringTypeIndex;
   --# return Length => (Length=1 and (Name(1)=' ' or Name(2)=' ')) or
   --#    Length=MaxDomainNameLength or (Name(Length+1)=' ' and
   --#    (for all Q in DomainNameStringTypeIndex range 1..Length => (Name(Q)/= ' ')));

   function ConvertDomainNameToWire(
      DomainNameVersion: in DomainNameStringType) return WireStringType;
   function ConvertStringToDomainName(S: in String) return DomainNameStringType;
   --# pre S'length <= MaxDomainNameLength;
   function ConvertStringToWire(S: in String) return WireStringType;
   --# pre S'length <= MaxDomainNameLength;

   -- top level record, fields are common to all subrecords
   type ResourceRecordType is tagged
      record
         ttlInSeconds : unsigned_types.Unsigned32;
         class : ClassType;
      end record;
   BlankOwner : constant WireStringType := ASCII.NUL & "mpty.A.resource.record          " &
      Spaces32 & Spaces32 & Spaces32;

   --maximum bucket size in hash table (num cols in 2d array),
   --also max num records returned from query
   MaxNumRecords : constant Integer := 64;
   subtype ReturnedRecordsIndexType is integer range 1..MaxNumRecords;
   subtype NumberOfRecordsType is integer range 0..MaxNumRecords;

   --number of buckets in hash table (num rows in 2d array)
   NumBuckets : constant Integer := 64;
   subtype NumBucketsIndexType is integer range 1..NumBuckets;

   type OwnerRecordBucketType is array (ReturnedRecordsIndexType) of WireStringType;
   type OwnerHashTableType is array(NumBucketsIndexType) of OwnerRecordBucketType;

end rr_type;
