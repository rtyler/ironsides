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

PACKAGE Unsigned_Types IS
   type Unsigned8 is mod 2**8; --SPARK caught missing * here, impressive!
   type Unsigned16 is mod 2**16;
   type Unsigned32 is mod 2**32;
   MAX_8BIT_VAL : constant natural := 2**8-1;
   MAX_16BIT_VAL : constant Natural := 2**16-1;
   MAX_32BIT_VAL : constant long_long_integer := 2**32-1;
end unsigned_types;
