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

package Task_Limit is
   MAX_TASKS : constant Integer := 100;
   subtype Task_Count_Subtype is Integer range 0..MAX_TASKS;
   protected type Task_Count_Type is
      pragma Priority(0);
      procedure Increment(Success : out Boolean);
      --# global in out Task_Count_Type;
      --# derives Task_Count_Type from * &
      --#         Success from Task_Count_Type;
      procedure Decrement;
      --# global in out Task_Count_Type;
      --# derives Task_Count_Type from *;
   private
      Task_Count : Task_Count_Subtype := 0;
   end Task_Count_Type;
end Task_Limit;

