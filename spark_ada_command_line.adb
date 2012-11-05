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

-------------------------------------------------------------------------------
-- (C) Altran Praxis Limited
-------------------------------------------------------------------------------
--
-- The SPARK toolset is free software; you can redistribute it and/or modify it
-- under terms of the GNU General Public License as published by the Free
-- Software Foundation; either version 3, or (at your option) any later
-- version. The SPARK toolset is distributed in the hope that it will be
-- useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General
-- Public License for more details. You should have received a copy of the GNU
-- General Public License distributed with the SPARK toolset; see file
-- COPYING3. If not, go to http://www.gnu.org/licenses for a complete copy of
-- the license.
--
--=============================================================================

with Ada.Command_Line;
with Ada.Text_IO;
with Spark.Ada.Text_IO;
with Gnat.Os_Lib;
use type Spark.Ada.Text_IO.Exception_T;

package body SPARK_Ada_Command_Line is
   --# hide SPARK_Ada_Command_Line;

   function Argument_Count return Natural is
   begin
      return Standard.Ada.Command_Line.Argument_Count;
   end Argument_Count;


   procedure Create_File_From_Argument(Number : in Natural;
      File : out Spark.Ada.Text_IO.File_Type) is
   begin
      if Argument_Count /= 1 then
         Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
            Item => "Usage: " & Standard.Ada.Command_Line.Command_Name &
               " input_file");
         Gnat.Os_Lib.OS_Exit(1);
      end if;
      Spark.Ada.Text_IO.Open(File => File,
                             Mode => Spark.Ada.Text_IO.In_File,
                             Name => Standard.Ada.Command_Line.Argument(Number),
                             Form => "");
      if Spark.Ada.Text_IO.Get_Last_Exception_File(File) /=
            Spark.Ada.Text_IO.No_Exception then
         Ada.Text_IO.Put_Line(File => Ada.Text_IO.Standard_Error,
            Item => Standard.Ada.Command_Line.Argument(Number) & ": not valid file");
         Gnat.Os_Lib.OS_Exit(1);
      end if;
   end Create_File_From_Argument;



   procedure Exit_With_Status (Code : in Exit_Status) is
   begin
      Gnat.Os_Lib.Os_Exit(Integer(Code));
   end Exit_With_Status;

end SPARK_Ada_Command_Line;
