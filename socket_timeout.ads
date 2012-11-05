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

WITH Gnat.Sockets;

package Socket_Timeout is
   type Socket_Type is private;
   -- NOTE:  This is Windows specific.  On Linux, this would be rewritten for microseconds.
   procedure Set_Socket_Timeout(Socket : Socket_Type;
      Milliseconds : Natural);
   --# derives null from Socket, Milliseconds;
private
   --# hide Socket_Timeout;
   type Socket_Type is new Gnat.Sockets.Socket_Type;
end Socket_Timeout;
