with Ada.Calendar;
package body Non_Spark_Stuff is
   --# hide non_spark_stuff
   --SPARK doesn't support Ada.Calendar
   EPOCH_START : constant Ada.Calendar.Time := Ada.Calendar.Time_Of(1970,1,1,0.0);
   --THIS MUST BE CALLED WITH ALL PARAMETERS HAVING VALID VALUES OR IT WILL THROW
   --AN EXCEPTION
   function Time_Of(Year, Month, Day, Hour, Minute, Second : Natural) return Unsigned_Types.Unsigned32
         is
      use type Ada.Calendar.Time;
   begin
      return Unsigned_Types.Unsigned32(Ada.Calendar.Time_Of(Year,Month,Day,
         Duration(3600*Hour+60*Minute+Second)) - EPOCH_START);
   end Time_Of;
end Non_Spark_Stuff;

