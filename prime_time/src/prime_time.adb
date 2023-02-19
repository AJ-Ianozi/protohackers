--    Copyright (C) 2023 A.J. Ianozi <aj@ianozi.com>
--
--    This file is part of AJ's Protohacker competition submission
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <https://www.gnu.org/licenses/>.

with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Sockets.Server;     use GNAT.Sockets.Server;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Prime_Server;            use Prime_Server;
with Prime_Checker;           use Prime_Checker;
procedure Prime_Time is
   Port          : constant := 5_876;
   Shutting_Down : Boolean  := False;
begin
   Init_Primes;
   declare
      Factory : aliased Prime_Factory;
      Server  : Connections_Server (Factory'Access, Port);
   begin
      Put_Line ("Prime server started.  Type ""exit"" to quit.");
      while not Shutting_Down loop
         declare
            Command : constant String := Get_Line;
         begin
            if To_Lower (Command) = "exit" then
               Shutting_Down := True;
            else
               Put_Line ("Unkown command: " & Command);
            end if;
         end;
      end loop;
      Put_Line ("Echo server stopping");
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Prime_Time;
