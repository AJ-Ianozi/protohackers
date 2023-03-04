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
with My_Server;               use My_Server;

procedure Budget_Chat is
   Port          : constant := 5876;
   Shutting_Down : Boolean  := False;
begin
   declare
      Factory : aliased Server_Factory;
      Server  : Connections_Server (Factory'Access, Port);
   begin
      declare
--         This was part of my "use a common queue" idea but I scrapped it.
--          Leaving here in case I want to reference the code later.
--         task Process_Chat;

--         task body Process_Chat is
--            My_Message : Message;
--         begin
--            loop
--               --  Grab the next message.
--               Messages.Dequeue (My_Message);
--               declare
--                  Msg : constant String :=
--                           (case My_Message.Msg_type is
--                              when Chat =>
--                                 ("[" & To_String(My_Message.From) & "] "
--                                 & To_String(My_Message.Msg)),
--                              when Entered =>
--                                ("* " & To_String(My_Message.From) &
--                                  " has entered the room"),
--                              when Left =>
--                                 ("* " & To_String(My_Message.From) &
--                                 " has left the room")
--                           );
--               begin
--                  User_List.Send_Msg(Msg);
--               end;
--            end loop;
--         end Process_Chat;
      begin
         --  The exit loop.
         Put_Line ("Chat server started.  Type ""exit"" to quit.");
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
         Put_Line ("Chat server stopping");
      end;
   end;
exception
   when Error : others =>
      Put_Line ("Error: " & Exception_Information (Error));
end Budget_Chat;
