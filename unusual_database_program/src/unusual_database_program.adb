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
with Ada.Text_IO;             use Ada.Text_IO;
with Ada.Streams;             use Ada.Streams;
with GNAT.Sockets;            use GNAT.Sockets;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Containers.Indefinite_Ordered_Maps;

procedure Unusual_Database_Program is

   --  Task to process the server
   task Process_Server;

   task body Process_Server is
      --  Convert stream element array to string
      function To_String
               (Data : Stream_Element_Array;
                Pointer : Stream_Element_Offset) return String
      is
         Result : constant String 
                  (Integer (Data'First) .. Integer (Pointer)) := 
                     [for I in Integer (Data'First) .. Integer (Pointer) =>
                        Character'Val (Data (Stream_Element_Offset (I))) ];
      begin
         return Result;
      end To_String;
      --  Convert string to stream element array
      function To_Stream_Element_Array (Data : String)
               return Stream_Element_Array
      is
         Result : constant Stream_Element_Array (1 .. Data'Length) :=
                            [for I of Data =>
                             Stream_Element'Val (Character'Pos (I))];
      begin
         return Result;
      end To_Stream_Element_Array;
      --  Server info
      Version : constant String := "AJ's Ada server 1.0";
      Port    : constant := 5876;
      --  Socket stuff
      Server  : Socket_Type;
      Address : Sock_Addr_Type;
      From    : Sock_Addr_Type;
      Data    : Stream_Element_Array (1 .. 1024);
      Last    : Stream_Element_Offset;

      --  The "database"
      package Database_Map is new
        Ada.Containers.Indefinite_Ordered_Maps
         (Key_Type        => String,
          Element_Type    => Unbounded_String);
      use Database_Map;

      DB : Map;

   begin
      --  Set version.
      DB.Insert ("version", To_Unbounded_String (Version));
      --  Create server socket
      Create_Socket (Server, Family_Inet, Socket_Datagram);
      Set_Socket_Option
         (Server, Socket_Level, (Reuse_Address, True));
      --  Set server address
      Address.Addr := Any_Inet_Addr;
      Address.Port := Port;
      --  Bind server to address
      Bind_Socket (Server, Address);
      --  Begin main loop.
      loop
         begin
            Put_Line ("Waiting for data....");
            Receive_Socket (Server, Data, Last, From);
            Put_Line (" Received the following from " & Image (From.Addr));
            declare
               Request : constant String := To_String (Data, Last);
               Equal   : constant Natural := Index (Request, "=");
            begin
               Put_Line (Request);
               --  Contains "=", This is a new entry
               if Equal > 0 then
                  declare
                     Key : constant String :=
                           (if Equal = Request'First then ""
                            else Request (Request'First .. Equal - 1));
                     Val : constant Unbounded_String :=
                           (if Equal = Request'Last then Null_Unbounded_String
                            else To_Unbounded_String
                                (Request (Equal + 1 .. Request'Last)));
                  begin
                     --  Do not overite "version"
                     if Key /= "version" then
                        --  Insert (or replace) the key
                        if DB.Contains (Key) then
                           DB (Key) := Val;
                        else
                           DB.Insert (Key, Val);
                        end if;
                     end if;
                  end;
               else
                  --  Request for lookup.
                  declare
                     Result : constant String := Request & "=" &
                                                (if DB.Contains (Request) then
                                                   To_String (DB (Request))
                                                else "");
                  begin
                     Send_Socket
                        (Server,
                         To_Stream_Element_Array (Result),
                         Last,
                         From);
                  end;
               end if;

            end;
         exception
            when Socket_Error =>
               Put_Line ("Received a socket error.");
            when others =>
               Put_Line ("Recevied another error.");
         end;
      end loop;

   end Process_Server;

   Shutting_Down : Boolean := False;
begin
   --  The exit loop.
   Put_Line ("Unusual DB server started.  Type ""exit"" to quit.");
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
   Put_Line ("DB server stopping");

end Unusual_Database_Program;
