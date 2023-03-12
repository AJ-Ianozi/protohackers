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

with GNAT.Sockets;          use GNAT.Sockets;
with GNAT.Sockets.Server;   use GNAT.Sockets.Server;
with Ada.Strings.Maps;      use Ada.Strings.Maps;

with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;
use GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

package My_Server is
   use GNAT.Sockets.Connection_State_Machine;

   --  Constants and character set for string processing later
   LF         : constant String        := "" & Character'Val (10);
   CR         : constant String        := "" & Character'Val (13);
   Trim_Set   : constant Character_Set := To_Set (Sequence => " " & LF & CR);

   --  Proxy Settings
   Budget_Port   : constant Port_Type := 16963;
   Budget_Server : constant String  := "chat.protohackers.com";

   Buffer_Size : constant := 8_192;

   --  Tony's evil idea
   Tonys_Address : constant String := "7YWHMfk9JZe0LM0g1ZauHuiSxhI";
   function Inject_String (Source : String) return String;

   --  Budget Chat server stuff
   task type Budget_Connection is
      entry Init (Belongs_To : Connection_Ptr);
      entry Connect;
      entry Send_Data (Data : String);
      entry Shutdown;
   end Budget_Connection;
   type Budget_Ptr is access Budget_Connection;

   Invalid_Connection, Invalid_Username : exception;

   --  Server connection and factory.
   type Server_Factory is new Connections_Factory with private;
   type Server_Connection is new Connection with private;

   overriding function Create
     (Factory  : access Server_Factory;
      Listener : access Connections_Server'Class; From : Sock_Addr_Type)
      return Connection_Ptr;
   overriding procedure Finalize (Client : in out Server_Connection);
   overriding procedure Activated (Client : in out Server_Connection);
   overriding procedure Process_Packet (Client : in out Server_Connection);
   overriding procedure Sent (Client : in out Server_Connection);

private

   type Server_Factory is new Connections_Factory with null record;

   type Server_Connection is new State_Machine with record
      From   : Sock_Addr_Type;
      Active : Boolean := True;

      --  Budget chat specific stuff
      Proxy : Budget_Ptr;

      --  Message is at max, 8kb characters long. Terminated by chr10 (\n)
      Data : Terminated_Strings.String_Data_Item
               (Buffer_Size, Character'Val (10));
   end record;

end My_Server;
