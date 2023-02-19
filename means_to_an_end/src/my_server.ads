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

with Ada.Containers.Ordered_Sets;

with Interfaces;          use Interfaces;
with GNAT.Sockets;        use GNAT.Sockets;
with GNAT.Sockets.Server; use GNAT.Sockets.Server;

--  For the protocol
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Integers;
with GNAT.Sockets.Connection_State_Machine.Big_Endian.Unsigneds;

package My_Server is
   Invalid_Connection : exception;
   use GNAT.Sockets.Connection_State_Machine;

   type Server_Factory is new Connections_Factory with private;
   type Server_Connection is new Connection with private;

   overriding function Create
     (Factory  : access Server_Factory;
      Listener : access Connections_Server'Class; From : Sock_Addr_Type)
      return Connection_Ptr;

   overriding procedure Finalize (Client : in out Server_Connection);

   overriding procedure Process_Packet (Client : in out Server_Connection);

   overriding procedure Sent (Client : in out Server_Connection);

private

   type Query_Entry is record
      Timestamp : Integer_32;
      Price     : Integer_32 := 0;
   end record;

   function "<" (Left, Right : Query_Entry) return Boolean;
   overriding function "=" (Left, Right : Query_Entry) return Boolean;
   --  Our set of queries.
   package Query_Entries is new Ada.Containers.Ordered_Sets (Query_Entry);

   type Server_Factory is new Connections_Factory with null record;
   type Server_Connection is new State_Machine with record
      From   : Sock_Addr_Type;
      Active : Boolean := True;

      --  Our ordered set.
      Query_List : Query_Entries.Set;

      --  Data items for the actual protocol
      Operation : Big_Endian.Unsigneds.Unsigned_8_Data_Item;
      Arg1      : Big_Endian.Integers.Integer_32_Data_Item;
      Arg2      : Big_Endian.Integers.Integer_32_Data_Item;
   end record;

end My_Server;
