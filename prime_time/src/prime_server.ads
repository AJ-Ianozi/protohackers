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

with Ada.Exceptions;       use Ada.Exceptions;
with Ada.Streams;          use Ada.Streams;
with GNAT.Sockets;         use GNAT.Sockets;
with GNAT.Sockets.Server;  use GNAT.Sockets.Server;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package Prime_Server is

   type Prime_Factory is new Connections_Factory with private;
   function Create
            (  Factory  : access Prime_Factory;
               Listener : access Connections_Server'Class;
               From     : Sock_Addr_Type
            )  return Connection_Ptr;
   procedure Trace
             (  Factory    : in out Prime_Factory;
                Context    : String;
                Occurrence : Exception_Occurrence
             );

   type Prime_Connection is new Connection with private;
   procedure Finalize (Client : in out Prime_Connection);
   procedure Received
             (  Client  : in out Prime_Connection;
                Data    : Stream_Element_Array;
                Pointer : in out Stream_Element_Offset
             );

   procedure Sent (Client : in out Prime_Connection);

private
   type Prime_Factory is new Connections_Factory with null record;

   type Prime_Connection is new Connection with record
      From : Sock_Addr_Type;
      Active : Boolean := True;
      Json_Str : Unbounded_String := Null_Unbounded_String;
   end record;

end Prime_Server;