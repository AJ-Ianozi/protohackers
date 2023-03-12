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
with Ada.Strings;             use Ada.Strings;
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Strings.Unbounded;   use Ada.Strings.Unbounded;
with Ada.Streams;             use Ada.Streams;
with Ada.Characters.Handling; use Ada.Characters.Handling;

--  For non-blocking sockets
with Ada.Containers; use Ada.Containers;
with Ada.Containers.Synchronized_Queue_Interfaces;
with Ada.Containers.Unbounded_Synchronized_Queues;

package body My_Server is

   function Inject_String (Source : String) return String is
      --  Tries to guess if it contains a boguscoin address
      function Contains_Boguscoin (Source : String) return Boolean is
      begin
         if Source'Length >= 26 and then
            (Source (Source'First) = '7' or else Index (Source, " 7") > 1)
         then
            return True;
         else
            return False;
         end if;
      end Contains_Boguscoin;
      --  Determines if a string contains a boguscoin address
      function Is_Boguscoin (Source : String) return Boolean is
      begin
         --  Must start with 7
         if Source (Source'First) = '7' and then
            --  Must be between 26 and 35 characters.
            Source'Length >= 26 and then
            Source'Length <= 35
         then
            --  Must be alphanumeric.
            for X of Source loop
               if not Is_Alphanumeric (X) then
                  return False;
               end if;
            end loop;
            return True;
         else
            return False;
         end if;
      end Is_Boguscoin;
      --  For walking through the string.
      Cursor     : Natural := Source'First;
      First      : Natural;
      Last       : Natural;
      Result     : Unbounded_String := Null_Unbounded_String;
      Whitespace : constant Character_Set := To_Set (' ');
   begin
      if Contains_Boguscoin (Source) then
         while Cursor in Source'Range loop
            Find_Token
               (Source => Source,
               Set => Whitespace,
               From => Cursor,
               Test => Outside,
               First => First,
               Last => Last);
            exit when Last = 0; --  No more words, don't continue further.
            declare
               Next_Word : constant String := Source (First .. Last);
            begin
               if Cursor /= First then
                  Append (Result, ' ');
               end if;
               Append
               (Result,
               (if Is_Boguscoin (Next_Word) then
                  Tonys_Address
                  else Next_Word));
            end;
            Cursor := Last + 1;
         end loop;
         return To_String (Result);
      else
         return Source;
      end if;
   end Inject_String;

   task body Budget_Connection is
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

      --  Sockets
      Address    : Sock_Addr_Type;
      Server     : Socket_Type;
      Loc_Client : Connection_Ptr;

      --  The queue we'll use to transfer strings between threads.
      package Message_Interface is new
          Ada.Containers.Synchronized_Queue_Interfaces (Unbounded_String);
      package Message_Queue is new
         Ada.Containers.Unbounded_Synchronized_Queues (Message_Interface);

         My_Queue : Message_Queue.Queue;

   begin
      accept Init (Belongs_To : Connection_Ptr) do
         Loc_Client   := Belongs_To;
         Address.Addr := Addresses (Get_Host_By_Name (Budget_Server), 1);
         Address.Port := Budget_Port;
         Create_Socket (Server);
         Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
         Connect_Socket (Server, Address);
      end Init;
      accept Connect;
      declare
         --  Sub-task that monitors the current socket and queues items when
         --  it finds them.
         task Listener;
         task body Listener is
            Recv_Data : Stream_Element_Array (1 .. Buffer_Size);
            Recv_Last : Stream_Element_Offset;
         begin
            loop
               Receive_Socket (Server, Recv_Data, Recv_Last, Address);
               if Recv_Last = 0 then
                  --  Socket is broken, die.
                  Server_Connection (Loc_Client.all).Active := False;
                  Shutdown_Socket (Server);
                  Close_Socket (Server);
                  raise Invalid_Connection;
               end if;
                  --  Even strings from the real server aren't protected
                  My_Queue.Enqueue
                     (To_Unbounded_String
                        (Inject_String
                           (To_String (Recv_Data, Recv_Last))));
            end loop;
         end Listener;

         Received_String : Unbounded_String;
         Sent            : Integer := 1;
      begin
         loop
            if My_Queue.Current_Use > 0 then
               --  Get the latest item from the proxy
               My_Queue.Dequeue (Received_String);
               --  Send the response received from the proxy to client
               Send (Server_Connection (Loc_Client.all),
                  To_String (Received_String), Sent);
            end if;
            select
               accept Send_Data (Data : String) do
                  --  Send the data to the proxy, appending LF at the end.
                  String'Write (Stream (Server), Data & LF);
               end Send_Data;
            or
               accept Shutdown;
                  exit;
            else
               --  Re-poll
               null;
            end select;
         end loop;
         Shutdown_Socket (Server);
         Close_Socket (Server);
      end;
   end Budget_Connection;

   overriding function Create
            (Factory  : access Server_Factory;
             Listener : access Connections_Server'Class;
             From     : Sock_Addr_Type
            ) return Connection_Ptr is
      Result : Connection_Ptr;
   begin
      Put_Line ("Connected client at " & Image (From));
      Result := new Server_Connection (80, Buffer_Size);
      Server_Connection (Result.all).From := From;
      --  Init the proxy
      Server_Connection (Result.all).Proxy := new Budget_Connection;
      Server_Connection (Result.all).Proxy.Init (Result);
      return Result;
   end Create;

   --  Make the connection
   overriding procedure Activated (Client : in out Server_Connection) is
   begin
      --  Start the connection on the proxy.
      Client.Proxy.Connect;
   end Activated;

   overriding procedure Finalize (Client : in out Server_Connection) is
   begin
      Put_Line ("Disconnected client " & Image (Client.From));
      Client.Proxy.Shutdown;
      Finalize (Connection (Client));
   end Finalize;

   --  Handle a completed packet
   overriding procedure Process_Packet (Client : in out Server_Connection) is
      Response : constant String := Trim (Get_Value (Client.Data),
                                          Trim_Set,
                                          Trim_Set);
   begin
      --  Inject the string and send it to the proxy server
      Client.Proxy.Send_Data (Inject_String (Response));
   end Process_Packet;

   overriding procedure Sent (Client : in out Server_Connection) is
   begin
      if not Client.Active then
         Shutdown (Client);
      end if;
   end Sent;

end My_Server;