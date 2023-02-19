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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Maps;      use Ada.Strings.Maps;

with GNAT.Sockets.Connection_State_Machine.Terminated_Strings;
use GNAT.Sockets.Connection_State_Machine.Terminated_Strings;

with Ada.Containers.Ordered_Maps;

--  This was part of my "use a common queue" idea but I scrapped it.
--  Leaving here in case I want to reference the code later.
--  with Interfaces;            use Interfaces;
--  with Ada.Containers.Synchronized_Queue_Interfaces;
--  with Ada.Containers.Unbounded_Synchronized_Queues;

package My_Server is
   use GNAT.Sockets.Connection_State_Machine;

   --  Constants and character set for trimming later
   LF       : constant String        := "" & Character'Val (10);
   CR       : constant String        := "" & Character'Val (13);
   Trim_Set : constant Character_Set := To_Set (Sequence => " " & LF & CR);

   Invalid_Connection, Invalid_Username : exception;

   --  Server connection and factory.
   type Server_Factory is new Connections_Factory with private;
   type Server_Connection is new Connection with private;

   --  Poor Man's UIDs
   type Unique_ID is mod 2**64;
   protected UID is
      procedure Generate (Set : out Unique_ID);
   private
      Current : Unique_ID := 0;
   end UID;

   --  User List
   type User is record
      Connection : Connection_Ptr;
      Username   : Unbounded_String := Null_Unbounded_String;
      Signed_In  : Boolean          := False;
   end record;

   --  Ordered map to hold the users.  This could also be a vector.
   package User_Map is new Ada.Containers.Ordered_Maps
     (Key_Type => Unique_ID, Element_Type => User);

   protected type Users is
      --  Adds user to list.
      procedure Insert (Key : Unique_ID; Item : User);
      --  Removes user from list.
      procedure Remove (Key : Unique_ID);
      --  Retrieves user from list.
      function Retrieve (Key : Unique_ID) return User;
      --  Signs in the user based on UID.
      procedure Sign_In (Key : Unique_ID; Username : Unbounded_String);
      --  Sends a message to all users except for "From".
      function Send_Msg
        (Msg : String; From : Unbounded_String := Null_Unbounded_String)
         return Natural; -- Messages sent.
      --  Returns a list of users.
      function Get_Users
        (Exclude_Username : Unbounded_String := Null_Unbounded_String)
         return String;
   private
      Map : User_Map.Map;
   end Users;

   --  The actual user list
   User_List : Users;

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
      UID    : Unique_ID;
      Active : Boolean := True;

      --  Quick access account settings.
      Signed_In : Boolean          := False;
      Username  : Unbounded_String := Null_Unbounded_String;

      --  Message is at max, 8kb characters long. Terminated by chr10 (\n)
      Data : Terminated_Strings.String_Data_Item (8_192, Character'Val (10));
   end record;

   --  This was part of my "use a common queue" idea but I scrapped it.
   --  Leaving here in case I want to reference the code later.
   --  Messages
   --  type Message_Type is (Chat, Entered, Left);
   --  type Message is record
   --     Msg_type : Message_Type := Chat;
   --     From     : Unbounded_String := Null_Unbounded_String;
   --     Msg      : Unbounded_String := Null_Unbounded_String;
   --  end record;
   --
   --  package Message_Interface is new
   --       Ada.Containers.Synchronized_Queue_Interfaces(Message);
   --  package Message_Queue is new
   --      Ada.Containers.Unbounded_Synchronized_Queues(Message_Interface);
   --
   --  The actual message queue
   --  Messages : Message_Queue.Queue;

end My_Server;
