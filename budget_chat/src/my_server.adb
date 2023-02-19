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
with Ada.Strings.Fixed;       use Ada.Strings.Fixed;
with Ada.Characters.Handling; use Ada.Characters.Handling;

package body My_Server is

   protected body UID is
      procedure Generate (Set : out Unique_ID) is
      begin
         Set := Current;
         Current := Current + 1;
      end Generate;
   end UID;

   protected body Users is
      procedure Insert (Key : Unique_ID; Item : User) is
      begin
            Map.Insert (Key, Item);
      end Insert;

      procedure Remove (Key : Unique_ID) is
      begin
         Map.Delete (Key);
      end Remove;

      function  Retrieve (Key : Unique_ID) return User is
         Result : constant User := Map.Element (Key);
      begin
         return Result;
      end Retrieve;

      procedure Sign_In (Key : Unique_ID; Username : Unbounded_String) is
         New_Item : constant User :=
                     (
                        Connection => Map.Element (Key).Connection,
                        Username => Username,
                        Signed_In => True
                     );
      begin
         Map.Replace (Key, New_Item);
      end Sign_In;

      function Get_Users
         (Exclude_Username : Unbounded_String := Null_Unbounded_String)
      return String is
         List : Unbounded_String := Null_Unbounded_String;
      begin
         for Item of Map
            when Item.Signed_In = True and then
                 Item.Username /= Exclude_Username
         loop
            List := List & ", " & Item.Username;
         end loop;
         return Slice
                  (List,
                    (if List = Null_Unbounded_String then 1 else 3),
                    Length (List));
      end Get_Users;

      function Send_Msg
               (Msg  : String;
                From : Unbounded_String := Null_Unbounded_String
               ) return Natural
      is
         Full_Message : constant String  := Msg & CR & LF;
         Len          : constant Integer := Full_Message'Length;
         Result       : Natural          := 0;
      begin
         for Item of Map
            when Item.Signed_In = True and then
                  Item.Username /= From
         loop
            Put_Line ("Telling " & To_String (Item.Username) & ": " & Msg);
            declare
               Sent : Integer := Full_Message'First;
            begin
               declare
               begin
                  Send (Server_Connection (Item.Connection.all),
                         Full_Message,
                         Sent);
               end;
               Put_Line ("Len is " & Len'Image & " and sent is " & Sent'Image);
            exception
               when others =>
                  --  Disconnect bad actor.
                  Finalize (Server_Connection (Item.Connection.all));
            end;
            Result := Result + 1;
         end loop;
         return Result;
      end Send_Msg;
   end Users;

   overriding function Create
            (Factory  : access Server_Factory;
             Listener : access Connections_Server'Class;
             From     : Sock_Addr_Type
            ) return Connection_Ptr is
      Result : Connection_Ptr;
      Client_UID : Unique_ID;
   begin
      Put_Line ("Connected client at " & Image (From));
      UID.Generate (Client_UID);
      Result := new Server_Connection (80, 8192);
      Server_Connection (Result.all).From := From;
      Server_Connection (Result.all).UID := Client_UID;
      User_List.Insert (Client_UID,
                        (Connection => Result,
                         Username   => Null_Unbounded_String,
                         Signed_In  => False)
                       );
      return Result;
   end Create;

   overriding procedure Activated (Client : in out Server_Connection) is
      Sent : Integer := 1;
   begin
      Send (Client,
            "Welcome to adachat!  Please enter your username." & CR & LF,
            Sent);
   end Activated;

   overriding procedure Finalize (Client : in out Server_Connection) is
   begin
      Put_Line ("Disconnected client " & Image (Client.From));
      if Client.Signed_In then
         declare
            Sent_Messages : constant Natural :=
            User_List.Send_Msg
               ("* " & To_String (Client.Username) & " has left the room",
               Client.Username);
         begin
            Put_Line ("Messages sent to " & Sent_Messages'Image & " users");
            --  This was part of my "use a common queue" idea but I scrapped it
            --  Leaving here in case I want to reference the code later.
            --  Messages.Enqueue ( (Msg_type => Left,
            --                      From => Username,
            --                      Msg => Null_Unbounded_String));
         end;
         User_List.Remove (Client.UID);
      end if;
      Finalize (Connection (Client));
   end Finalize;

   overriding procedure Process_Packet (Client : in out Server_Connection) is
      function Is_Alphanumeric (Item : String) return Boolean is
      begin
         for X of Item loop
            if not Is_Alphanumeric (X) then
               return False;
            end if;
         end loop;
         return True;
      end Is_Alphanumeric;
      Response : constant String := Trim (Get_Value (Client.Data),
                                          Trim_Set,
                                          Trim_Set);
      Sent : Integer := 1;
      Sent_Messages : Natural := 0;
   begin
      if Client.Signed_In then
         Sent_Messages :=
               User_List.Send_Msg ("[" & To_String (Client.Username) & "] "
                                          & Response, Client.Username);
--         This was part of my "use a common queue" idea but I scrapped it.
--          Leaving here in case I want to reference the code later.
--         Messages.Enqueue ( (Msg_type => Chat,
--                             From => Client.Username,
--                             Msg => To_Unbounded_String(Response)));
      else
         declare
         begin
            --  Potentially received a username.
            if Response'Length > 0 and then Is_Alphanumeric (Response) then
               --  Username is valid. Sign the chat person in.
               Client.Username := To_Unbounded_String (Response);
               User_List.Sign_In (Client.UID, Client.Username);
               Client.Signed_In := True;

               --  Announce to client
               Send (Client,
                     "* The room contains: " &
                      User_List.Get_Users (Client.Username) & CR & LF,
                     Sent);
               --  Announce client to others:
               Sent_Messages :=
                        User_List.Send_Msg
                                  ("* " & To_String (Client.Username) &
                                   " has entered the room", Client.Username);
--         This was part of my "use a common queue" idea but I scrapped it.
--          Leaving here in case I want to reference the code later.
--               Messages.Enqueue ( ( Msg_type => Entered,
--                                    From => Client.Username,
--                                    Msg => Null_Unbounded_String));
               Put_Line ("Client said '" & Response & "'" & CR & LF);
            else
               raise Invalid_Username;
            end if;
         end;
      end if;
      Put_Line ("Sent messages to " & Sent_Messages'Image & " users");
   exception
      when Constraint_Error | Invalid_Username =>
            Send (Client, "Invalid username.  Disconnecting :)....", Sent);
            Client.Active := False;
   end Process_Packet;

   overriding procedure Sent (Client : in out Server_Connection) is
   begin
      if not Client.Active then
         Shutdown (Client);
      end if;
   end Sent;

end My_Server;
