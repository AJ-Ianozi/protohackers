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

with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Ada.Strings.Maps;  use Ada.Strings.Maps;
with Prime_Checker;     use Prime_Checker;

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

with JSON.Parsers;
with JSON.Types;

package body Prime_Server is

   overriding function Create
            (Factory  : access Prime_Factory;
             Listener : access Connections_Server'Class;
             From     : Sock_Addr_Type
            ) return Connection_Ptr
   is
      Result : Connection_Ptr;
   begin
      Put_Line ("Connected client at " & Image (From));
      Result := new Prime_Connection (80, 120);
      Prime_Connection (Result.all).From := From;
      return Result;
   end Create;

   overriding procedure Finalize (Client : in out Prime_Connection) is
   begin
      Put_Line ("Disconnected client " & Image (Client.From));
      Finalize (Connection (Client));
   end Finalize;

   overriding procedure Received
             (Client  : in out Prime_Connection;
              Data    : Stream_Element_Array;
              Pointer : in out Stream_Element_Offset
             )
   is
      function As_String return String is
         Result : constant String (1 .. Data'Length) :=
            [for X of Data => Character'Val (X)];
      begin
         return Result;
      end As_String;
      Malformed : exception;
      package Types is new JSON.Types (Long_Integer, Long_Float);
      package Parsers is new JSON.Parsers (Types);
      --  Index or find to character(10) into the ubound string.
      --  Be sure to pass the pointer to it once I find a stop.
   begin
      Put_Line (" Received `" & As_String & "`");
      --  Go through each character, moving it into the buffer.
      for X in Data'Range loop
         --  Set pointer to the next item to process.
         Pointer := X;
         --  Add the next character to the string.
         Append (Client.Json_Str, Character'Val (Data (X)));
         --  Exit the loop prematurely if we've reached the end.
         exit when Character'Val (Data (X)) = Character'Val (10);
      end loop;
      --  Increment the pointer to the next unprocessed element.
      Pointer := Pointer + 1;

      --  If we received a full json object
      if
         Element (Client.Json_Str, Length (Client.Json_Str)) =
         Character'Val (10)
      then
         declare
            --  This is just to catch the parse error exception
         begin
            --  Process the json object.
            declare
               Parser : Parsers.Parser :=
                                 Parsers.Create (To_String (Client.Json_Str));
               Item : constant Types.JSON_Value := Parser.Parse;
               use Types;
            begin
               if Item.Kind = Object_Kind and then
                  Item.Contains ("method") and then
                  Item ("method").Kind = String_Kind and then
                  Item ("method").Value = "isPrime" and then
                  Item.Contains ("number")
               then
                  declare
                     IsPrime : Boolean;
                     Prime_I : Long_Integer;
                     Prime_F : Long_Float;
                  begin
                     case Item ("number").Kind is
                        when Float_Kind =>
                           Prime_F := Item ("number").Value;
                           IsPrime := Is_Prime (Prime_F);
                        when Integer_Kind =>
                           Prime_I := Item ("number").Value;
                           IsPrime := Is_Prime (Prime_I);
                        when others =>
                           raise Malformed;
                     end case;
                     declare
                        Result : constant String :=
                           (if IsPrime then
                              "{""method"":""isPrime"",""prime"":true}"
                            else
                              "{""method"":""isPrime"",""prime"":false}")
                           & Character'Val (10);
                        Ptr : Integer := 0;
                     begin
                        --  Send result
                        Send (Client, Result, Ptr);
                     end;
                  end;
               else
                  raise Malformed;
               end if;
            end;
         exception
            when Parsers.Parse_Error =>
               --  This was added on in the last minute when they tried to test
               --  if a giant integer was prime or not, and json-ada does not
               --  support big ints.
               if Element (Client.Json_Str, 1) = '{'
                  and then
                     (Element
                      (Client.Json_Str, Length (Client.Json_Str) - 1) = '}'
                     or else
                       Element
                        (Client.Json_Str, Length (Client.Json_Str) - 2) = '}')
                  and then Index
                              (Client.Json_Str, """method"":""isPrime""") > 0
                  and then Index (Client.Json_Str, """number"":") > 0
               then
                  declare
                     Check_Set : constant Character_Set :=
                                             To_Set (Sequence => ",}");
                     Number_Start : constant Natural :=
                                                Index
                                                   (Client.Json_Str,
                                                     """number"":");
                     Sub_String : constant String :=
                                             Slice (Client.Json_Str,
                                                   Number_Start + 9,
                                                   Length (Client.Json_Str));
                     EntryString : constant String :=
                                             Sub_String
                                               (Sub_String'First ..
                                                 Index
                                                  (Sub_String, Check_Set) - 1);
                     Our_Big_Int : constant Big_Integer :=
                                                   From_String (EntryString);
                     Result : constant String :=
                           (if Is_Prime (Our_Big_Int) then
                              "{""method"":""isPrime"",""prime"":true}"
                            else
                              "{""method"":""isPrime"",""prime"":false}")
                           & Character'Val (10);
                        Ptr : Integer := 0;
                  begin
                     Put_Line ("Sending: `" & Result & "`");
                     Send (Client, Result, Ptr);
                  end;
               else
                  raise Malformed;
               end if;
         end;
         Client.Json_Str := Null_Unbounded_String;
      end if;

   exception
      when others =>
         declare
            Err : constant String := "malformed" & Character'Val (10);
            Ptr : Integer := Err'First;
         begin
            Put_Line ("THIS WAS MALFORMED - `" &
                     To_String (Client.Json_Str) & "`");
            Send (Client, Err, Ptr);
            Client.Active := False;
         end;
   end Received;

   overriding procedure Sent (Client : in out Prime_Connection) is
   begin
      --  If client was deactivated, shut down after latest send.
      if not Client.Active then
         Shutdown (Client);
      end if;
   end Sent;

end Prime_Server;
