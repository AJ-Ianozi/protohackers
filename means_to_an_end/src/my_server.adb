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

with Ada.Streams; use Ada.Streams;
with Ada.Text_IO; use Ada.Text_IO;

package body My_Server is

   --  For the ordered set.
   function "<" (Left, Right : Query_Entry) return Boolean is
   begin
      return Left.Timestamp < Right.Timestamp;
   end "<";
   overriding function "=" (Left, Right : Query_Entry) return Boolean is
   begin
      return Left.Timestamp = Right.Timestamp;
   end "=";

   overriding function Create
     (Factory  : access Server_Factory;
      Listener : access Connections_Server'Class; From : Sock_Addr_Type)
      return Connection_Ptr
   is
      Result : Connection_Ptr;
   begin
      Put_Line ("Connected client at " & Image (From));
      Result                              := new Server_Connection (80, 120);
      Server_Connection (Result.all).From := From;
      return Result;
   end Create;

   overriding procedure Finalize (Client : in out Server_Connection) is
   begin
      Put_Line ("Disconnected client " & Image (Client.From));
      Finalize (Connection (Client));
   end Finalize;

   overriding procedure Process_Packet (Client : in out Server_Connection) is
      use Query_Entries;
   begin

      case Client.Operation.Value is
         when 73 => -- 'I': Insert
            declare
               Next_Query : constant Query_Entry :=
                 (Timestamp => Client.Arg1.Value, Price => Client.Arg2.Value);
               Inserted : Boolean;
               Position : Cursor;
            begin
               --  Attempt to insert.  Inserted is set to false if non-existant
               Client.Query_List.Insert (Next_Query, Position, Inserted);
               if not Inserted then
                  --  Undefined behaivor, kill connection.
                  raise Invalid_Connection;
               end if;
            end;

         when 81 => -- 'Q': Query
            declare
               --  The response:
               Packet  : Stream_Element_Array (1 .. 9);
               Pointer : Stream_Element_Offset := Packet'First;
               Index   : Stream_Element_Offset := Packet'First;
               --  The test.
               Min_Time : constant Query_Entry :=
                 (Timestamp => Client.Arg1.Value, Price => 0);
               Max_Time : constant Query_Entry :=
                 (Timestamp => Client.Arg2.Value, Price => 0);
               Start_Pos : constant Cursor :=
                 Client.Query_List.Ceiling (Min_Time);
               End_Pos : constant Cursor := Client.Query_List.Floor (Max_Time);
               Cur     : Cursor          := Start_Pos;
               Count   : Integer         := 0;
               Result  : Integer_32      := 0;
               Calc    : Long_Integer    := 0;
            begin
               --  Some quick checks.
               if Min_Time.Timestamp <= Max_Time.Timestamp
                 and then Start_Pos /= No_Element
                 and then End_Pos /= No_Element
                 and then Element (Start_Pos).Timestamp <=
                   Element (End_Pos).Timestamp
               then
                  --  Calculate the mean.
                  while Cur /= No_Element loop
                     Count := Count + 1;
                     Calc  := Calc + Long_Integer (Element (Cur).Price);
                     exit when Cur = End_Pos;
                     Cur := Next (Cur);
                  end loop;
                  if Calc /= 0 then
                     Result :=
                       Integer_32 (Long_Float (Calc) / Long_Float (Count));
                  end if;
               end if;
               --  Send the result.
               Big_Endian.Integers.Put (Packet, Pointer, Result);
               Send (Client, Packet (Packet'First .. Pointer - 1), Index);
               if Index /= Pointer then
                  raise Invalid_Connection;
               end if;
            end;

         when others =>
            raise Invalid_Connection;
      end case;
      New_Line;
   end Process_Packet;

   overriding procedure Sent (Client : in out Server_Connection) is
   begin
      if not Client.Active then
         Shutdown (Client);
      end if;
   end Sent;

end My_Server;
