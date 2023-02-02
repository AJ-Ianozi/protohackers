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

with Ada.Text_IO; use Ada.Text_IO;
package body Prime_Checker is
   procedure Init_Primes is
      Prime_File : File_Type;
   begin
      --  Use pregenerated primes for most of our searching.
      Open(Prime_File, In_File, "millionprimes.txt");
      while not End_Of_File(Prime_File) loop
         declare
            Next_Line : constant String := Get_Line(Prime_File);
            Next_Prime : constant Long_Integer := Long_Integer'Value(Next_Line);
         begin
            --  I hope this works
            Prime_Array(Next_Prime) := True;
         end;
      end loop;
      Close(Prime_File);
   end Init_Primes;
   --  Quick function to determine if something is prime or not.
   function Is_Prime(I : Big_Integer) return Boolean is
      use Convert;
   begin
      --  2 is prime
      if I = 2 then
         return True;
      --  even numbers or lower than 2 are not prime
      elsif I < 2 or else I mod 2 = 0 then
         return False;
      --  Use our lookup table we built to quickly calculate from known primes
      elsif I <= 179424673 then
         return Prime_Array(From_Big_Integer(I));
      --  Have to calculate the prime the hard way.
      else
         declare
            Cur : Big_Integer := 179424674;
            Last : constant Big_Integer := I;
         begin
            loop
               if I mod Cur = 0 then
                  return False;
               else
                  Cur := Cur + 1;
               end if;
               exit when Cur = Last;
            end loop;
         end;
      end if;
      return True;
   end Is_Prime;
   --  Overriding functions to more easily calculate.
   function Is_Prime(I : Long_Integer) return Boolean is
      use Convert;
   begin
      return Is_Prime(To_Big_Integer(I));
   end Is_Prime;
   function Is_Prime(I : Long_Float) return Boolean is
      X : constant Long_Integer := Long_Integer (Long_Float'Truncation(I));
      use Convert;
   begin
      if X > 0 then
         declare
            Left : constant Long_Float := Long_Float'Remainder(I,
                                                               Long_Float(X));
         begin
            put_line(Left'Image);
            if Left = 0.0 then
               return Is_Prime(To_Big_Integer(X));
            else
               return False;
            end if;
         end;
      else
         return False;
      end if;
   end Is_Prime;
end Prime_Checker;