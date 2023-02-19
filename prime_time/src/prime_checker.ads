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

with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

package Prime_Checker is
   --  Must be called first.
   procedure Init_Primes;
   --  For calculating if a number is prime.
   function Is_Prime (I : Big_Integer)  return Boolean;
   function Is_Prime (I : Long_Integer) return Boolean;
   function Is_Prime (I : Long_Float)   return Boolean;
private
   package Convert is new Signed_Conversions (Long_Integer);
   Prime_Array : array (Long_Integer range 2 .. 179424673) of Boolean :=
                                                            [others => False];
end Prime_Checker;