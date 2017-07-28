-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

with Ada.Finalization;

package Storage is

private

   type Arr2 is array (Natural range <>) of Natural;
   type Arr2_Ptr is access Arr2;

   type Hash_Table(Size : Natural) is new Ada.Finalization.Controlled with
   record
      Ptr : Arr2_Ptr;
   end record;

   procedure Finalize  (Object : in out Hash_Table);
   procedure Initialize(Object : in out Hash_Table);

end Storage;