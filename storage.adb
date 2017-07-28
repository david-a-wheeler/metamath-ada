-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

with Unchecked_Deallocation;

package body Storage is

   procedure Initialize(Object : in out Hash_Table) is
   begin
      Object.Ptr := new Arr2(0..(13*Object.Size)/10);
      --Object.Ptr.All := (others=>Natural'Last);
      for I in Object.Ptr.All'range loop
         Object.Ptr.All(I) := Natural'Last;
      end loop;
      --Put_Line("procedure Initialize" & Object.Size & Object.Ptr.All'Length);
   end;

   procedure Finalize(Object : in out Hash_Table) is
      procedure Free is new Unchecked_Deallocation(Arr2, Arr2_Ptr);
   begin
      if Object.Ptr/= null then
         Free(Object.Ptr);
      end if;
      --Put_Line("procedure Finalize" & Object.Size);
   end;
end Storage;

