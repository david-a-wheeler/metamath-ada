-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

with Use_Pack; use Use_Pack;

package body Storage.Sets is

   procedure Clear(S : in out Set) is
   begin
      S.Last:=0;
   end;

   procedure Clear(S : in out Hashed_Set) is
   begin
      S.Last:=0;
      for I in S.Ht.Ptr.All'range loop
         S.Ht.Ptr(I) := Natural'Last;
      end loop;
   end;


   procedure Add(S : in out Set; E : in Element; Check : in Boolean :=False) is
   begin
      if Check then
         for I in 1..S.Last loop
            if S.Equal(S.Elements(I), E) then
               return;
            end if;
         end loop;
      end if;
      --pragma Assert(S.Last < S.Size, "Sets.Add : set full " & S.Size);
      Inc(S.Last);
      S.Elements(S.Last) := E;
   end;

   procedure Add(S : in out Hashed_Set; E : in Element; Check : in Boolean := False) is
      Hi : Natural;
   begin
      if Check and then Get_Index(S,E)>0 then
         return;
      end if;
      Add(Set(S),E);
      Hi := S.Hash(E) mod S.Ht.Ptr'Length;
      while S.Ht.Ptr(Hi)/=Natural'Last loop
         Hi := (Hi+1) mod S.Ht.Ptr'Length;
      end loop;
      S.Ht.Ptr(Hi) := S.Last;
   exception
      when others=>
         Put_Line("procedure Add:" & S.Last);
         raise;
   end;

   procedure Delete(S : in out Set; Index : Positive) is
   begin
      pragma Assert(Index in 1..S.Last);
      S.Elements(Index) := S.Elements(S.Last);
      Dec(S.Last);
   end;

   function Get_Index(S : Set; E : Element) return Natural is
   begin
      for I in 1..S.Last loop
         if S.Equal(S.Elements(I), E) then
            return I;
         end if;
      end loop;
      return 0;
   end;

   function Get_Index(S : in Hashed_Set; E : in Element) return Natural is
      Hi : Natural;
   begin
      Hi := S.Hash(E) mod S.Ht.Ptr'Length;
      while S.Ht.Ptr(Hi)/=Natural'Last loop
         if S.Equal(S.Elements(S.Ht.Ptr(Hi)),E) then
            --            Put_Line("function Get_Index" & Hi);
            return S.Ht.Ptr(Hi);
         end if;
         Hi := (Hi+1) mod S.Ht.Ptr'Length;
      end loop;
      return 0;
   end;

   function Get(S : in Set; Index : Positive) return Element is
   begin
      return S.Elements(Index);
   end;

   function Last(S : in Set) return Natural is
   begin
      return S.Last;
   end;

   procedure Iterate(S : in Set; Process : in Process_Procedure) is
   begin
      for I in 1..S.Last loop
         Process(S.Elements(I));
      end loop;
   end;

end Storage.Sets;