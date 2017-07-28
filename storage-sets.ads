-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

generic
   type Element is private;
package Storage.Sets is

   type Relation is access function(L, R : Element) return Boolean;

   type Set(Size : Natural; Equal : Relation) is tagged private;

   procedure Clear(S : in out Set);
   procedure Add(S : in out Set; E : in Element; Check : in Boolean :=False);
   procedure Delete(S : in out Set; Index : Positive);

   type Process_Procedure is access procedure(E : in Element);

   procedure Iterate(S : in Set; Process : in Process_Procedure);

   function Last(S : Set) return Natural;
   function Get(S : Set; Index : Positive) return Element;
   function Get_Index(S : Set; E : Element) return Natural;

   type Hash_Function is access function(E : Element) return Natural;

   type Hashed_Set(Size : Natural; Equal : Relation; Hash : Hash_Function) is
      new Set with private;

   procedure Add(S : in out Hashed_Set; E : in Element; Check : in Boolean :=False);

   function Get_Index(S : Hashed_Set; E : Element) return Natural;
   -- 0 if E don't exist
   procedure Clear(S : in out Hashed_Set);

private

   type Arr is array (Positive range <>) of Element;

   type Set(Size : Natural; Equal : Relation) is tagged record
      Elements : Arr(1..Size);
      Last : Natural :=0;
   end record;

   type Hashed_Set(Size : Natural; Equal : Relation; Hash : Hash_Function) is
      new Sets.Set(Size, Equal) with
      record
      Ht : Hash_Table(Size);
   end record;

end Storage.Sets;