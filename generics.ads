-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

package Generics is

   generic
      type T is private;
   procedure Swap(Left, Right : in out T);

   generic
      type T is (<>);
   procedure Inc(N : in out T);

   generic
      type T is (<>);
   procedure Dec(N : in out T);

   generic
      type T1 is (<>);--range <>;
      type T2 is (<>);--range <>;
   procedure Add(X : in out T1; Y : in T2);

   generic
      type T is digits <>;
   procedure Add_Float(X : in out T; Y : in T);

   generic
      type T is delta <>;
   procedure Add_Fixed(X : in out T; Y : in T);

   generic
      type T is (<>);
   function Cat (Left : in String; Right : in T) return String;

   pragma Inline(Swap);
   pragma Inline(Inc);
   pragma Inline(Dec);
   pragma Inline(Add);
   pragma Inline(Add_Float);
   pragma Inline(Add_Fixed);

end;
