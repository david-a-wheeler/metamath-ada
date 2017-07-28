-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

package body Generics is

   procedure Swap(Left, Right : in out T) is
      Temp : constant T := Left;
   begin
      Left := Right;
      Right := Temp;
   end;

   procedure Inc(N : in out T) is
   begin
      N := T'Succ(N);
   end;

   procedure Dec(N : in out T) is
   begin
      N := T'Pred(N);
   end;

   procedure Add(X : in out T1; Y : in T2) is
   begin
      X := T1'Val(T1'Pos(X)+T2'Pos(Y));
      --   exception
      --      when others =>
      --         Ada.Text_Io.Put_Line(T1'Image(X) & T2'Image(Y));
   end;

   procedure Add_Float(X : in out T; Y : in T) is
   begin
      X := X+Y;
   end;

   procedure Add_Fixed(X : in out T; Y : in T) is
   begin
      X := X+Y;
   end;

   function Cat (Left : in String; Right : in T) return String is
   begin
      return Left & T'Image(Right);
   end;


end;
