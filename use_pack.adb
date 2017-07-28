-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

with Ada.Calendar; use Ada.Calendar;

package body Use_Pack is


   type Hash_Value is mod 2**31-1;

   function Hash(S : String) return Natural is
      H : Hash_Value := 0;
   begin
      for I in S'range loop
         H := 256*H+Character'Pos(S(I));
      end loop;
      return Natural(H);
   end;

   function Hash(Sp : String_P) return Natural is
   begin
      return Hash(Sp.All);
   end;

   function "="(L,R : String_P) return Boolean is
   begin
      return L.All=R.All;
   end;

   Error : exception;

   procedure Assert(B : in Boolean; Text : in String := "") is
   begin
      if not B then
         Put_Line(Text);
         raise Error;
      end if;
   end;

   function Min(X1, X2 : Integer) return Integer is
   begin
      if X1<X2 then
         return X1;
      else
         return X2;
      end if;
   end Min;

   function "not" (Right : in Character_Set) return Character_Set is
   begin
      return Character_Set (not Character_Set_Internal (Right));
   end;

   function "or"  (Left, Right : in Character_Set) return Character_Set is
   begin
      return Character_Set
         (Character_Set_Internal (Left) or Character_Set_Internal (Right));
   end;

   Tab : constant Character := Character'Val(9);

   Char_Map : constant array (Character) of Character_Set :=
      (Tab => Tab_C,
      ' ' => Space,
      '0'..'9' => Digit,
      'A'..'Z' => Uc_Letter,
      'a'..'z' => Lc_Letter,
      others => Other);

   function Is_A(C : Character_Set; Ch : Character) return Boolean is
   begin
      return (Char_Map(Ch) and C) /= 0;
   end;

   function Find(C : Character_Set; S : String) return Natural is
   begin
      for I in S'range loop
         if Is_A(C, S(I)) then
            return I;
         end if;
      end loop;
      return 0;
   end;

   procedure Wait_Key(C : Character := Ascii.Nul) is
      C2 : Character;
   begin
      loop
         Ada.Text_Io.Get_Immediate(C2);
         exit when C = Ascii.Nul or C2 = C;
      end loop;
   end;

   ---------------------------------------------------------

   function "&" (Left : in Integer; Right : in String) return String is
   begin
      return Integer'Image(Left) & Right;
   end;

   function "&" (Left : in Integer; Right : in Integer) return String is
   begin
      return Integer'Image(Left) & Integer'Image(Right);
   end;

   ---------------------------------------------------------

   function To_String(B : Bstring) return String is
   begin
      return B.Data(1..B.Length);
   end;

   function Last_Char(B : Bstring) return Character is
   begin
      return B.Data(B.Length);
   end;

   procedure Add(B : in out Bstring; S : in String) is
   begin
      B.Data(B.Length+1..B.Length+S'Length) := S;
      B.Length := B.Length+S'Length;
   end;

   ---------------------------------------------------------

   procedure Start(T : in out Timer) is
   begin
      T.Start_Time := Ada.Calendar.Clock;
   end;

   procedure Stop(T : in out Timer) is
   begin
      T.Stop_Time := Ada.Calendar.Clock;
   end;

   function Time(T : Timer) return Duration is
      Extime_Clock : constant Duration := 0.000001466;
   begin
      return T.Stop_Time-T.Start_Time-Extime_Clock;
   end;

   package Duration_Io is new Ada.Text_Io.Fixed_Io(Duration);

   procedure Put_Line(T : in Timer) is
   begin
      Duration_Io.Put(Time(T));
      Ada.Text_Io.New_Line;
   end;

end;                                  