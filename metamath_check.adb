-- Copyright (C) 2017  Tony Häger,  tony7@tele2.se

with Use_Pack; use Use_Pack;
with Storage.Sets;

-- Reads a metamath database file from standard in and checks all proofs.
-- If a proof is wrong, hopefully it is reported with some more or less helpful message.

procedure Metamath_Check is
   use type Integer_16;

   package P is new Storage.Sets(String_P); use P;

   -- Hashed_Set is used to represent constants, variables and labels with positive numbers.

   -- If the size of a data structure is too small the program will just crash.

   Constants : Hashed_Set(1100, "="'access , Hash'access);
   Variables : Hashed_Set(430, "="'access , Hash'access);
   Variable_Type : array(Integer_16 range 1..430) of Natural; -- Nr of the variables' last $f statement .

   Data : array (1..1350000) of Integer_16; -- Store of all expressions, negative number are variables,
   --                                          positive number are constants.
   Data_Last : Natural :=0; -- Last used position in Data

   type Expression is record
      First, Last : Natural := 0; -- Indices in Data for the expression.
   end record;

   Labels : Hashed_Set(70000, "="'access , Hash'access);
   Expr : array (1..Labels.Size) of Expression;
   Ekind : array (Expr'range) of Character; -- $f, $e, $a or $p

   Hypotheses : array(1..75000) of Natural; -- Lists of $e hypotheses for each assertion. Index in Expr.
   H_Last : Natural := 0;
   First_Hyp, Last_Hyp : array (Expr'range) of Natural; -- Indices in Hypotheses for each assertion.

   Used_Variables : array(1..160000) of Integer_16; -- Lists of mandatory variables in each assertion.
   V_Last : Natural := 0;
   First_Var, Last_Var : array (Expr'range) of Natural; -- Indices in Used_Variables for each assertion.

   D1, D2 : array(1..170000) of Integer_16; -- Lists of disjoint variables for each assertion.
   D_Last : Natural := 0;
   First_Dvar, Last_Dvar : array (Expr'range) of Natural; -- Indices in D1, D2 for each assertion.

   Line : String(1..200); -- One line of the read database file.
   Last, P1 : Natural; -- Indices in Line. Last: last char on the line. P1: first position of current token.
   P2 : Natural := Natural'Last; -- First position after the current token.

   Line_Nr : Natural := 0; -- Current line number of the file.
   --File : File_Type;

   function "="(L, R : Expression) return Boolean is
   begin
      return Data(L.First..L.Last)=Data(R.First..R.Last);
   end;

   --Nr_Hsv : Natural :=0;

   Eof : Boolean := False;

   procedure Skip_White_Space is
   begin
      --Put_Line("Skip_White_Space");
      loop
         P1 := Find(not White_Space, Line(P2..Last));
         if P1>0 then
            exit;
            --elsif not End_Of_File(File) then
         elsif not End_Of_File then
            --Get_Line (File, Line, Last);
            Get_Line (Line, Last);
            Inc(Line_Nr);
            P2 := 1;
         else
            Eof := True;
            exit;
         end if;
      end loop;
   end;

   Keep_Token : Boolean := False; -- Return the current token again.

   function Next_Token_I return String is -- A token is a string of any chars except white space.
   begin
      if Keep_Token then
         Keep_Token := False;
         if P1<P2 then
            return Line(P1..P2-1);
         else
            return Next_Token_I;
         end if;
      else
         Skip_White_Space;

         if Eof then
            return "EOF";
         else
            P2 := Find(White_Space, Line(P1..Last));
            if P2>0 then
               return Line(P1..P2-1);
            else
               P2 := Last+1;
               return Line(P1..Last);
            end if;
         end if;
      end if;
   end;

   function Next_Token return String is
      T : constant String := Next_Token_I ;
   begin
      if T="$(" then
         while Next_Token_I /="$)" loop -- Skip comment.
            null;
         end loop;
         return Next_Token;
      else
         --Put_Line("Next_Token:" & T);
         return T;
      end if;
   end;

   function Current_Token return String is
   begin
      if Eof then
         return "EOF";
      else
         return Line(P1..P2-1);
      end if;
   end;

   function Read_Nr return Natural is -- Read number in a compressed proof. 'Z' -> Natural'Last.
      N : Natural :=0;
   begin
      --Put_Line("Read_Nr");
      loop
         Skip_White_Space;
         P2 := P1+1;
         if Line(P1) in 'A'..'T' then
            N := 20*N+1+Character'Pos(Line(P1))-Character'Pos('A');
            exit;
         elsif Line(P1) in 'U'..'Y' then
            N := 5*N+1+Character'Pos(Line(P1))-Character'Pos('U');
         elsif Line(P1)='Z' then
            return Natural'Last;
         else
            P2 := P1;
            exit;
         end if;
      end loop;

      return N;
   end;

   function To_String(E : Expression) return String is
      S : Bstring(5+2*(E.Last-E.First));
   begin
      for I in E.First..E.Last loop
         if Data(I)<0 then -- variable
            if I>E.First and then (Data(I-1)<0 or else Is_A(Letter, Last_Char(S))) then
               Add(S, " "); -- increase readability
            end if;
            Add(S, P.Get(Variables, Natural(-Data(I))).all);
         else -- constant
            Add(S, P.Get(Constants, Natural(Data(I))).all);
         end if;
      end loop;
      return To_String(S);
   exception
      when Constraint_Error =>
         Put_Line("To_String:" & E.First & E.Last);
         return "";
   end;

   procedure Define_Constants is
   begin
      while Next_Token/= "$." loop
         Add(Constants, new String'(Current_Token)) ;-- Hmm, no check if it is already defined?
      end loop;
   end;

   procedure Define_Variables is
   begin
      while Next_Token/= "$." loop
         Add(Variables, new String'(Current_Token)); -- Hmm, no check if it is already defined?
      end loop;
   end;

   function Get_Label_Nr(S : String) return Natural is
      Sp : String_P := new String'(S);
      X : Natural;
   begin
      X := Get_Index(Labels, Sp);
      Free(Sp);
      pragma Assert(X /=0, "Undefined label:" & S);
      return X;
   end;

   --Max_Stack : Natural := 0;

   function Last_E return Natural is
   begin
      return P.Last(Labels);
   end;

   procedure Skip_Proof is
   begin
      while Next_Token/="$." loop
         null;
      end loop;
   end;

   type Expressions is array (Natural range <>) of Expression;

   --   Uv_Last_Max : Natural :=0;
   --   Last_Rt_Max : Natural :=0;
   --   Last_Z_Max : Natural :=0;

   type Used_Vars is array (Integer_16 range <>) of Boolean;

   procedure Check_Variables(X : in Natural; Vars : in out Used_Vars; Nr_Vars : in out Integer) is
      -- mark and count all variables in the expression
   begin
      for J in Expr(X).First..Expr(X).Last loop
         if Data(J)<0 then
            if not Vars(-Data(J)) then
               Inc(Nr_Vars);
               Vars(-Data(J)) := True;
            end if;
         end if;
      end loop;
   end;

   procedure Check_Proof is

      Stack : Expressions(1..300);
      Stack_Last : Natural := 0;

      --      procedure Show_Stack is
      --      begin
      --         Put_Line("Stack:");
      --         for I in 1..Stack_Last loop
      --            Put_Line(To_String(Stack(I)));
      --         end loop;
      --      end;

      Vars1 : Used_Vars(1..Integer_16(P.Last(Variables))) := (others => False);

      Fail : Boolean := False; -- Some error found in the proof.

      --      Qwe : array(First_Dvar(Last_E)..First_Dvar(Last_E)+Nr_Dvar(Last_E)-1) of Boolean :=
      --         (others => False);  -- Check unused disjoint variable restrictions

      procedure Handle(X : Natural) is

         Vars : Used_Vars(1..Integer_16(P.Last(Variables))) := (others => False);
         Subs : array (Vars'range) of Expression;

         procedure Compare(E1, E2 : Expression) is -- with Subs on E2
            I1 : Natural := E1.First;
         begin
            --Put_Line("Compare:" & E1.First & E1.Last & E2.First & E2.Last);
            Abc:
               for I2 in E2.First..E2.Last loop
               if Data(I2)<0 and then Vars(-Data(I2)) then
                  for J in Subs(-Data(I2)).First..Subs(-Data(I2)).Last loop
                     if Data(I1)/=Data(J) then
                        Fail := True;
                        exit Abc;
                     end if;
                     Inc(I1);
                  end loop;
               else
                  if Data(I1)/=Data(I2) then
                     Fail := True;
                     exit Abc;
                  end if;
                  Inc(I1);
               end if;
            end loop Abc;
         end;

         procedure Copy(E1 : in Expression; E2 : out Expression) is -- E2 := E1 with Subs
         begin
            E2.First := Data_Last+1;
            for I in E1.First..E1.Last loop
               if Data(I)<0 and then Vars(-Data(I)) then
                  for J in Subs(-Data(I)).First..Subs(-Data(I)).Last loop
                     Inc(Data_Last);
                     Data(Data_Last) := Data(J);
                  end loop;
               else
                  Inc(Data_Last);
                  Data(Data_Last) := Data(I);
               end if;
            end loop;
            E2.Last := Data_Last;
         end;

         function Have_Same_Variable(L, R : Expression) return Boolean is
         begin
            --Put_Line("Have_Same_Variable " & To_String(L) & " " & To_String(R));

            for I in L.First..L.Last loop
               if Data(I)<0 then -- variable
                  for J in R.First..R.Last loop
                     if Data(J)<0 then -- variable

                        --Inc(Nr_Hsv);
                        if Data(I)=Data(J) then
                           return True;
                        elsif Vars1(-Data(I)) and Vars1(-Data(J)) then
                           for K in First_Dvar(Last_E)..Last_Dvar(Last_E) loop
                              if (-Data(I)=D1(K) and -Data(J)=D2(K)) or
                                    (-Data(I)=D2(K) and -Data(J)=D1(K)) then
                                 --Qwe(K) := True;
                                 goto Aaa;
                              end if;
                           end loop;

                           Fail:= True;
                           Put_Line("Missing disjoint variable requirement in " &
                              Get(Labels, Last_E).all & " for variables "
                              & P.Get(Variables, Natural(-Data(I))).all & " and "
                              & P.Get(Variables, Natural(-Data(J))).all);

                           <<Aaa>> null;
                        end if;
                     end if;
                  end loop;
               end if;
            end loop;

            return False;
         end;
         J : Natural;

         Nr_Hyp_X : constant Natural := Last_Hyp(X)-First_Hyp(X)+1;
         Nr_Vars_X : constant Natural := Last_Var(X)-First_Var(X)+1;
      begin
         if Stack_Last<Nr_Hyp_X+Nr_Vars_X then
            Put_Line("Stack underflow");
            Fail := True;
            return;
         end if;

         for I in 1..Nr_Vars_X loop
            Vars(Used_Variables(First_Var(X)+I-1)) := True;

            if Data(Expr(Variable_Type(Used_Variables(First_Var(X)+I-1))).First) /=
                  Data(Stack(Stack_Last-(Nr_Hyp_X+Nr_Vars_X)+I).First) then
               Fail := True;
               Put_Line("Wrong variable type");
            end if;

            Subs(Used_Variables(First_Var(X)+I-1)) := (Stack(Stack_Last-(Nr_Hyp_X+Nr_Vars_X)+I).First+1,
               Stack(Stack_Last-(Nr_Hyp_X+Nr_Vars_X)+I).Last);
            --Put_Line("Sub:" & P.Get(Variables, Used_Variables(I)).all);
         end loop;

         for I in First_Dvar(X)..Last_Dvar(X) loop
            if (Vars(D1(I)) and Vars(D2(I))) and then Have_Same_Variable(Subs(D1(I)), Subs(D2(I))) then
               Fail := True;
               Put_Line("Fail: distinct variable violation in " &
                  Get(Labels, Last_E).all & " Line Nr " & Line_Nr);
               Put_Line("Using " & Get(Labels, X).all);
               Put_Line("Variable " & P.Get(Variables, Natural(D1(I))).all & " := "
                  & To_String(Subs(D1(I))));
               Put_Line("Variable " & P.Get(Variables, Natural(D2(I))).all & " := "
                  & To_String(Subs(D2(I))));
               return;
            elsif Fail then
               return;
            end if;
         end loop;

         J := Nr_Vars_X+1;

         for I in First_Hyp(X)..Last_Hyp(X) loop
            Compare(Stack(Stack_Last-(Nr_Hyp_X+Nr_Vars_X)+J), Expr(Hypotheses(I)));
            if Fail then
               Put_Line("Using " & Get(Labels, X).all);
               Put_Line("Hypothese:" & (I-First_Hyp(X)+1) & " not equal");
               return;
            end if;
            Inc(J);
         end loop;

         Inc(Stack_Last);
         --Max_Stack := Max(Max_Stack, Stack_Last);
         Copy(Expr(X), Stack(Stack_Last));

         Add(Stack_Last, -(Nr_Hyp_X+Nr_Vars_X)); -- Remove used hypotheses from the stack.
         Stack(Stack_Last) := Stack(Stack_Last+Nr_Hyp_X+Nr_Vars_X);
      end;

      procedure Handle2(X : Natural) is -- better name!
      begin
         if Ekind(X) in 'e'..'f' then
            if Ekind(X) = 'e' then
               Fail := True;
               for I in First_Hyp(Last_E)..Last_Hyp(Last_E) loop
                  if X=Hypotheses(I) then
                     Fail := False;
                     exit;
                  end if;
               end loop;
               if Fail then
                  Put_Line(Current_Token & "is not hypothesis of " & Get(Labels, Last_E).all);
               end if;
            end if;
            Inc(Stack_Last);
            Stack(Stack_Last) := Expr(X);
         else
            --Show_Stack;

            Handle(X);
            --Show_Stack;
         end if;
      end;

      Reffed_Th : array (1..350) of Natural;
      Last_Rt : Natural :=0;

      Z_Th : array (1..550) of Expression; -- reused proof steps
      Last_Z : Natural :=0;

      Initial_Data_Last : constant Natural := Data_Last;
      X : Natural;

      Nr_Hyp_E : constant Natural := Last_Hyp(Last_E)-First_Hyp(Last_E)+1;
      Nr_Vars_E : constant Natural := Last_Var(Last_E)-First_Var(Last_E)+1;

   begin
      --Put_Line("procedure Check_Proof" & Last_E);

      for I in 1..Nr_Vars_E loop
         Vars1(Used_Variables(First_Var(Last_E)+I-1)) := True;
      end loop;

      if Next_Token="(" then -- compressed format

         while Next_Token/= ")" loop
            Inc(Last_Rt);
            Reffed_Th(Last_Rt) := Get_Label_Nr(Current_Token);
            if Reffed_Th(Last_Rt)=0 then
               Put_Line("Undefined label:" & Current_Token);
               Fail := True;
            end if;
         end loop;

         --Last_Rt_Max := Max(Last_Rt_Max, Last_Rt);

         loop
            X := Read_Nr;
            exit when X=0 or Fail;
            if X in 1..Nr_Vars_E then
               Inc(Stack_Last);
               Stack(Stack_Last) := Expr(Variable_Type(Used_Variables(First_Var(Last_E)+X-1)));

            elsif X in Nr_Vars_E+1..Nr_Vars_E+Nr_Hyp_E then
               Handle2(Hypotheses(First_Hyp(Last_E)+X-(Nr_Vars_E+1)));

            elsif X in Nr_Vars_E+Nr_Hyp_E+1..Nr_Vars_E+Nr_Hyp_E+Last_Rt then
               Handle2(Reffed_Th(X-(Nr_Vars_E+Nr_Hyp_E)));

            elsif X=Natural'Last then
               Inc(Last_Z);
               Z_Th(Last_Z) := Stack(Stack_Last);

            elsif X>Nr_Vars_E+Nr_Hyp_E+Last_Rt then
               Inc(Stack_Last);
               Stack(Stack_Last) := Z_Th(X-(Nr_Vars_E+Nr_Hyp_E+Last_Rt));
            end if;

         end loop;
         --Last_Z_Max := Max(Last_Z_Max, Last_Z);

         Skip_Proof; --read the "$."
      else
         Keep_Token := True;

         while Next_Token/= "$." loop

            X := Get_Label_Nr(Current_Token);
            if X=0 then
               Put_Line("Undefined label:" & Current_Token);
               Fail := True;
            end if;
            Handle2(X);

            exit when Fail;
         end loop;
      end if;

      --Show_Stack;

      if Fail or else not(Stack_Last=1 and then Stack(1)=Expr(Last_E)) then
         Put_Line("Failed to prove " & Get(Labels, Last_E).all & " : " & To_String(Expr(Last_E)) &
            " Line Nr " & Line_Nr);
         Keep_Token := True;
         Skip_Proof;
         --Wait_Key;
      end if;

      --      for I in Qwe'range loop
      --         if not Qwe(I) then
      --            Put_Line("In " & Get(Labels, Last_E).all & " $d not needed for " &
      --               P.Get(Variables, Natural(D1(I))).all & " and " &
      --               P.Get(Variables, Natural(D2(I))).all);
      --            Wait_Key;
      --         end if;
      --      end loop;

      Data_Last := Initial_Data_Last; -- Remove all temporary expressions created in the proof.
   end Check_Proof;

   Active_Hyps : Array_Of_Naturals(1..35); -- a stack
   Hyp_Last : Natural := 0;

   Blocks, Blocks2 : array (0..10) of Natural;
   B_Last : Natural := 0;

   --Max_Hyps : Natural := 0; -- Max used hypotheses.
   Ek : String(1..2);

   Dvar : array(1..100) of Integer_16; -- All variables between $d and $.
   Dvar_Last : Natural := 0;

   Beg_Dvar : Natural := 1;
   D_Last_Temp : Natural := Natural'Last;

   Vars : Used_Vars(1..Integer_16(Variables.Size));
   Nr_Vars : Integer := 0;

   T : Timer;
begin
   Start(T);

   --Use_Utf8 := True;
   --Open(File, In_File, "..\..\..\metamath\set2.mm");

   --while not End_Of_File(File) loop
   while not End_Of_File loop
      --Put_Line("loop top:" & Next_Token);
      if Next_Token="EOF" then -- "EOF" could be written in the infile to exit before the end.
         exit;
      elsif Current_Token="$c" then
         Define_Constants;

      elsif Current_Token="$v" then
         Define_Variables;

      elsif Current_Token="${" then
         Inc(B_Last);
         pragma Assert(B_Last in Blocks'range, "B_Last=" & B_Last);
         Blocks(B_Last) := Hyp_Last;
         Blocks2(B_Last) := Min(D_Last, D_Last_Temp);

      elsif Current_Token="$}" then
         Hyp_Last := Blocks(B_Last); -- cancel hypotheses in the block exited.
         D_Last_Temp := Blocks2(B_Last);
         Dec(B_Last);

      elsif Current_Token="$d" then
         if D_Last_Temp<D_Last then
            for I in 1..B_Last loop
               Add(Blocks2(I), D_Last+1-Beg_Dvar);
            end loop;
            D1(D_Last+1..D_Last+1+D_Last_Temp-Beg_Dvar) := D1(Beg_Dvar..D_Last_Temp);
            D2(D_Last+1..D_Last+1+D_Last_Temp-Beg_Dvar) := D2(Beg_Dvar..D_Last_Temp);
            D_Last := D_Last+1+D_Last_Temp-Beg_Dvar;
            Beg_Dvar := (D_Last-(1+D_Last_Temp-Beg_Dvar))+1;
         end if;

         Dvar_Last := 0;
         while Next_Token/="$." loop
            declare
               S : String_P := new String'(Current_Token);
               X : Natural;
            begin
               X := Get_Index(Variables, S);
               pragma Assert(X /=0, "Undefined variable");
               Free(S);
               Inc(Dvar_Last);
               Dvar(Dvar_Last) := Integer_16(X);
            end;
         end loop;

         for I in 1..Dvar_Last-1 loop
            for J in I+1..Dvar_Last loop
               for K in Beg_Dvar..D_Last loop
                  if D1(K)=Dvar(I) and D2(K)=Dvar(J) then
                     goto Continue;
                  end if;
               end loop;

               Inc(D_Last);
               D1(D_Last) := Dvar(I);
               D2(D_Last) := Dvar(J);
               <<Continue>> null;
            end loop;
         end loop;

         if D_Last_Temp<D_Last then
            D_Last_Temp := Natural'Last;
         end if;

      else -- label
         declare
            S : constant String_P := new String'(Current_Token);
         begin
            pragma Assert(Get_Index(Labels, S)=0, "label already defined");
            pragma Assert(S(S'First) /= '$', "#1");
            Add(Labels, S);
            --            Put_Line(S.All);
         end;

         Ek := Next_Token;

         pragma Assert(Ek="$f" or Ek="$e" or Ek="$a" or Ek="$p", "expected $feap");

         Ekind(Last_E) := Ek(2);

         -- $f is always of the form wff|set|class <variable> $. and in the outermost block
         -- $e always begins with "|-" and is contained in an inner block ( in set.mm )

         if Ek(2)='e' then
            Inc(Hyp_Last);
            Active_Hyps(Hyp_Last) := Last_E;
            --Max_Hyps := Max(Max_Hyps, Hyp_Last);

         elsif Ek(2)='a' or Ek(2)='p' then
            First_Hyp(Last_E) := H_Last+1;
            Last_Hyp(Last_E) := H_Last+Hyp_Last;

            for I in 1..Hyp_Last loop
               Hypotheses(H_Last+I) := Active_Hyps(I);
            end loop;
            Add(H_Last, Hyp_Last);

            First_Dvar(Last_E) := Beg_Dvar;
            Last_Dvar(Last_E) := Min(D_Last_Temp, D_Last);
         end if;

         Expr(Last_E).First := Data_Last+1;

         while Next_Token/= "$." and then Current_Token/= "$=" loop
            Inc(Data_Last);
            declare
               S : String_P := new String'(Current_Token);
               X : Integer_16;
            begin
               X := Integer_16(Get_Index(Constants, S));
               if X>0 then
                  Data(Data_Last) := X;
               else
                  X := Integer_16(Get_Index(Variables, S));
                  Assert(X /=0, "Undefined symbol:" & Current_Token);
                  Data(Data_Last) := -X;
               end if;
               Free(S);
            end;
         end loop;

         Expr(Last_E).Last := Data_Last;
         --Put_Line(To_String(Expr(Last_E)));

         if Ek(2)='f' then
            Variable_Type(-Data(Data_Last)) := Last_E;

         elsif Ek(2)='a' or Ek(2)='p' then

            Vars(1..Integer_16(P.Last(Variables))) := (others => False);

            Check_Variables(Last_E, Vars(1..Integer_16(P.Last(Variables))), Nr_Vars);

            for I in First_Hyp(Last_E)..Last_Hyp(Last_E) loop
               Check_Variables(Hypotheses(I), Vars, Nr_Vars);
            end loop;

            First_Var(Last_E) := V_Last+1;
            for I in 1..Integer_16(P.Last(Variables)) loop
               if Vars(I) then
                  Inc(V_Last);
                  Used_Variables(V_Last) := I;
                  for J in reverse First_Var(Last_E)..V_Last-1 loop
                     exit when Variable_Type(Used_Variables(J))<=Variable_Type(Used_Variables(J+1));
                     Swap(Used_Variables(J), Used_Variables(J+1));
                  end loop;
               end if;
            end loop;
            Last_Var(Last_E) := V_Last;
         end if;

         if Current_Token= "$=" then
            --Put_Line(Last_E & Get(Labels, Last_E).all);
            Check_Proof;
            --Skip_Proof;
         end if;
      end if;

   end loop;

   --Close(File);

   Stop(T);
   Put_Line(T);

   Put_Line(P.Last(Constants) & P.Last(Variables) & Last_E & Data_Last & Line_Nr & H_Last & D_Last & V_Last);
   --Put_Line(Max_Hyps & Max_Stack & Uv_Last_Max & Last_Rt_Max & Last_Z_Max & Nr_Hsv);
   --Wait_Key;
exception
   when others=>
      Put_Line(Line_Nr & ":" & Line(1..Last) & P1 & P2);
      --Close(File);
      raise;
end Metamath_Check;
