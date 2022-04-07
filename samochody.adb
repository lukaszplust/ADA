with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO;
with Ada.Numerics.Discrete_Random;
with Ada.Numerics;
use Ada.Numerics;
with Ada.Strings.Unbounded;
use Ada.Strings.Unbounded;

procedure Samochody is




   Liczba_Produktow: constant Integer := 5;
	Liczba_Aut: constant Integer := 3;
   Liczba_Klientow: constant Integer := 2;

   Czas_Oczekiwania_Klienta: constant Integer := 15;
	Czas_Oczekiwania_Czesci: constant Integer := 10;

   Pojemnosc_Magazynu: constant Integer := 10;

   subtype Zakres_Czasu_Produkcji is Integer range 8 .. 24;
   subtype Zakres_Czasu_Uzytkowania is Integer range 10 .. 15;

   subtype Zakres_Produkcji is Integer range 1 .. Liczba_Produktow;
   subtype Zakres_Aut is Integer range 1 .. Liczba_Aut;
   subtype Zakres_Klientow is Integer range 1 .. Liczba_Klientow;

   type Typ_Magazynu is array(Zakres_Produkcji) of Integer;

   Nazwa_Produktu: constant array (Zakres_Produkcji) of String(1 .. 7)
     := ("Drzwi  ", "Szyba  ", "Maska  ", "Fotel  ", "Hamulec");

	Nazwa_Zestawu: constant array (Zakres_Aut) of String(1 .. 7)
     := ("Porshe ", "Bmw    ", "Mustang");

	Nazwa_Konsumenta: constant array(Zakres_Klientow) of String(1 .. 5)
	:= ("Jakub", "Marek");








	Magazyn: Typ_Magazynu := (0, 0, 0, 0, 0);
   Sklad_Zestawu: array(Zakres_Aut, Zakres_Produkcji) of Integer
     := ((2, 1, 2, 1, 2),
        (2, 2, 0, 1, 0),
        (1, 1, 2, 0, 1));
   Numer_Zestawu: array(Zakres_Aut) of Integer := (1, 1, 1);



   package Losowe_Uzytkowanie is new Discrete_Random(Zakres_Czasu_Uzytkowania);
	package Losowy_Zestaw is new Discrete_Random(Zakres_Aut);



   task type TProducent is
		entry Zacznij(Produkcje: in Zakres_Produkcji; Czas_Produkcji: in Integer);
	end TProducent;

	task type TKonsument is
		entry Zacznij(Numer_Konsumenta: in Zakres_Klientow; Czas_Uzytkowania: in Integer);
	end TKonsument;

	task type TBufor is
		entry Przyjmij(Produkcje: in Zakres_Produkcji; Numer: in Integer; Czy_Przyjeto: out Boolean);
		entry Wydaj(Zestaw: in Zakres_Aut; Numer: out Integer);
   end TBufor;


   Producenci: array(1 .. Liczba_Produktow) of TProducent;
	Klienci: array(1 .. Liczba_Klientow) of TKonsument;
	Bufor: TBufor;




     task body TProducent is
		package Losowa_Produkcja is new Discrete_Random(Zakres_Czasu_Produkcji);

		Generator: Losowa_Produkcja.Generator;
		Numer_Produktu: Integer := 1;

		Nr_Typu_Produktu: Integer;
		Produkcja: Integer;
		Przyjeto: Boolean := False;

   begin
		accept Zacznij(Produkcje: in Zakres_Produkcji; Czas_Produkcji: in Integer) do
			Losowa_Produkcja.Reset(Generator);

			Nr_Typu_Produktu := Produkcje;
			Produkcja := Czas_Produkcji;
		end Zacznij;

		Put_Line("P [info]: ZACZETO PRODUKCJE: " & Nazwa_Produktu(Nr_Typu_Produktu));

		loop
			delay Duration(Losowa_Produkcja.Random(Generator));
			Put_Line("P [info]: WYPRODUKOWANO: " & Nazwa_Produktu(Nr_Typu_Produktu) & " numer " & Integer'Image(Numer_Produktu));

			loop
				Bufor.Przyjmij(Nr_Typu_Produktu, Numer_Produktu, Przyjeto);
				if Przyjeto = False then
					Put_Line("P [warn]: Brak miejsca na czesci " & Nazwa_Produktu(Nr_Typu_Produktu) & ". Czekam 5 sekund");

					delay Duration(5.0);
				else
					Numer_Produktu := Numer_Produktu + 1;
				end if;
				exit;
			end loop;
		end loop;
   end TProducent;



   task body TKonsument is
		Generator: Losowe_Uzytkowanie.Generator;
		Generator2: Losowy_Zestaw.Generator;

		Nr_Konsumenta: Zakres_Klientow;
		Numer_Zestawu: Integer;
		Uzytkowanie: Integer;
		Rodzaj_Zestawu: Integer;

	begin
		accept Zacznij(Numer_Konsumenta: in Zakres_Klientow; Czas_Uzytkowania: in Integer) do
			Losowe_Uzytkowanie.Reset(Generator);
			Losowy_Zestaw.Reset(Generator2);

			Nr_Konsumenta := Numer_Konsumenta;
			Uzytkowanie := Czas_Uzytkowania;
		end Zacznij;

		loop
			Put_Line("K [info]: PRZYSZEDL: klient " & Nazwa_Konsumenta(Nr_Konsumenta));
			delay Duration(Losowe_Uzytkowanie.Random(Generator));

         Rodzaj_Zestawu := Losowy_Zestaw.Random(Generator2);

			select
				delay Duration(Czas_Oczekiwania_Klienta);
				Put_Line("K [info]: WYCHODZI: " & Nazwa_Konsumenta(Nr_Konsumenta) & ". Nie jest zainteresowany");
			then abort
				Put_Line("K [info]: Klient " & Nazwa_Konsumenta(Nr_Konsumenta) & " chce kupic " & Nazwa_Zestawu(Rodzaj_Zestawu));
				loop
					Bufor.Wydaj(Rodzaj_Zestawu, Numer_Zestawu);
					if Numer_Zestawu = 0 then
						Put_Line("K [warn]: Brak czesci samochodowych " & Nazwa_Zestawu(Rodzaj_Zestawu) & " dla klienta " & Nazwa_Konsumenta(Nr_Konsumenta) & ". Czekam 5 sekund");
						delay Duration(5.0);
					else
						Put_Line("K [info]: " & Nazwa_Konsumenta(Nr_Konsumenta) & " kupil " & Nazwa_Zestawu(Rodzaj_Zestawu) & " #" & Integer'Image(Numer_Zestawu));
                  Put_Line("K [info]: WYCHODZI: " & Nazwa_Konsumenta(Nr_Konsumenta) & ".");

						exit;
					end if;
				end loop;
			end select;

			delay Duration(Losowe_Uzytkowanie.Random(Generator));
		end loop;
	end TKonsument;

   task body TBufor is
		W_Magazynie: Integer := 0;

		Licznik_Nie_Przyjeto: Integer := 0;
		Max_Produkt: Integer := 0;
		Max_Produkt_Rodzaj: Zakres_Produkcji := 1;


		function Mozna_Przyjac(Produkcje: Zakres_Produkcji) return Boolean is
			Wolne: Integer;
			Tmp_Mag: Typ_Magazynu;
			Brak: array(Zakres_Aut, Zakres_Produkcji) of Integer;
			Max_Brak: array(Zakres_Produkcji) of Integer;
			Braki: Integer;
		begin
			if W_Magazynie >= Pojemnosc_Magazynu then
				return False;
			else
				Wolne := Pojemnosc_Magazynu - W_Magazynie;
				Tmp_Mag := Magazyn;
				Tmp_Mag(Produkcje) := Tmp_Mag(Produkcje) + 1;

				for P in Zakres_Produkcji
				loop
					Max_Brak(P) := 0;
				end loop;

				for A in Zakres_Aut
				loop
					for P in Zakres_Produkcji
					loop
						Brak(A, P) := Integer'Max(0, Sklad_Zestawu(A, P) - Tmp_Mag(P));

						if Brak(A, P) > Max_Brak(P) then
							Max_Brak(P) := Brak(A, P);
						end if;
					end loop;
				end loop;
			end if;

			Braki := 0;

			for P in Zakres_Produkcji
			loop
				Braki := Braki + Max_Brak(P);
			end loop;

			return Wolne >= Braki;
		end Mozna_Przyjac;


		function Mozna_Wydac(Zestaw: Zakres_Aut) return Boolean is
		begin
			for P in Zakres_Produkcji loop
				if Magazyn(P) < Sklad_Zestawu(Zestaw, P) then
					return False;
				end if;
			end loop;

			return True;
		end Mozna_Wydac;


		procedure Sklad_Magazynu is
			Str: Unbounded_String;
		begin
			Append(Str, "B [info]: Zawartosc miejsca na czesci: ");
			for P in Zakres_Produkcji loop
				Append(Str, Integer'Image(Magazyn(P)) & ", ");

				if P = Liczba_Produktow then
					Append(Str, ASCII.LF);
				end if;
			end loop;

			Put_Line(To_String(Str));
		end Sklad_Magazynu;

		procedure Usun_Czesc(Produkt: in Zakres_Produkcji) is
		begin
			Magazyn(Produkt) := Magazyn(Produkt) - 1;
			W_Magazynie := W_Magazynie - 1;
		end Usun_Czesc;

	begin
		Put_Line("B [info]: Zaczeto Bufor" & ASCII.LF);
		loop
			Put_Line("B [info]: Czekam na zamówienie...");
			select
				accept Wydaj(Zestaw: in Zakres_Aut; Numer: out Integer) do
					if Mozna_Wydac(Zestaw) then
						Put_Line("B [info]: WYDANO: " & Nazwa_Zestawu(Zestaw) & " nr " & Integer'Image(Numer_Zestawu(Zestaw)));

						for P in Zakres_Produkcji loop
							Magazyn(P) := Magazyn(P) - Sklad_Zestawu(Zestaw, P);
							W_Magazynie := W_Magazynie - Sklad_Zestawu(Zestaw, P);
						end loop;

						Numer := Numer_Zestawu(Zestaw);
						Numer_Zestawu(Zestaw) := Numer_Zestawu(Zestaw) + 1;
					else
						Numer := 0;
					end if;
				end Wydaj;

			or delay Duration(5.0);
				Put_Line("B [info]: Brak zamówien. Zajmuje sie czyms innym");
				accept Przyjmij(Produkcje: in Zakres_Produkcji; Numer: in Integer; Czy_Przyjeto: out Boolean) do
					if Mozna_Przyjac(Produkcje) then
						Magazyn(Produkcje) := Magazyn(Produkcje) + 1;
						W_Magazynie := W_Magazynie + 1;
						Czy_Przyjeto := True;
						Put_Line("B [info]: PRZYJETO: " & Nazwa_Produktu(Produkcje) & " na polke");
						Licznik_Nie_Przyjeto := 0;
					else
						Czy_Przyjeto := False;

						Licznik_Nie_Przyjeto := Licznik_Nie_Przyjeto + 1;
						if Licznik_Nie_Przyjeto >= 3 then
							Put_Line("B [warn]: Nie mozna dodac czesci 3-ci raz. Mozliwe zakleszczenie. Usuwam czesc, której jest wiecej niz pozostale.");

							Max_Produkt := 0;
							Max_Produkt_Rodzaj := 1;
							for P in Zakres_Produkcji
							loop
								if Magazyn(P) > Magazyn(Max_Produkt_Rodzaj) then
									Max_Produkt := Magazyn(P);
									Max_Produkt_Rodzaj := P;
								end if;
							end loop;
							Usun_Czesc(Max_Produkt_Rodzaj);
							Licznik_Nie_Przyjeto := 0;
							Sklad_Magazynu;
						end if;
					end if;
				end Przyjmij;

			end select;
			Sklad_Magazynu;
		end loop;
	end TBufor;

begin

	for i in 1 .. Liczba_Produktow
	loop
		Producenci(i).Zacznij(i, 10);
	end loop;

	for j in 1 .. Liczba_Klientow
	loop
		Klienci(j).Zacznij(j, 12);
	end loop;
end Samochody;






