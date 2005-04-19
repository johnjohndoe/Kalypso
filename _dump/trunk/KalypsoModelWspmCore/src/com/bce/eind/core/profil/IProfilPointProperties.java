package com.bce.eind.core.profil;

import com.bce.eind.core.profil.impl.points.ProfilPointProperty;


public interface IProfilPointProperties
{
   public final static IProfilPointProperty BEWUCHS_AX = new ProfilPointProperty( "AX", true, true , false, 4 );
 
   public  final static IProfilPointProperty BEWUCHS_AY = new ProfilPointProperty( "AY", true, true , false, 4 );

   public  final static IProfilPointProperty BEWUCHS_DP = new ProfilPointProperty( "DP", true, true, false, 4  );

   public  final static IProfilPointProperty BREITE = new ProfilPointProperty( "Breite", false, true, true, 4  );

   public  final static IProfilPointProperty DURCHSTROEMTE = new ProfilPointProperty( "Durchstr�mte", false, false, false, 4  );

   public  final static IProfilPointProperty HOCHWERT = new ProfilPointProperty( "Hochwert", true, true, true, 4  );
   
   public  final static IProfilPointProperty UNTERKANTEBRUECKE = new ProfilPointProperty( "Br�ckenunterkante", true, true, false, 4  );
   
   public  final static IProfilPointProperty OBERKANTEBRUECKE = new ProfilPointProperty( "Br�ckenoberkante", true, true, false, 4  );

   public  final static IProfilPointProperty HOEHE = new ProfilPointProperty( "H�he", false, true , true, 4 );

   public  final static IProfilPointProperty RAUHEIT = new ProfilPointProperty( "Rauheit", false, true, false, 4  );

   public  final static IProfilPointProperty RECHTSWERT = new ProfilPointProperty( "Rechtswert", true, true , true, 4 );

   public  final static IProfilPointProperty TRENNFLAECHE = new ProfilPointProperty( "Trennefl�che", false, false , false, 4 );

   public  final static IProfilPointProperty BORDVOLL = new ProfilPointProperty ("Bordvollpunkt",true,true, false, 4 );
}
