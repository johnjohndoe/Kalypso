package com.bce.eind.core.profil.impl.points.pointproperties;

import com.bce.eind.core.profil.IProfilPointProperty;

public class ProfilPointProperties
{
   public final  IProfilPointProperty BEWUCHS_AX = new ProfilPointProperty( "AX", true, true , false, 4 );
 
   public  final IProfilPointProperty BEWUCHS_AY = new ProfilPointProperty( "AY", true, true , false, 4 );

   public  final IProfilPointProperty BEWUCHS_DP = new ProfilPointProperty( "DP", true, true, false, 4  );

   public  final IProfilPointProperty BREITE = new ProfilPointProperty( "Breite", false, true, true, 4  );

   public  final IProfilPointProperty DURCHSTROEMTE = new ProfilPointProperty( "Durchstr�mte", false, false, false, 4  );

   public  final IProfilPointProperty HOCHWERT = new ProfilPointProperty( "Hochwert", true, true, true, 4  );
   
   public  final IProfilPointProperty UNTERKANTEBRUECKE = new ProfilPointProperty( "Br�ckenunterkante", true, true, false, 4  );
   
   public  final IProfilPointProperty OBERKANTEBRUECKE = new ProfilPointProperty( "Br�ckenoberkante", true, true, false, 4  );

   public  final IProfilPointProperty HOEHE = new ProfilPointProperty( "H�he", false, true , true, 4 );

   public  final IProfilPointProperty RAUHEIT = new ProfilPointProperty( "Rauheit", false, true, false, 4  );

   public  final IProfilPointProperty RECHTSWERT = new ProfilPointProperty( "Rechtswert", true, true , true, 4 );

   public  final IProfilPointProperty TRENNFLAECHE = new ProfilPointProperty( "Trennefl�che", false, false , false, 4 );

   public  final IProfilPointProperty BORDVOLL = new ProfilPointProperty ("Bordvollpunkt",true,true, false, 4 );
}
