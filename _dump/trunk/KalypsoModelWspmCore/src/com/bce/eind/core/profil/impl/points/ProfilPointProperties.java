package com.bce.eind.core.profil.impl.points;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPointProperty;
import com.bce.eind.core.profil.IProfil.POINT_PROPERTY;


public class ProfilPointProperties
{
   public final static IProfilPointProperty BEWUCHS_AX = new ProfilPointProperty( "AX", true, true , false, 4,POINT_PROPERTY.BEWUCHS_AX);
 
   public  final static IProfilPointProperty BEWUCHS_AY = new ProfilPointProperty( "AY", true, true , false, 4 ,POINT_PROPERTY.BEWUCHS_AY);

   public  final static IProfilPointProperty BEWUCHS_DP = new ProfilPointProperty( "DP", true, true, false, 4 ,POINT_PROPERTY.BEWUCHS_DP );

   public  final static IProfilPointProperty BREITE = new ProfilPointProperty( "Breite", false, true, true, 4 ,POINT_PROPERTY.BREITE );

   public  final static IProfilPointProperty DURCHSTROEMTE = new ProfilPointProperty( "Durchströmte", false, false, false, 4 ,POINT_PROPERTY.DURCHSTROEMTE );

   public  final static IProfilPointProperty HOCHWERT = new ProfilPointProperty( "Hochwert", true, true, true, 4 ,POINT_PROPERTY.HOCHWERT );
   
   public  final static IProfilPointProperty UNTERKANTEBRUECKE = new ProfilPointProperty( "Brückenunterkante", true, true, false, 4,POINT_PROPERTY.UNTERKANTEBRUECKE  );
   
   public  final static IProfilPointProperty OBERKANTEBRUECKE = new ProfilPointProperty( "Brückenoberkante", true, true, false, 4 ,POINT_PROPERTY.OBERKANTEBRUECKE );

   public  final static IProfilPointProperty HOEHE = new ProfilPointProperty( "Höhe", false, true , true, 4 ,POINT_PROPERTY.HOEHE);

   public  final static IProfilPointProperty RAUHEIT = new ProfilPointProperty( "Rauheit", false, true, false, 4 ,POINT_PROPERTY.RAUHEIT );

   public  final static IProfilPointProperty RECHTSWERT = new ProfilPointProperty( "Rechtswert", true, true , true, 4 ,POINT_PROPERTY.TRENNFLAECHE);

   public  final static IProfilPointProperty TRENNFLAECHE = new ProfilPointProperty( "Trennefläche", false, false , false, 4,POINT_PROPERTY.TRENNFLAECHE );

   public  final static IProfilPointProperty BORDVOLL = new ProfilPointProperty ("Bordvollpunkt",true,true, false, 4 ,POINT_PROPERTY.BORDVOLL);
   
   public final static IProfilPointProperty getPointProperty(final IProfil.POINT_PROPERTY pointProperty)
   {
     switch( pointProperty )
     {
       case BEWUCHS_AX:
         return ProfilPointProperties.BEWUCHS_AX;
       case BEWUCHS_AY:
         return ProfilPointProperties.BEWUCHS_AY;
       case BEWUCHS_DP:
         return ProfilPointProperties.BEWUCHS_DP;
       case BREITE:
         return ProfilPointProperties.BREITE;
       case DURCHSTROEMTE:
         return ProfilPointProperties.DURCHSTROEMTE;
       case HOCHWERT:
         return ProfilPointProperties.HOCHWERT;
       case UNTERKANTEBRUECKE:
         return ProfilPointProperties.UNTERKANTEBRUECKE;
       case OBERKANTEBRUECKE:
         return ProfilPointProperties.OBERKANTEBRUECKE;
       case HOEHE:
         return ProfilPointProperties.HOEHE;
       case RAUHEIT:
         return ProfilPointProperties.RAUHEIT;
       case RECHTSWERT:
         return ProfilPointProperties.RECHTSWERT;
       case TRENNFLAECHE:
         return ProfilPointProperties.TRENNFLAECHE;
       case BORDVOLL:
         return ProfilPointProperties.BORDVOLL;
       default:
         return null;
     }
   }
}
