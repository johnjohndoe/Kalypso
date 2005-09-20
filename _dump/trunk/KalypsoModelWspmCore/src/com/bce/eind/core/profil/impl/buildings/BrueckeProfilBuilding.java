package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;
import java.util.LinkedList;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;
import com.bce.eind.core.profil.impl.PlainProfil;

/**
 * @author kimwerner
 */
public class BrueckeProfilBuilding extends AbstractProfilBuilding
{
  public BrueckeProfilBuilding( )
  {
    super( BUILDING_TYP.BRUECKE, Arrays.asList( BUILDING_PROPERTY.BREITE,
        BUILDING_PROPERTY.UNTERWASSER, BUILDING_PROPERTY.PFEILERFORM, BUILDING_PROPERTY.RAUHEIT ),
        new PointProperty[]
        { PointProperty.UNTERKANTEBRUECKE, PointProperty.OBERKANTEBRUECKE } );
  }

  @Override
  /**
   * erzeugt die verknüpften Objekte des Bauwerks im Profil und setzt sie auf einen default Wert
   */
  public void addProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.addProfilProperties(profil);
    final LinkedList<IProfilPoint> points = profil.getPoints();
   
 
    for( IProfilPoint pt : points )
    {
      final double h = pt.getValueFor( PointProperty.HOEHE );
      profil.setValueFor( pt, PointProperty.OBERKANTEWEHR, h );
    }

  }

  
}
