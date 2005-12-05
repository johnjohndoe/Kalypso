package com.bce.eind.core.profil.impl.buildings.building;

import java.util.Arrays;
import java.util.LinkedList;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

/**
 * @author kimwerner
 */
public class BuildingBruecke extends AbstractProfilBuilding
{
  public BuildingBruecke( )
  {
    super( BUILDING_TYP.BRUECKE, Arrays.asList( BUILDING_PROPERTY.BREITE,
        BUILDING_PROPERTY.UNTERWASSER, BUILDING_PROPERTY.PFEILERFORM, BUILDING_PROPERTY.RAUHEIT ),
        new POINT_PROPERTY[]
        { POINT_PROPERTY.UNTERKANTEBRUECKE, POINT_PROPERTY.OBERKANTEBRUECKE } );
  }

  @Override
  /**
   * erzeugt die verknüpften Objekte des Bauwerks im Profil und setzt sie auf einen default Wert
   */
  public void addProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.addProfilProperties( profil );
    final LinkedList<IProfilPoint> points = profil.getPoints();

    for( final IProfilPoint pt : points )
    {
      final double h = pt.getValueFor( POINT_PROPERTY.HOEHE );
      pt.setValueFor( POINT_PROPERTY.OBERKANTEBRUECKE, h );
      pt.setValueFor( POINT_PROPERTY.UNTERKANTEBRUECKE, h );
    }

  }

}
