package com.bce.eind.core.profil.impl.buildings.building;

import java.util.Arrays;
import java.util.LinkedList;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.PlainProfil;

public class BuildingWehr extends AbstractProfilBuilding
{

  public BuildingWehr( )
  {
    super( BUILDING_TYP.WEHR, Arrays.asList( BUILDING_PROPERTY.WEHRART, BUILDING_PROPERTY.FORMBEIWERT ), new POINT_PROPERTY[] { POINT_PROPERTY.OBERKANTEWEHR } );
    m_buildingValues.put( BUILDING_PROPERTY.WEHRART, IProfil.WEHR_TYP.Beiwert );
  }

  @Override
  public void addProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.addProfilProperties( profil );
    final LinkedList<IProfilPoint> points = profil.getPoints();
    for( IProfilPoint pt : points )
    {

      final double h = pt.getValueFor( POINT_PROPERTY.HOEHE );
      pt.setValueFor( POINT_PROPERTY.OBERKANTEWEHR, h );

    }
  }

  @Override
  public void removeProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.removeProfilProperties( profil );
    final IProfilDevider[] devider = profil.getDevider( DEVIDER_TYP.WEHR );
    final int deviderCount = devider == null ? 0 : devider.length;
    for( int i = 0; i < deviderCount; i++ )
    {
      profil.removeDevider( devider[i] );
    }
  }

}
