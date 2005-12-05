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
    super( BUILDING_TYP.WEHR, Arrays.asList( BUILDING_PROPERTY.WEHRART ), new POINT_PROPERTY[]
    { POINT_PROPERTY.OBERKANTEWEHR } );

    try
    {
      setValue( BUILDING_PROPERTY.WEHRART, IProfil.WEHR_TYP.Beiwert );
    }
    catch( ProfilDataException e )
    {
      // do nothing
    }
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
    final IProfilDevider[] devider = profil.getDevider( DEVIDER_TYP.DURCHSTROEMTE );
    final IProfilDevider leftDev = profil.addDevider( points.getFirst(), DEVIDER_TYP.WEHR );
    final IProfilDevider rightDev = profil.addDevider( points.getLast(), DEVIDER_TYP.WEHR );
    if( (devider != null) && (devider.length > 0) )
      profil.moveDevider( leftDev, devider[0].getPoint() );
    if( (devider != null) && (devider.length > 1) )
      profil.moveDevider( rightDev, devider[1].getPoint() );
    leftDev.setValueFor( IProfilDevider.DEVIDER_PROPERTY.BEIWERT, 0.0 );
    rightDev.setValueFor( IProfilDevider.DEVIDER_PROPERTY.BEIWERT, 0.0 );
  }

  @Override
  public void removeProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.removeProfilProperties( profil );
    final IProfilDevider[] devider = profil.getDevider( DEVIDER_TYP.WEHR );
    if( devider.length < 1 )
    {
      return;
    }
    for( IProfilDevider dev : devider )
    {
      profil.removeDevider( dev );
    }

  }

}
