package com.bce.eind.core.profil.impl.buildings;

import java.util.Arrays;
import java.util.LinkedList;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_PROPERTY;
import com.bce.eind.core.profil.IProfilConstants.BUILDING_TYP;
import com.bce.eind.core.profil.IProfilConstants.DEVIDER_TYP;
import com.bce.eind.core.profil.impl.PlainProfil;

public class WehrProfilBuilding extends AbstractProfilBuilding
{

  public WehrProfilBuilding( )
  {
    super( BUILDING_TYP.WEHR, Arrays.asList( BUILDING_PROPERTY.WEHRART ), new PointProperty[]
    { PointProperty.OBERKANTEWEHR } );
  }

  @Override
  public void addProfilProperties( PlainProfil profil ) throws ProfilDataException
  {
    super.addProfilProperties( profil );
    final LinkedList<IProfilPoint> points = profil.getPoints();
    for( IProfilPoint pt : points )
    {

      final double h = pt.getValueFor( PointProperty.HOEHE );
      profil.setValueFor( pt, PointProperty.OBERKANTEWEHR, h );

    }
    final IProfilDevider[] devider = profil.getDevider( DEVIDER_TYP.WEHR );
    if((devider == null)||( devider.length < 2 ))
      profil.addDevider( points.getLast(), DEVIDER_TYP.WEHR );
    if((devider == null)||( devider.length < 1 ))
      profil.addDevider( points.getFirst(), DEVIDER_TYP.WEHR );
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
