package org.kalypso.model.wspm.core.profil.impl.devider;

import java.util.Comparator;

import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;


public class DeviderComparator implements Comparator<IProfilDevider>
{

  public int compare( IProfilDevider o1, IProfilDevider o2 )
  {

    try
    {

      if( o1.getPoint().getValueFor( POINT_PROPERTY.BREITE ) > o2.getPoint().getValueFor(
          POINT_PROPERTY.BREITE ) )
      {
        return 1;
      }
      if( o1.getPoint().getValueFor( POINT_PROPERTY.BREITE ) < o2.getPoint().getValueFor(
          POINT_PROPERTY.BREITE ) )
      {
        return -1;
      }
      return 0;
    }
    catch( ProfilDataException e )
    {
      return 0;
    }
  }

}
