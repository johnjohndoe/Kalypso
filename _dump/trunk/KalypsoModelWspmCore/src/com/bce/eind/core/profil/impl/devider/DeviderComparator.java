package com.bce.eind.core.profil.impl.devider;

import java.util.Comparator;

import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilDataException;

public class DeviderComparator implements Comparator<IProfilDevider>
{

  public int compare( IProfilDevider o1, IProfilDevider o2 )
  {

    try
    {

      if( o1.getPoint().getValueFor( PointProperty.BREITE ) > o2.getPoint().getValueFor(
          PointProperty.BREITE ) )
      {
        return 1;
      }
      if( o1.getPoint().getValueFor( PointProperty.BREITE ) < o2.getPoint().getValueFor(
          PointProperty.BREITE ) )
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
