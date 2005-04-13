/*
 * Created on 14.03.2005
 */
package com.bce.eind.core.profil.util;

import java.util.Iterator;

import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.IProfilPointProperty;
import com.bce.eind.core.profil.ProfilDataException;

/**
 * @author kimwerner
 */
public class ProfilUtil
{
  public static final IProfilPoint splitSegment( IProfilPoint startPoint, IProfilPoint endPoint )
      throws ProfilDataException
  {
    if( (startPoint == null) | (endPoint == null) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final IProfilPoint point = startPoint.clonePoint();
    for( final Iterator<IProfilPointProperty> ppIt = point.getProperties().iterator(); ppIt.hasNext(); )
    {
      final IProfilPointProperty ppp = ppIt.next();
      if( ppp.isInterpolation() )
      {
        try
        {
          final double m_x = (startPoint.getValueFor( ppp ) + endPoint.getValueFor( ppp )) / 2.0;
          point.setValueFor( ppp, m_x );
        }
        catch( ProfilDataException e )
        {
          throw new ProfilDataException( "Fehler bei der Interpolation" );
        }
      }
    }
    return point;
  }
}
