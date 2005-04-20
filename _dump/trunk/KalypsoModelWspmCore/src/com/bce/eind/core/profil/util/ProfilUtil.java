/*
 * Created on 14.03.2005
 */
package com.bce.eind.core.profil.util;

import java.util.Iterator;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.points.ProfilPoint;
import com.bce.eind.core.profil.impl.points.ProfilPointProperties;

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
    for( final Iterator<IProfil.POINT_PROPERTY> ppIt = point.getProperties().iterator(); ppIt.hasNext(); )
    {
      final IProfil.POINT_PROPERTY ppp = ppIt.next();
      if(ProfilPointProperties.getPointProperty(ppp).isInterpolation() )
      {
        try
        {
          final double m_x = (startPoint.getValueFor( ppp ) + endPoint.getValueFor( ppp )) / 2.0;
          ((ProfilPoint)point).setValueFor( ppp, m_x );
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
