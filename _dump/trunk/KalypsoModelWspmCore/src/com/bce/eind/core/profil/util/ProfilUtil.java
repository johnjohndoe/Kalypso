/*
 * Created on 14.03.2005
 */
package com.bce.eind.core.profil.util;

import java.util.Iterator;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.PointProperty;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.impl.points.ProfilPoint;

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
    for( final Iterator<PointProperty> ppIt = point.getProperties().iterator(); ppIt
        .hasNext(); )
    {
      final PointProperty ppp = ppIt.next();
       if( (Boolean)ppp.getParameter(PointProperty.PARAMETER.INTERPOLATION))
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

  /**
   * findet einen Punkt in einem Profil 1.hole Punkt[index] und vergleiche Punkt.breite mit breite ->
   * 2.suche Punkt bei breite mit einer Toleranz von delta 3.kein Punkt gefunden -> (return null)
   */
  public final IProfilPoint findPoint( final IProfil profil, final int index, final double breite,
      final double delta )
  {
    final IProfilPoint point = profil.getPoints().get( index );
    try
    {
      if( point != null )
      {
        if( point.getValueFor( (PointProperty.BREITE) ) == breite )
          return point;
      }

      return profil.findPoint( breite, delta );
      
    }
    catch( ProfilDataException e )
    {
      //sollte nie passieren da Breite als Eigenschaft immer existiert
    }
    return null;
  }
 
}
