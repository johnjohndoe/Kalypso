/*
 * Created on 14.03.2005
 */
package com.bce.eind.core.profil.util;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import com.bce.eind.core.profil.IProfil;
import com.bce.eind.core.profil.IProfilDevider;
import com.bce.eind.core.profil.IProfilPoint;
import com.bce.eind.core.profil.ProfilDataException;
import com.bce.eind.core.profil.IProfilDevider.DEVIDER_TYP;
import com.bce.eind.core.profil.IProfilPoint.PARAMETER;
import com.bce.eind.core.profil.IProfilPoint.POINT_PROPERTY;
import com.bce.eind.core.profil.impl.points.ProfilPoint;

/**
 * @author kimwerner
 */
public class ProfilUtil
{
  public static final List<IProfilPoint> getInnerPoints(final IProfil profil,final DEVIDER_TYP deviderTyp)
  {
    final IProfilDevider[] deviders = profil.getDevider(deviderTyp);
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final int leftPos = (deviders.length > 1)? points.indexOf(deviders[0].getPoint()):0;
    final int rightPos = (deviders.length > 1)? points.indexOf(deviders[deviders.length -1].getPoint())+1:0;
    return (leftPos < rightPos)?points.subList(leftPos,rightPos):null;

  }
  public static final IProfilPoint splitSegment( IProfilPoint startPoint, IProfilPoint endPoint )
      throws ProfilDataException
  {
    if( (startPoint == null) | (endPoint == null) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final IProfilPoint point = startPoint.clonePoint();
    for( final Iterator<POINT_PROPERTY> ppIt = point.getProperties().iterator(); ppIt.hasNext(); )
    {
      final POINT_PROPERTY ppp = ppIt.next();
      if( (Boolean)ppp.getParameter( PARAMETER.INTERPOLATION ) )
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
        if( point.getValueFor( (POINT_PROPERTY.BREITE) ) == breite )
          return point;
      }

      return profil.findPoint( breite, delta );

    }
    catch( ProfilDataException e )
    {
      // sollte nie passieren da Breite als Eigenschaft immer existiert
    }
    return null;
  }

  public static IProfilPoint getPointBefore( final IProfil profil, IProfilPoint point )
      throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( point == points.getFirst() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new ProfilDataException( "Punkt nicht im Profil: " + point );

    return points.get( i - 1 );
  }

  public static IProfilPoint getPointAfter( final IProfil profil, final IProfilPoint point )
      throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( point == points.getLast() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new ProfilDataException( "Punkt nicht im Profil: " + point );

    return points.get( i + 1 );
  }

}
