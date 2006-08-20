/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.model.wspm.core.profil.util;

import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;

import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilDevider;
import org.kalypso.model.wspm.core.profil.IProfilPoint;
import org.kalypso.model.wspm.core.profil.ProfilDataException;
import org.kalypso.model.wspm.core.profil.IProfilDevider.DEVIDER_TYP;
import org.kalypso.model.wspm.core.profil.IProfilPoint.PARAMETER;
import org.kalypso.model.wspm.core.profil.IProfilPoint.POINT_PROPERTY;
import org.kalypso.model.wspm.core.profil.impl.points.ProfilPoint;

/**
 * @author kimwerner
 */
public class ProfilUtil
{
  public static final IProfilPoint getProfilPoint( final IProfil profil, final IProfilPoint pointBefore, final IProfilPoint pointAfter )
  {
    if( profil == null )
      return null;
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final IProfilPoint leftP = ((pointBefore == null) && (!points.isEmpty())) ? points.getFirst() : pointBefore;
    try
    {
      final IProfilPoint rightP = (pointAfter == null) ? getPointAfter( profil, leftP ) : pointAfter;
      return splitSegment( leftP, rightP );
    }

    catch( ProfilDataException e )
    {
      // no Point available
      return null;
    }
  }

  public static final List<IProfilPoint> getInnerPoints( final IProfil profil, final DEVIDER_TYP deviderTyp )
  {
    final IProfilDevider[] deviders = profil.getDevider( deviderTyp );
    final LinkedList<IProfilPoint> points = profil.getPoints();
    final int leftPos = (deviders.length > 1) ? points.indexOf( deviders[0].getPoint() ) : 0;
    final int rightPos = (deviders.length > 1) ? points.indexOf( deviders[deviders.length - 1].getPoint() ) + 1 : 0;
    return (leftPos < rightPos) ? points.subList( leftPos, rightPos ) : null;

  }

  public static final List<IProfilPoint> getInnerPoints( final IProfil profil, final IProfilDevider leftDevider, final IProfilDevider rightDevider )
  {

    final LinkedList<IProfilPoint> points = profil.getPoints();
    final int leftPos = (leftDevider != null) ? points.indexOf( leftDevider.getPoint() ) : 0;
    final int rightPos = (rightDevider != null) ? points.indexOf( rightDevider.getPoint() )  : 0;
    return (leftPos < rightPos) ? points.subList( leftPos, rightPos ) : null;

  }

  /**
   * return true if all selected properties are equal
   */
  public static final boolean comparePoints( final POINT_PROPERTY property, final IProfilPoint point1, final IProfilPoint point2 )
  {
    try
    {
      return Math.abs( point1.getValueFor( property ) - point2.getValueFor( property ) ) <= (Double) property.getParameter( PARAMETER.PRECISION );
    }
    catch( ProfilDataException e )
    {
      return false;
    }
  }

  /**
   * return true if getValueFor(property) is equal
   */
  public static final boolean comparePoints( final POINT_PROPERTY[] properties, final IProfilPoint point1, final IProfilPoint point2 )
  {
    for( POINT_PROPERTY property : properties )
    {
      if( !comparePoints( property, point1, point2 ) )
      {
        return false;
      }
    }
    return true;
  }

  public static final IProfilPoint splitSegment( IProfilPoint startPoint, IProfilPoint endPoint ) throws ProfilDataException
  {
    if( (startPoint == null) | (endPoint == null) )
      throw new ProfilDataException( "Profilpunkt existiert nicht" );
    final IProfilPoint point = startPoint.clonePoint();
    for( final Iterator<POINT_PROPERTY> ppIt = point.getProperties().iterator(); ppIt.hasNext(); )
    {
      final POINT_PROPERTY ppp = ppIt.next();
      if( (Boolean) ppp.getParameter( PARAMETER.INTERPOLATION ) )
      {
        try
        {
          final double m_x = (startPoint.getValueFor( ppp ) + endPoint.getValueFor( ppp )) / 2.0;
          ((ProfilPoint) point).setValueFor( ppp, m_x );
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
   * findet einen Punkt in einem Profil 1.hole Punkt[index] und vergleiche Punkt.breite mit breite -> 2.suche Punkt bei
   * breite mit einer Toleranz von delta 3.kein Punkt gefunden -> (return null)
   */
  public final IProfilPoint findPoint( final IProfil profil, final int index, final double breite, final double delta )
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

  public static IProfilPoint getPointBefore( final IProfil profil, IProfilPoint point ) throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() || point == points.getFirst() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new ProfilDataException( "Punkt nicht im Profil: " + point );

    return points.get( i - 1 );
  }

  public static IProfilPoint getPointAfter( final IProfil profil, final IProfilPoint point ) throws ProfilDataException
  {
    final LinkedList<IProfilPoint> points = profil.getPoints();
    if( points.isEmpty() || point == points.getLast() )
      return null;

    final int i = points.indexOf( point );
    if( i == -1 )
      throw new ProfilDataException( "Punkt nicht im Profil: " + point );

    return points.get( i + 1 );
  }

}
