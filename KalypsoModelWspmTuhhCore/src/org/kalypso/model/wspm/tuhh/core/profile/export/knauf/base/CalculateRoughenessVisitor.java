/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base;

import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecordVisitor;
import org.kalypso.model.wspm.core.profil.wrappers.ProfileRecord;
import org.kalypso.model.wspm.tuhh.core.profile.export.knauf.KnaufReach;

/**
 * @author Dirk Kuch
 */
public class CalculateRoughenessVisitor implements IProfileRecordVisitor
{
  double m_ksValues = 0.0;

  double m_kstValues = 0.0;

  int m_pointsKsValues = 0;

  int m_pointsKstValues = 0;

  private final double m_segmentStart;

  private final double m_segmentEnd;

  Set<IProfileRecord> m_points = new LinkedHashSet<>();

  public CalculateRoughenessVisitor( final double segmentStart, final double segmentEnd )
  {
    m_segmentStart = segmentStart;
    m_segmentEnd = segmentEnd;
  }

  @Override
  public void visit( final IProfileRecord point, final int searchDirection )
  {
    if( !isBetween( point ) )
      return;

    m_points.add( point );

    final Double ksValue = point.getKsValue();
    if( Objects.isNotNull( ksValue ) && !Double.isNaN( ksValue ) )
    {
      m_ksValues += ksValue;
      m_pointsKsValues += 1;
    }

    final Double kstValue = point.getKstValue();
    if( Objects.isNotNull( kstValue ) && !Double.isNaN( kstValue ) )
    {
      m_kstValues += kstValue;
      m_pointsKstValues += 1;
    }
  }

  private boolean isBetween( final IProfileRecord point )
  {
    final double breite = point.getBreite();
    if( breite < m_segmentStart || breite >= m_segmentEnd )
      return false;

    return true;
  }

  private double getDistance( final IProfileRecord... points )
  {
    if( ArrayUtils.getLength( points ) == 1 )
      return 0.0;

    final IProfileRecord p1 = points[0];
    final IProfileRecord p2 = points[ArrayUtils.getLength( points ) - 1];

    return Math.abs( p2.getBreite() - p1.getBreite() );
  }

  public Double getRoughness( final KnaufReach reach )
  {
    if( m_points.size() == 0 )
      return 0.0;

    final KNAUF_FLIESSGESETZ fliessgesetz = reach.getFliessgesetz();

    final IProfileRecord[] points = m_points.toArray( new ProfileRecord[] {} );

    final double distance = getDistance( points );
    if( distance == 0.0 )
      points[0].getKsValue();

    double base = 0.0;

    for( int index = 0; index < points.length - 1; index++ )
    {
      final IProfileRecord p1 = points[index];
      final IProfileRecord p2 = points[index + 1];

      final double d = getDistance( p1, p2 );

      final Double value = fliessgesetz.getRoughnessValue( p1 );

      base += d * value;
    }

    final double roughness = base / distance;
    if( Double.isNaN( roughness ) )
      return 0.0;

    return roughness;
  }

  @Override
  public boolean isWriter( )
  {
    return false;
  }
}
