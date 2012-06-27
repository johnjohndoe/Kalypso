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
package org.kalypso.model.wspm.tuhh.core.profile.export.knauf.base;

import org.kalypso.commons.exception.CancelVisitorException;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecord;
import org.kalypso.model.wspm.core.profil.wrappers.IProfileRecordVisitor;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;

/**
 * @author Dirk Kuch
 */
public class FindBridgeBorderVisitor implements IProfileRecordVisitor
{
  private final String m_property;

  double m_min = Double.MAX_VALUE;

  double m_max = -Double.MAX_VALUE;

  public FindBridgeBorderVisitor( final String property )
  {
    m_property = property;
  }

  @Override
  public void visit( final IProfileRecord point, final int searchDirection ) throws CancelVisitorException
  {
    final int index = point.getProfile().indexOfProperty( m_property );
    if( index < 0 )
      throw new CancelVisitorException();

    final Double hoehe = point.getHoehe();
    final Double border = getValue( point, index );

    if( Doubles.isNaN( hoehe, border ) )
      return;

    if( !isRelevant( hoehe, border ) )
      return;

    if( border < m_min )
      m_min = border;

    if( border > m_max )
      m_max = border;
  }

  private Double getValue( final IProfileRecord point, final int index )
  {
    final Object value = point.getValue( index );
    if( value instanceof Number )
      return ((Number) value).doubleValue();

    return null;
  }

  private boolean isRelevant( final Double a, final Double b )
  {
    if( Doubles.isNaN( a, b ) )
      return false;

    if( IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE.equals( m_property ) )
      return true;

    return Math.abs( a - b ) > 0.005;
  }

  public double getBorder( )
  {
    if( Double.MAX_VALUE == m_min )
      return 0.0;
    if( -Double.MAX_VALUE == m_max )
      return 0.0;

    return (m_min + m_max) / 2.0;
  }

  @Override
  public boolean isWriter( )
  {
    return false;
  }
}
