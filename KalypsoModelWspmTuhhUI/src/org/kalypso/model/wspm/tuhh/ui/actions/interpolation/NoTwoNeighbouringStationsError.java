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
package org.kalypso.model.wspm.tuhh.ui.actions.interpolation;

import java.math.BigDecimal;
import java.util.SortedMap;
import java.util.TreeMap;

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.databinding.validation.ValidationStatus;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class NoTwoNeighbouringStationsError implements IValidator
{
  private final SortedMap<BigDecimal, IProfileFeature> m_stations = new TreeMap<>();

  public NoTwoNeighbouringStationsError( final IProfileFeature[] profiles )
  {
    for( final IProfileFeature profile : profiles )
    {
      final BigDecimal bigStation = profile.getBigStation();
      m_stations.put( bigStation, profile );
    }
  }

  private BigDecimal getFirstStation( )
  {
    return m_stations.firstKey();
  }

  private BigDecimal getLastStation( )
  {
    return m_stations.lastKey();
  }

  public IProfileFeature getPreviousProfile( final BigDecimal station )
  {
    if( station == null )
      return null;

    final SortedMap<BigDecimal, IProfileFeature> headMap = m_stations.headMap( station );
    if( headMap.isEmpty() )
      return null;

    return m_stations.get( headMap.lastKey() );
  }

  public IProfileFeature getNextProfile( final BigDecimal station )
  {
    if( station == null )
      return null;

    final SortedMap<BigDecimal, IProfileFeature> tailMap = m_stations.tailMap( station );
    if( tailMap.isEmpty() )
      return null;

    return m_stations.get( tailMap.firstKey() );
  }

  /**
   * @see org.eclipse.core.databinding.validation.IValidator#validate(java.lang.Object)
   */
  @Override
  public IStatus validate( final Object value )
  {
    final BigDecimal station = (BigDecimal) value;
    if( station == null )
      return ValidationStatus.error( Messages.getString( "NoTwoNeighbouringStationsError.0" ) ); //$NON-NLS-1$

    final IProfileFeature prevProfile = getPreviousProfile( station );

    final IProfileFeature nextProfile = getNextProfile( station );

    if( prevProfile == null || nextProfile == null )
    {
      final String msg = Messages.getString( "NoTwoNeighbouringStationsError_0", getFirstStation(), getLastStation() ); //$NON-NLS-1$
      return ValidationStatus.error( msg );
    }

    return ValidationStatus.ok();
  }

}
