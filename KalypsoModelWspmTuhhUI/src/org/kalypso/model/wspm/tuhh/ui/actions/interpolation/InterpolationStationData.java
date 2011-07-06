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

import org.eclipse.core.databinding.beans.BeanProperties;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class InterpolationStationData extends AbstractModelObject
{
  private static final String STR_NOT_FOUND = Messages.getString( "InterpolationStationPage_0" ); //$NON-NLS-1$

  public static final String PROPERTY_ONLY_CHANNEL = "onlyChannel"; //$NON-NLS-1$

  public static final String PROPERTY_STATION = "station"; //$NON-NLS-1$

  private static final String PROPERTY_PREV_LABEL = "previousLabel"; //$NON-NLS-1$

  private static final String PROPERTY_NEXT_LABEL = "nextLabel"; //$NON-NLS-1$

  private boolean m_onlyRiverChannel = true;

  private BigDecimal m_station;

  private IProfileFeature m_prevProfile;

  private IProfileFeature m_nextProfile;

  private final IProfileFeature[] m_profiles;

  private final NoTwoNeighbouringStationsError m_neighbourRule;

  public InterpolationStationData( final IProfileFeature[] profiles )
  {
    m_profiles = profiles;
    m_neighbourRule = new NoTwoNeighbouringStationsError( m_profiles );
  }

  public void setOnlyChannel( final boolean selection )
  {
    m_onlyRiverChannel = selection;
  }

  public boolean getOnlyChannel( )
  {
    return m_onlyRiverChannel;
  }

  public void setStation( final BigDecimal station )
  {
    final BigDecimal oldStation = m_station;
    final String oldPrevLabel = getPreviousLabel();
    final String oldNextLabel = getNextLabel();

    m_station = station;

    m_prevProfile = m_neighbourRule.getPreviousProfile( station );
    m_nextProfile = m_neighbourRule.getNextProfile( station );

    firePropertyChange( PROPERTY_STATION, oldStation, station );
    firePropertyChange( PROPERTY_PREV_LABEL, oldPrevLabel, getPreviousLabel() );
    firePropertyChange( PROPERTY_NEXT_LABEL, oldNextLabel, getNextLabel() );
  }

  public BigDecimal getStation( )
  {
    return m_station;
  }

  public IProfileFeature getPreviousProfile( )
  {
    return m_prevProfile;
  }

  public String getPreviousLabel( )
  {
    if( m_prevProfile == null )
      return STR_NOT_FOUND;

    return m_prevProfile.getBigStation().toString();
  }

  public IProfileFeature getNextProfile( )
  {
    return m_nextProfile;
  }

  public String getNextLabel( )
  {
    if( m_nextProfile == null )
      return STR_NOT_FOUND;

    return m_nextProfile.getBigStation().toString();
  }

  public BigDecimal getNewStation( )
  {
    return m_station;
  }

  public IProfileFeature[] getProfiles( )
  {
    return m_profiles;
  }

  public IObservableValue observeStation( )
  {
    return BeanProperties.value( getClass(), PROPERTY_STATION ).observe( this );
  }

  public IObservableValue observeNextLabel( )
  {
    return BeanProperties.value( getClass(), PROPERTY_NEXT_LABEL ).observe( this );
  }

  public IObservableValue observePrevLabel( )
  {
    return BeanProperties.value( getClass(), PROPERTY_PREV_LABEL ).observe( this );
  }

  public IObservableValue observeOnlyChannel( )
  {
    return BeanProperties.value( getClass(), PROPERTY_ONLY_CHANNEL ).observe( this );
  }
}
