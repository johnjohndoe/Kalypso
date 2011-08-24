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
package org.kalypso.model.wspm.tuhh.ui.panel.roughness;

import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.kalypso.commons.java.util.AbstractModelObject;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.observation.result.IComponent;

/**
 * @author Dirk Kuch
 */
public class ProfileRoguhnessesDataModel extends AbstractModelObject
{
  public static final String PROPERTY_LEFT_FLOODPLAIN = "leftFloodplain"; //$NON-NLS-1$

  public static final String PROPERTY_RIGHT_FLOODPLAIN = "rightFloodplain"; //$NON-NLS-1$

  public static final String PROPERTY_RIVER_TUBE = "riverTube"; //$NON-NLS-1$

  Double m_riverTube;

  Double m_leftFloodplain;

  Double m_rightFloodplain;

  private final IProfil m_profile;

  private final IComponent m_roughness;

  public ProfileRoguhnessesDataModel( final IProfil profile, final IComponent roughness )
  {
    m_profile = profile;
    m_roughness = roughness;

    init( profile, roughness );
  }

  private void init( final IProfil profile, final IComponent roughness )
  {
    m_leftFloodplain = RoughnessFlowzones.findLeftFloodplainValue( profile, roughness );
    m_rightFloodplain = RoughnessFlowzones.findRightFloodplainValue( profile, roughness );
    m_riverTube = RoughnessFlowzones.findRiverTubeValue( profile, roughness );

  }

  public IObservableValue getObservableValue( final String property )
  {
    return BeansObservables.observeValue( this, property );
  }

  public Double getLeftFloodplain( )
  {
    return m_leftFloodplain;
  }

  public Double getRightFloodplain( )
  {
    return m_rightFloodplain;
  }

  public Double getRiverTube( )
  {
    return m_riverTube;
  }

  public void setLeftFloodplain( final Double leftFloodplain )
  {
    final Object oldValue = m_leftFloodplain;

    m_leftFloodplain = leftFloodplain;

    RoughnessFlowzones.setLeftFloodplain( m_profile, m_roughness, leftFloodplain );

    firePropertyChange( PROPERTY_LEFT_FLOODPLAIN, oldValue, leftFloodplain );

  }

  public void setRightFloodplain( final Double rightFloodplain )
  {
    final Object oldValue = m_rightFloodplain;

    m_rightFloodplain = rightFloodplain;

    RoughnessFlowzones.setRightFloodplain( m_profile, m_roughness, rightFloodplain );

    firePropertyChange( PROPERTY_RIGHT_FLOODPLAIN, oldValue, rightFloodplain );
  }

  public void setRiverTube( final Double riverTube )
  {
    final Object oldValue = m_riverTube;

    m_riverTube = riverTube;

    RoughnessFlowzones.setRiverTube( m_profile, m_roughness, riverTube );

    firePropertyChange( PROPERTY_RIVER_TUBE, oldValue, riverTube );
  }
}
