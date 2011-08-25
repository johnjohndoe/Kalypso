/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
import org.kalypso.model.wspm.core.IWspmPointProperties;
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

  private final IComponent m_component;

  public static final String PROPERTY_LEFT_FLOODPLAIN_CLASS = "leftFloodplainClass"; //$NON-NLS-1$

  public static final String PROPERTY_RIGHT_FLOODPLAIN_CLASS = "rightFloodplainClass"; //$NON-NLS-1$

  public static final String PROPERTY_RIVER_TUBE_CLASS = "riverTubeClass"; //$NON-NLS-1$

  String m_riverTubeClass;

  String m_leftFloodplainClass;

  String m_rightFloodplainClass;

  public ProfileRoguhnessesDataModel( final IProfil profile, final IComponent component )
  {
    m_profile = profile;
    m_component = component;

    init( profile, component );
  }

  private void init( final IProfil profile, final IComponent component )
  {
    /* ks or kst value? */
    if( isSimpleRoughnessType( component ) )
    {
      m_leftFloodplain = RoughnessFlowzones.findLeftFloodplainValue( profile, component );
      m_rightFloodplain = RoughnessFlowzones.findRightFloodplainValue( profile, component );
      m_riverTube = RoughnessFlowzones.findRiverTubeValue( profile, component );
    }
    else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_CLASS.equals( component.getId() ) )
    {
      m_leftFloodplainClass = RoughnessFlowzones.findLeftFloodplainClass( profile, component );
      m_rightFloodplainClass = RoughnessFlowzones.findRightFloodplainClass( profile, component );
      m_riverTubeClass = RoughnessFlowzones.findRiverTubeClass( profile, component );
    }
  }

  private boolean isSimpleRoughnessType( final IComponent component )
  {
    if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KS.equals( component.getId() ) )
      return true;
    else if( IWspmPointProperties.POINT_PROPERTY_RAUHEIT_KST.equals( component.getId() ) )
      return true;
    else if( IWspmPointProperties.POINT_PROPERTY_ROUGHNESS_FACTOR.equals( component.getId() ) )
      return true;

    return false;
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

    RoughnessFlowzones.setLeftFloodplain( m_profile, m_component, leftFloodplain );

    firePropertyChange( PROPERTY_LEFT_FLOODPLAIN, oldValue, leftFloodplain );

  }

  public void setRightFloodplain( final Double rightFloodplain )
  {
    final Object oldValue = m_rightFloodplain;
    m_rightFloodplain = rightFloodplain;

    RoughnessFlowzones.setRightFloodplain( m_profile, m_component, rightFloodplain );

    firePropertyChange( PROPERTY_RIGHT_FLOODPLAIN, oldValue, rightFloodplain );
  }

  public void setRiverTube( final Double riverTube )
  {
    final Object oldValue = m_riverTube;
    m_riverTube = riverTube;

    RoughnessFlowzones.setRiverTube( m_profile, m_component, riverTube );

    firePropertyChange( PROPERTY_RIVER_TUBE, oldValue, riverTube );
  }

  public String getRiverTubeClass( )
  {
    return m_riverTubeClass;
  }

  public void setRiverTubeClass( final String riverTubeClass )
  {
    final Object oldValue = m_riverTubeClass;
    m_riverTubeClass = riverTubeClass;

    RoughnessFlowzones.setRiverTube( m_profile, m_component, riverTubeClass );

    firePropertyChange( PROPERTY_RIVER_TUBE_CLASS, oldValue, riverTubeClass );
  }

  public String getLeftFloodplainClass( )
  {
    return m_leftFloodplainClass;
  }

  public void setLeftFloodplainClass( final String leftFloodplainClass )
  {
    final Object oldValue = m_leftFloodplainClass;
    m_leftFloodplainClass = leftFloodplainClass;

    RoughnessFlowzones.setLeftFloodplain( m_profile, m_component, leftFloodplainClass );

    firePropertyChange( PROPERTY_LEFT_FLOODPLAIN_CLASS, oldValue, leftFloodplainClass );
  }

  public String getRightFloodplainClass( )
  {
    return m_rightFloodplainClass;
  }

  public void setRightFloodplainClass( final String rightFloodplainClass )
  {
    final Object oldValue = m_rightFloodplainClass;
    m_rightFloodplainClass = rightFloodplainClass;

    RoughnessFlowzones.setRightFloodplain( m_profile, m_component, rightFloodplainClass );

    firePropertyChange( PROPERTY_RIGHT_FLOODPLAIN_CLASS, oldValue, rightFloodplainClass );
  }

}
