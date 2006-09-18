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
package org.kalypso.ogc.gml.om;

import javax.xml.namespace.QName;

import org.kalypso.observation.result.IComponent;
import org.kalypso.ogc.swe.RepresentationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * A component wich was previously read from a feature.
 * 
 * @author schlienger
 */
public class FeatureComponent implements IComponent
{
  private final Feature m_itemDef;

  public FeatureComponent( final Feature itemDef )
  {
    m_itemDef = itemDef;
  }

  private Feature getPhenomenon( )
  {
    final Feature phenomenon = FeatureHelper.resolveLink( m_itemDef, ObservationFeatureFactory.SWE_PROPERTY );
    return phenomenon;
  }
  
  /**
   * @see org.kalypso.observation.result.IComponent#getId()
   */
  public String getId( )
  {
    if( m_itemDef instanceof XLinkedFeature_Impl )
      return ((XLinkedFeature_Impl)m_itemDef).getHref();
    
    return m_itemDef.getId();
  }
  
  public String getName( )
  {
    final Feature phenomenon = getPhenomenon();
    return (String) FeatureHelper.getFirstProperty( phenomenon, ObservationFeatureFactory.GML_NAME );
  }

  public String getDescription( )
  {
    final Feature phenomenon = getPhenomenon();
    return (String) FeatureHelper.getFirstProperty( phenomenon, ObservationFeatureFactory.GML_DESCRIPTION );
  }
  
  /**
   * @see org.kalypso.observation.result.IComponent#getUnit()
   */
  public String getUnit( )
  {
    return getRepresentationType().getUnit();
  }
  
  /**
   * @see org.kalypso.observation.result.IComponent#getFrame()
   */
  public String getFrame( )
  {
    return getRepresentationType().getFrame();
  }

  public RepresentationType getRepresentationType( )
  {
    return (RepresentationType) m_itemDef.getProperty( ObservationFeatureFactory.SWE_REPRESENTATION );
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getValueTypeName()
   */
  public QName getValueTypeName( )
  {
    return getRepresentationType().getValueTypeName();
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getDefaultValue()
   */
  public Object getDefaultValue( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  public Feature getItemDefinition( )
  {
    return m_itemDef;
  }
}
