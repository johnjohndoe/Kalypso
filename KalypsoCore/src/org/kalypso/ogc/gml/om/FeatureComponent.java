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

import org.kalypso.gmlschema.property.restriction.IRestriction;
import org.kalypso.gmlschema.swe.RepresentationType;
import org.kalypso.observation.phenomenon.DictionaryPhenomenon;
import org.kalypso.observation.phenomenon.FeaturePhenomenon;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.AbstractComponent;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;
import org.kalypsodeegree_impl.model.feature.binding.NamedFeatureHelper;

/**
 * A component wich was previously read from a feature.
 * 
 * @author schlienger
 */
public class FeatureComponent extends AbstractComponent
{
  private final Feature m_itemDef;

  public FeatureComponent( final Feature itemDef )
  {
    m_itemDef = itemDef;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( obj instanceof FeatureComponent )
    {
      return m_itemDef.equals( ((FeatureComponent) obj).m_itemDef );
    }

    return false;
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getDefaultValue()
   */
  public Object getDefaultValue( )
  {
    // REMARK: The ItemDefinition has no notion of default value, so for now we always return null.
    // TODO: Maybe we can define a meaningful policy what to return as a default value (for example: first value of an
    // enumeration)
    return null;
  }

  public String getDescription( )
  {
    if( m_itemDef == null )
    {
      return "";
    }
    return NamedFeatureHelper.getDescription( m_itemDef );
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getFrame()
   */
  public String getFrame( )
  {
    return getRepresentationType().getFrame();
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getId()
   */
  public String getId( )
  {
    if( m_itemDef instanceof XLinkedFeature_Impl )
    {
      return ((XLinkedFeature_Impl) m_itemDef).getHref();
    }

    return m_itemDef.getId();
  }

  public Feature getItemDefinition( )
  {
    return m_itemDef;
  }

  public String getName( )
  {
    if( m_itemDef == null )
    {
      return "<no name>";
    }
    return NamedFeatureHelper.getName( m_itemDef );
  }

  public IPhenomenon getPhenomenon( )
  {
    final Object phenomProperty = m_itemDef.getProperty( ObservationFeatureFactory.SWE_PROPERTY );
    if( phenomProperty instanceof String )
    {
      return new DictionaryPhenomenon( (String) phenomProperty, null, null );
    }
    else if( phenomProperty instanceof XLinkedFeature_Impl )
    {
      final String name = NamedFeatureHelper.getName( (Feature) phenomProperty );
      final String description = NamedFeatureHelper.getDescription( (Feature) phenomProperty );
      return new DictionaryPhenomenon( ((XLinkedFeature_Impl) phenomProperty).getHref(), name, description );
    }
    else if( phenomProperty instanceof Feature )
    {
      return new FeaturePhenomenon( (Feature) phenomProperty );
    }

    return null;
  }

  public RepresentationType getRepresentationType( )
  {
    return (RepresentationType) m_itemDef.getProperty( ObservationFeatureFactory.SWE_REPRESENTATION );
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getUnit()
   */
  public String getUnit( )
  {
    return getRepresentationType().getUnit();
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getValueTypeName()
   */
  public QName getValueTypeName( )
  {
    return getRepresentationType().getValueTypeName();
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return m_itemDef.hashCode();
  }

  /**
   * @see org.kalypso.observation.result.IComponent#getRestrictions()
   */
  public IRestriction[] getRestrictions( )
  {
    return getRepresentationType().getRestrictions();
  }
}
