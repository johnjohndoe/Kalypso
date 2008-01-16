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
package org.kalypsodeegree_impl.gml.binding.commons;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * Abstract helper class to implement 'binding' classes for specific feature (types).
 * 
 * @author Gernot Belger
 */
public class AbstractFeatureBinder
{

  /**
   * Check if this feature is suitable for the given qname. If true, the constructor wont complain.
   */
  public static final boolean checkFeature( final Feature feature, final QName qname )
  {
    return GMLSchemaUtilities.substitutes( feature.getFeatureType(), qname );
  }

  private final Feature m_featureToBind;

  private final QName m_qnameToBind;

  public AbstractFeatureBinder( final Feature featureToBind, final QName qnameToBind )
  {
    m_featureToBind = featureToBind;
    m_qnameToBind = qnameToBind;

    if( !checkFeature( featureToBind, qnameToBind ) )
    {
      final String msg = String.format( "featureToBind (%s) does not substitute %s", featureToBind.getFeatureType().getQName(), qnameToBind );
      throw new IllegalArgumentException( msg );
    }
  }

  /**
   * TODO: remove
   */
  public Feature getFeature( )
  {
    return m_featureToBind;
  }

  /**
   * TODO: rename to getFeature
   * 
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getWrappedFeature( )
  {
    return m_featureToBind;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper#getGmlID()
   */
  public String getGmlID( )
  {
    return getFeature().getId();
  }

  public QName getQname( )
  {
    return m_qnameToBind;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    return NamedFeatureHelper.getName( m_featureToBind );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    NamedFeatureHelper.setName( m_featureToBind, name );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( m_featureToBind );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( m_featureToBind, desc );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getLocation()
   */
  public GM_Object getLocation( )
  {
    return getProperty( NamedFeatureHelper.GML_LOCATION, GM_Object.class );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setLocation(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public void setLocation( final GM_Object location )
  {
    setProperty( NamedFeatureHelper.GML_LOCATION, location );
  }

  /**
   * Two bound features are equal if and only if their corresponding features are equal, and this is the case iff they
   * have the same id and live in the same workspace.
   * 
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object other )
  {
    if( other instanceof AbstractFeatureBinder )
    {
      final AbstractFeatureBinder otherBinder = (AbstractFeatureBinder) other;
      final Feature otherFeature = otherBinder.getFeature();

      if( m_featureToBind.getId().equals( otherFeature.getId() ) && m_featureToBind.getWorkspace() == otherFeature.getWorkspace() )
        return true;
    }

    return false;
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return new HashCodeBuilder().append( m_featureToBind.getId() ).append( m_featureToBind.getWorkspace() ).toHashCode();
  }

  /**
   * TODO: also make helper to adapt to given class if it is not yet of the given type Returns the property of bind
   * feature given the property {@link QName}
   * 
   * @param propertyQName
   *            the {@link QName} of the property to get.
   */
  protected <T> T getProperty( final QName propertyQName, final Class<T> propClass )
  {
    final Object prop = m_featureToBind.getProperty( propertyQName );
    try
    {
      return (T) prop;
    }
    catch( final ClassCastException e )
    {
      throw new RuntimeException( "Property of type[" + propClass + "] expected " + "\n\tbut found this type :" + prop.getClass() );
    }
  }

  protected void setProperty( final QName propertyQName, final Object newValue )
  {
    m_featureToBind.setProperty( propertyQName, newValue );
  }

}