/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.gml.binding.commons;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.HashCodeBuilder;
import org.eclipse.core.runtime.Assert;
import org.eclipse.core.runtime.IAdaptable;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Object;

/**
 * Abstract helper class to implement 'binding' classes for specific feature (types).
 * 
 * @author Gernot Belger
 */
public abstract class AbstractFeatureBinder implements IFeatureWrapper2
{
  private final Feature m_featureToBind;

  private final QName m_qnameToBind;

  public AbstractFeatureBinder( final Feature featureToBind, final QName qnameToBind )
  {
    m_featureToBind = featureToBind;
    m_qnameToBind = qnameToBind;

    // REMARK: first check, then create error message... Always producing the error message costs very much performance!
    if( !GMLSchemaUtilities.substitutes( featureToBind.getFeatureType(), qnameToBind ) )
    {
      final String msg = String.format( "featureToBind (%s) does not substitute %s", featureToBind.getFeatureType().getQName(), qnameToBind );
      Assert.isLegal( false, msg );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper#getWrappedFeature()
   */
  public Feature getFeature( )
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
   * feature given the property {@link QName}
   * 
   * @param propertyQName
   *            the {@link QName} of the property to get.
   */
  @SuppressWarnings("unchecked")
  protected <T> T getProperty( final QName propertyQName, final Class<T> propClass )
  {
    final Object prop = m_featureToBind.getProperty( propertyQName );
    try
    {
      if( prop == null )
        return null;

      if( propClass.isAssignableFrom( prop.getClass() ) )
        return (T) prop;

      if( prop instanceof IAdaptable )
        return (T) ((IAdaptable) prop).getAdapter( propClass );

      throw new RuntimeException( "Property of type[" + propClass + "] expected " + "\n\tbut found this type :" + prop.getClass() );
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