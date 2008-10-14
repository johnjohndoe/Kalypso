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
package org.kalypsodeegree_impl.model.feature;

import javax.xml.namespace.QName;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;
import org.deegree.model.spatialschema.GeometryException;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;

/**
 * A Feature implementation wich delegates all calls to another feature, proved by a feature provider.
 * <p>
 * Everything is delegated to the provided feature, except #getParent (because this freature stil lives in its own
 * structure).
 * </p>
 * <p>
 * Cannot be used as workspace root.
 * </p>
 * 
 * @author Gernot Belger
 */
public class XLinkedFeature_Impl extends AbstractFeature
{
  private final Feature m_parentFeature;

  private final IRelationType m_parentRelation;

  private final String m_uri;

  private String m_featureId;

  private final String m_role;

  private final String m_arcrole;

  private final String m_title;

  private final String m_show;

  private final String m_actuate;

  private final IFeatureType m_basicFeatureType;

  private IFeatureType m_featureType;

  public XLinkedFeature_Impl( final Feature parentFeature, final IRelationType parentRelation, final IFeatureType featureType, final String href, final String role, final String arcrole, final String title, final String show, final String actuate )
  {
    m_parentFeature = parentFeature;
    m_parentRelation = parentRelation;
    m_basicFeatureType = featureType;
    m_role = role;
    m_arcrole = arcrole;
    m_title = title;
    m_show = show;
    m_actuate = actuate;

    if( !href.contains( "#" ) )
    {
      m_uri = null;
      m_featureId = null;
    }
    else if( href.startsWith( "#" ) )
    {
      m_uri = null;
      m_featureId = href.substring( 1 );
    }
    else
    {
      final String[] hrefParts = href.split( "#" );
      if( hrefParts.length == 2 )
      {
        m_uri = hrefParts[0];
        m_featureId = hrefParts[1];
      }
      else
      {
        m_uri = null;
        m_featureId = null;
      }
    }

    if( m_parentFeature == null )
      throw new IllegalArgumentException( "XLinked Feature must have parent feature: " + m_parentFeature );
  }

  /** Returns the linked feature. */
  public final Feature getFeature( )
  {
    final GMLWorkspace workspace = m_parentFeature.getWorkspace();
    if( workspace == null || m_featureId == null )
    {
      // REMARK: This may happen while loading the gml, so we ignore it and all access to
      // getFeature() should check for null
      return null;
    }

    final GMLWorkspace linkedWorkspace = workspace.getLinkedWorkspace( m_uri );
    final Feature feature = linkedWorkspace == null ? null : linkedWorkspace.getFeature( m_featureId );

    if( feature == null )
      throw new IllegalStateException( "No feature found at: " + m_uri + "#" + m_featureId );

    /* The first time we access the real feature, get our real feature type. */
    if( m_featureType == null )
      m_featureType = feature.getFeatureType();

    return feature;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getId()
   */
  public String getId( )
  {
    // return null in order to let the workspace generate internal ids
    return null;

    // do not access the provider/feature, because else we already access the remote workspace while loading
    // the old one, this leading to dead-locks
    // final IFeatureProvider provider = getProvider( getWorkspace() );
    // return provider.getId();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    /* As long as the feature was not accessed, we only know the target feature type of our defining property. */

    if( m_featureType != null )
      return m_featureType;

    return m_basicFeatureType;
  }

  /**
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperties()
   */
  @Deprecated
  public Object[] getProperties( )
  {
    final Feature feature = getFeature();
    return feature.getProperties();
  }

  /**
   * format of name if "namespace:name" or just "name" - both will work
   * 
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  public Object getProperty( final IPropertyType pt )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;

    return feature.getProperty( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getGeometryProperties()
   */
  public GM_Object[] getGeometryProperties( )
  {
    return getGeometryPropertyValues();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getDefaultGeometryProperty()
   */
  public GM_Object getDefaultGeometryProperty( )
  {
    return getDefaultGeometryPropertyValue();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getEnvelope()
   */
  public GM_Envelope getEnvelope( )
  {
    try
    {
      return getBoundedBy();
    }
    catch( GeometryException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  public void setProperty( final IPropertyType pt, final Object value )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.setProperty( pt, value );
  }

  /**
   * @deprecated use getProperty(IPropertyType)
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  @Deprecated
  public Object getProperty( final String propNameLocalPart )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;
    return feature.getProperty( propNameLocalPart );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(javax.xml.namespace.QName)
   */
  public Object getProperty( final QName propQName )
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;
    return feature.getProperty( propQName );
  }

  /**
   * @deprecated
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  @Deprecated
  public void setProperty( final String propLocalName, final Object value )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.setProperty( propLocalName, value );
  }

  /**
   * Returns the workspace of the linked feature.
   * 
   * @see org.kalypsodeegree.model.feature.Feature#getWorkspace()
   */
  public GMLWorkspace getWorkspace( )
  {
    return m_parentFeature.getWorkspace();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParent()
   */
  public Feature getParent( )
  {
    return getOwner();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setWorkspace(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void setWorkspace( final GMLWorkspace workspace )
  {
    // nothing to do, a delegated feature is never a root feature
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final Feature feature = getFeature();
    return feature == null ? "null" : FeatureHelper.getAnnotationValue( feature, IAnnotation.ANNO_LABEL );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(javax.xml.namespace.QName, java.lang.Object)
   */
  public void setProperty( final QName propQName, final Object value )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.setProperty( propQName, value );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#invalidEnvelope()
   */
  public void invalidEnvelope( )
  {
    setEnvelopesUpdated();
  }

  public String getHref( )
  {
    if( m_uri == null )
      return "#" + m_featureId;

    return m_uri + "#" + m_featureId;
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    if( !(obj instanceof XLinkedFeature_Impl) )
      return false;

    final XLinkedFeature_Impl other = (XLinkedFeature_Impl) obj;
    return new EqualsBuilder().append( m_uri, other.m_uri ).append( m_featureId, other.m_featureId ).isEquals();
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    return new HashCodeBuilder().append( m_uri ).append( m_featureId ).toHashCode();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParentRelation()
   */
  public IRelationType getParentRelation( )
  {
    return m_parentRelation;
  }

  public String getActuate( )
  {
    return m_actuate;
  }

  public String getArcrole( )
  {
    return m_arcrole;
  }

  public String getRole( )
  {
    return m_role;
  }

  public String getShow( )
  {
    return m_show;
  }

  public String getTitle( )
  {
    return m_title;
  }

  public String getUri( )
  {
    return m_uri;
  }

  public String getFeatureId( )
  {
    return m_featureId;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getBoundedBy()
   */
  public GM_Envelope getBoundedBy( ) throws GeometryException
  {
    final Feature feature = getFeature();
    if( feature == null )
      return null;

    return feature.getBoundedBy();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getDefaultGeometryPropertyValue()
   */
  public GM_Object getDefaultGeometryPropertyValue( )
  {
    return getFeature().getDefaultGeometryPropertyValue();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getGeometryPropertyValues()
   */
  public GM_Object[] getGeometryPropertyValues( )
  {
    return getFeature().getGeometryPropertyValues();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getOwner()
   */
  public Feature getOwner( )
  {
    return m_parentFeature;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getQualifiedName()
   */
  public QName getQualifiedName( )
  {
    return getFeatureType().getQName();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setEnvelopesUpdated()
   */
  public void setEnvelopesUpdated( )
  {
    final Feature feature = getFeature();
    if( feature != null )
      feature.invalidEnvelope();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setFeatureType(org.kalypso.gmlschema.feature.IFeatureType)
   */
  public void setFeatureType( IFeatureType ft )
  {
    m_featureType = ft;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setId(java.lang.String)
   */
  public void setId( String fid )
  {
    m_featureId = fid;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    return NamedFeatureHelper.getName( getFeature() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    NamedFeatureHelper.setName( getFeature(), name );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( getFeature() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( getFeature(), desc );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getLocation()
   */
  public GM_Object getLocation( )
  {
    Object property = getFeature().getProperty( NamedFeatureHelper.GML_LOCATION );
    if( property instanceof GM_Object )
      return (GM_Object) property;

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setLocation(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public void setLocation( final GM_Object location )
  {
    getFeature().setProperty( NamedFeatureHelper.GML_LOCATION, location );
  }
}