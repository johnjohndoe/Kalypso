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

import java.util.ArrayList;
import java.util.List;

import javax.xml.namespace.QName;

import org.deegree.model.spatialschema.GeometryException;
import org.eclipse.core.runtime.IAdaptable;
import org.eclipse.core.runtime.PlatformObject;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.IFeaturePropertyHandler;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;
import org.kalypsodeegree_impl.model.geometry.GM_Envelope_Impl;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

/**
 * Implementation of ogc feature
 *
 * @author doemming
 */
public class Feature_Impl extends PlatformObject implements Feature
{
  private final static GM_Envelope INVALID_ENV = new GM_Envelope_Impl();

  /**
   * all property-values are stored here in sequential order (as defined in application-schema) properties with
   * maxOccurency = 1 are stored direct properties with maxOccurency > 1 are stored in a list properties with
   * maxOccurency = "unbounded" should use FeatureCollections
   */
  private final Object[] m_properties;

  private final IFeatureType m_featureType;

  private final String m_id;

  private Object m_parent = null;

  private final IRelationType m_parentRelation;

  private GM_Envelope m_envelope = Feature_Impl.INVALID_ENV;

  protected Feature_Impl( final Object parent, final IRelationType parentRelation, final IFeatureType ft, final String id, final Object[] propValues )
  {
    if( ft == null )
      throw new UnsupportedOperationException( "must provide a featuretype" );

    m_parent = parent;

    m_parentRelation = parentRelation;
    m_featureType = ft;
    m_id = id;
    m_properties = propValues;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getId()
   */
  public String getId( )
  {
    return m_id;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    return m_featureType;
  }

  /**
   * @return array of properties, properties with maxoccurency>0 (as defined in applicationschema) will be embedded in
   *         java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperties()
   */
  @SuppressWarnings("deprecation")
  @Deprecated
  public Object[] getProperties( )
  {
    return m_properties;
  }

  /**
   * Accesses a property value of this feature.
   *
   * @return Value of the given properties. Properties with maxoccurency > 0 (as defined in applicationschema) will be
   *         embedded in java.util.List-objects
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  public Object getProperty( final IPropertyType pt )
  {
    if( pt == null )
      throw new IllegalArgumentException( "pt may not null" );

    final int pos = m_featureType.getPropertyPosition( pt );
    final IFeaturePropertyHandler fsh = getPropertyHandler();
    if( pos == -1 && !fsh.isFunctionProperty( pt ) )
    {
      final String msg = String.format( "Unknown property (%s) for type: %s", pt, m_featureType );
      throw new IllegalArgumentException( msg );
    }

    final Object currentValue = pos == -1 ? null : m_properties[pos];

    return fsh.getValue( this, pt, currentValue );
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
    catch( final GeometryException e )
    {
      e.printStackTrace();

      return null;
    }
  }

  private void calculateEnv( )
  {
    GM_Envelope env = null;
    final GM_Object[] geoms = getGeometryProperties();
    for( final GM_Object geometry : geoms )
    {
      final GM_Envelope geomEnv = GeometryUtilities.getEnvelope( geometry );
      if( env == null )
      {
        env = geomEnv;
      }
      else
      {
        env = env.getMerged( geomEnv );
      }
    }
    m_envelope = env;
  }

  public void invalidEnvelope( )
  {
    setEnvelopesUpdated();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  public void setProperty( final IPropertyType pt, final Object value )
  {
    final int pos = m_featureType.getPropertyPosition( pt );
    if( pos == -1 )
    {
      final String message = String.format( "Feature[%s] does not know this property %s", toString(), pt.getQName().toString() );
      throw new RuntimeException( new GMLSchemaException( message ) );
    }
    else
    {
      final IFeaturePropertyHandler fsh = getPropertyHandler();
      m_properties[pos] = fsh.setValue( this, pt, value );

      if( fsh.invalidateEnvelope( pt ) )
      {
        invalidEnvelope();
      }
    }
  }

  /**
   * @deprecated use getProperty(IPropertyType)
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  @Deprecated
  public Object getProperty( final String propNameLocalPart )
  {
    if( propNameLocalPart.indexOf( ':' ) > 0 )
      throw new UnsupportedOperationException( propNameLocalPart + " is not a localPart" );

    final IPropertyType pt = m_featureType.getProperty( propNameLocalPart );
    if( pt == null )
      throw new IllegalArgumentException( "unknown local part: " + propNameLocalPart );

    return getProperty( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(javax.xml.namespace.QName)
   */
  public Object getProperty( final QName propQName )
  {
    final IPropertyType pt = m_featureType.getProperty( propQName );
    if( pt == null )
    {
      final String message = String.format( "Unknown property:\n\tfeatureType=%s\n\tprop QName=%s", getFeatureType().getQName(), propQName );
      throw new IllegalArgumentException( message );
    }
    return getProperty( pt );
  }

  /**
   * @deprecated
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  @Deprecated
  public void setProperty( final String propLocalName, final Object value )
  {
    final IPropertyType pt = FeatureHelper.getPT( this, propLocalName );
    setProperty( pt, value );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getWorkspace()
   */
  public GMLWorkspace getWorkspace( )
  {
    if( m_parent instanceof GMLWorkspace )
      return (GMLWorkspace) m_parent;
    if( m_parent instanceof Feature )
      return ((Feature) m_parent).getWorkspace();
    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParent()
   */
  public Feature getParent( )
  {
    return getOwner();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParentRelation()
   */
  public IRelationType getParentRelation( )
  {
    return m_parentRelation;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setWorkspace(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void setWorkspace( final GMLWorkspace workspace )
  {
    if( (m_parent == null) || (m_parent == workspace) )
    {
      m_parent = workspace;
    }
    else
      throw new UnsupportedOperationException( "is not a root feature" );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buffer = new StringBuffer( "Feature " );
    if( m_featureType != null )
    {
      buffer.append( m_featureType.getQName().getLocalPart() );
    }
    if( m_id != null )
    {
      buffer.append( "#" + m_id );
    }
    return buffer.toString();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(javax.xml.namespace.QName, java.lang.Object)
   */
  public void setProperty( final QName propQName, final Object value )
  {
    final IFeatureType featureType = getFeatureType();

    final IPropertyType prop = featureType.getProperty( propQName );
    if( prop == null )
      throw new IllegalArgumentException( "Property not found: " + propQName );

    setProperty( prop, value );
  }

  private IFeaturePropertyHandler getPropertyHandler( )
  {
    return FeaturePropertyHandlerFactory.getInstance().getHandler( getFeatureType() );
  }

  /**
   * REMARK: only for internal use. Is used to determine if a property is a function property. Function properties do
   * not get transformed during load.<br/>
   * This is needed in order to prohibit loading of xlinked-workspaces during gml-loading, in order to avoid dead-locks.
   */
  public boolean isFunctionProperty( final IPropertyType pt )
  {
    final IFeaturePropertyHandler propertyHandler = getPropertyHandler();
    return propertyHandler.isFunctionProperty( pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getBoundedBy()
   */
  public GM_Envelope getBoundedBy( ) throws GeometryException
  {
    if( m_envelope == Feature_Impl.INVALID_ENV )
    {
      calculateEnv();
    }

    return m_envelope;
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getDefaultGeometryPropertyValue()
   */
  public GM_Object getDefaultGeometryPropertyValue( )
  {
    final IValuePropertyType defaultGeomProp = m_featureType.getDefaultGeometryProperty();
    if( defaultGeomProp == null )
      return null;

    final Object prop = getProperty( defaultGeomProp );
    if( defaultGeomProp.isList() )
    {
      final List props = (List) prop;
      return (GM_Object) (props.size() > 0 ? props.get( 0 ) : null);
    }

    if( prop == null || prop instanceof GM_Object )
      return (GM_Object) prop;

    throw new IllegalStateException( "Wrong geometry type: " + prop.getClass().getName() );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getGeometryPropertyValues()
   */
  public GM_Object[] getGeometryPropertyValues( )
  {
    final List<GM_Object> result = new ArrayList<GM_Object>();
    final IPropertyType[] ftp = m_featureType.getProperties();
    for( final IPropertyType element : ftp )
    {
      if( element instanceof IValuePropertyType && ((IValuePropertyType) element).isGeometry() )
      {
        final Object o = getProperty( element );
        if( o == null )
        {
          continue;
        }

        if( element.isList() )
        {
          result.addAll( (List) o );
        }
        else
        {
          result.add( (GM_Object) o );
        }
      }
    }

    return result.toArray( new GM_Object[result.size()] );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getOwner()
   */
  public Feature getOwner( )
  {
    if( m_parent instanceof Feature )
      return (Feature) m_parent;

    return null;
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
    m_envelope = INVALID_ENV;

    /* Invalidate geo-index of all feature-list which contains this feature. */
    // TODO: At the moment, only the owning list is invalidated. Lists who link to this feature are invald but not
    // invalidated.
    // TODO: This code is probably not very performant. How to improve this?
    // Alternative: instead of invalidating: before every query we check if any feature-envelope is invalid
    final Feature parent = getParent();
    if( parent == null )
      return;

    final IRelationType rt = getParentRelation();
    if( (rt != null) && rt.isList() )
    {
      // rt relation type and this relation type can differ (differnt feature workspaces!)
      final IRelationType relation = (IRelationType) parent.getFeatureType().getProperty( rt.getQName() );
      final FeatureList list = (FeatureList) parent.getProperty( relation );
      list.invalidate( this );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setFeatureType(org.kalypso.gmlschema.feature.IFeatureType)
   */
  public void setFeatureType( final IFeatureType ft )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setId(java.lang.String)
   */
  public void setId( final String fid )
  {
    throw new UnsupportedOperationException();
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    return NamedFeatureHelper.getName( this );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    NamedFeatureHelper.setName( this, name );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( this );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( final String desc )
  {
    NamedFeatureHelper.setDescription( this, desc );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getLocation()
   */
  public GM_Object getLocation( )
  {
    final Object property = getProperty( NamedFeatureHelper.GML_LOCATION );
    if( property instanceof GM_Object )
      return (GM_Object) property;

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setLocation(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public void setLocation( final GM_Object location )
  {
    setProperty( NamedFeatureHelper.GML_LOCATION, location );
  }

  /**
   * feature given the property {@link QName}
   *
   * @param propertyQName
   *          the {@link QName} of the property to get.
   */
  @SuppressWarnings("unchecked")
  protected <T> T getProperty( final QName propertyQName, final Class<T> propClass )
  {
    final Object prop = getProperty( propertyQName );
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

  /**
   * @see org.eclipse.core.runtime.PlatformObject#getAdapter(java.lang.Class)
   */
  @Override
  public Object getAdapter( final Class adapter )
  {
    /*
     * Small performance tweak and also works for new directly instantiated features when not registered with adapter
     * stuff.
     */
    if( adapter.isInstance( this ) )
      return this;

    return super.getAdapter( adapter );
  }
}