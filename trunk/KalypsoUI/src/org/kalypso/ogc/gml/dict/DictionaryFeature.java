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
package org.kalypso.ogc.gml.dict;

import javax.xml.namespace.QName;

import org.deegree.model.spatialschema.GeometryException;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.NamedFeatureHelper;

/**
 * Another feature implementation used by the dictionary catalog.
 * <p>
 * There are two reasons for a new implementation:
 * </p>
 * <p>
 * 1) This distinguishes between separate calls to {@link org.kalypso.ogc.gml.dict.DictionaryCatalog#getEntry(String)},
 * so releasing works fine.
 * </p>
 * <p>
 * 2) This feature does not support change of any values, which is prohibited for dictionary entries.
 * </p>
 * 
 * @author Gernot Belger
 */
public class DictionaryFeature implements Feature
{
  private final Feature m_feature;

  public DictionaryFeature( final Feature feature )
  {
    m_feature = feature;
  }

  /**
   * @see org.eclipse.core.runtime.IAdaptable#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    return m_feature.getAdapter( adapter );
  }

  /**
   * @see org.kalypsodeegree.model.feature.DeegreeFeature#getDefaultGeometryProperty()
   */
  public GM_Object getDefaultGeometryProperty( )
  {
    return m_feature.getDefaultGeometryProperty();
  }

  /**
   * @see org.kalypsodeegree.model.feature.DeegreeFeature#getEnvelope()
   */
  public GM_Envelope getEnvelope( )
  {
    return m_feature.getEnvelope();
  }

  /**
   * @see org.kalypsodeegree.model.feature.DeegreeFeature#getFeatureType()
   */
  public IFeatureType getFeatureType( )
  {
    return m_feature.getFeatureType();
  }

  /**
   * @see org.kalypsodeegree.model.feature.DeegreeFeature#getGeometryProperties()
   */
  public GM_Object[] getGeometryProperties( )
  {
    return m_feature.getGeometryProperties();
  }

  /**
   * @see org.kalypsodeegree.model.feature.DeegreeFeature#getId()
   */
  public String getId( )
  {
    return m_feature.getId();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParent()
   */
  public Feature getParent( )
  {
    return m_feature.getParent();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getParentRelation()
   */
  public IRelationType getParentRelation( )
  {
    return m_feature.getParentRelation();
  }

  /**
   * @see org.kalypsodeegree.model.feature.DeegreeFeature#getProperties()
   */
  public Object[] getProperties( )
  {
    return m_feature.getProperties();
  }

  /**
   * @see org.kalypsodeegree.model.feature.DeegreeFeature#getProperty(org.kalypso.gmlschema.property.IPropertyType)
   */
  public Object getProperty( final IPropertyType propertyType )
  {
    return m_feature.getProperty( propertyType );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(javax.xml.namespace.QName)
   */
  public Object getProperty( final QName propQName )
  {
    return m_feature.getProperty( propQName );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getProperty(java.lang.String)
   */
  public Object getProperty( final String propLocalName )
  {
    return m_feature.getProperty( propLocalName );
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#getWorkspace()
   */
  public GMLWorkspace getWorkspace( )
  {
    return m_feature.getWorkspace();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(org.kalypso.gmlschema.property.IPropertyType,
   *      java.lang.Object)
   */
  public void setProperty( final IPropertyType propertyType, final Object value )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(javax.xml.namespace.QName, java.lang.Object)
   */
  public void setProperty( final QName propQName, final Object value )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setProperty(java.lang.String, java.lang.Object)
   */
  public void setProperty( final String propLocalName, final Object value )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#setWorkspace(org.kalypsodeegree.model.feature.GMLWorkspace)
   */
  public void setWorkspace( final GMLWorkspace workspace )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.Feature#invalidEnvelope()
   */
  public void invalidEnvelope( )
  {
    m_feature.invalidEnvelope();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getBoundedBy()
   */
  @Override
  public GM_Envelope getBoundedBy( ) throws GeometryException
  {
    return m_feature.getBoundedBy();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getDefaultGeometryPropertyValue()
   */
  @Override
  public GM_Object getDefaultGeometryPropertyValue( )
  {
    return m_feature.getDefaultGeometryPropertyValue();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getGeometryPropertyValues()
   */
  @Override
  public GM_Object[] getGeometryPropertyValues( )
  {
    return m_feature.getGeometryPropertyValues();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getOwner()
   */
  @Override
  public Feature getOwner( )
  {
    return m_feature.getOwner();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#getQualifiedName()
   */
  @Override
  public QName getQualifiedName( )
  {
    return m_feature.getQualifiedName();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setEnvelopesUpdated()
   */
  @Override
  public void setEnvelopesUpdated( )
  {
    m_feature.setEnvelopesUpdated();
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setFeatureType(org.kalypso.gmlschema.feature.IFeatureType)
   */
  @Override
  public void setFeatureType( IFeatureType ft )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.Deegree2Feature#setId(java.lang.String)
   */
  @Override
  public void setId( String fid )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getName()
   */
  public String getName( )
  {
    return NamedFeatureHelper.getName( m_feature );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setName(java.lang.String)
   */
  public void setName( final String name )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getDescription()
   */
  @Override
  public String getDescription( )
  {
    return NamedFeatureHelper.getDescription( m_feature );
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setDescription(java.lang.String)
   */
  public void setDescription( final String desc )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#getLocation()
   */
  public GM_Object getLocation( )
  {
    Object property = m_feature.getProperty( NamedFeatureHelper.GML_LOCATION );
    if( property instanceof GM_Object )
      return (GM_Object) property;

    return null;
  }

  /**
   * @see org.kalypsodeegree.model.feature.binding.IFeatureWrapper2#setLocation(org.kalypsodeegree.model.geometry.GM_Object)
   */
  public void setLocation( final GM_Object location )
  {
    throw new UnsupportedOperationException( "Dictionary entries may not be changed." ); //$NON-NLS-1$
  }
}
