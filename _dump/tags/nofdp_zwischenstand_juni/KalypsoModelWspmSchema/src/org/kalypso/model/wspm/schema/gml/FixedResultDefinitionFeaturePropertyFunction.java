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
package org.kalypso.model.wspm.schema.gml;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.IGMLSchema;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction;
import org.kalypsodeegree_impl.model.feature.XLinkedFeature_Impl;

/**
 * @author Gernot Belger
 */
public class FixedResultDefinitionFeaturePropertyFunction extends FeaturePropertyFunction
{
  private static final QName QNAME_RECORD_DEFINITION = new QName( NS.SWE, "RecordDefinition" ); //$NON-NLS-1$

  private static final QName QNAME_RECORD_COMPONENT = new QName( NS.SWE, "component" ); //$NON-NLS-1$

  private static final QName QNAME_RECORD_RECORDSCHEMA = new QName( NS.SWE, "SWE_RecordSchema" ); //$NON-NLS-1$

  private final List<String> m_compHrefs = new ArrayList<String>();

  /**
   * @see org.kalypsodeegree_impl.model.feature.FeaturePropertyFunction#init(java.util.Properties)
   */
  @Override
  public void init( final Map<String, String> properties )
  {
    for( final Map.Entry<String, String> entry : properties.entrySet() )
    {
      if( entry.getKey().startsWith( "component" ) ) //$NON-NLS-1$
        m_compHrefs.add( entry.getValue() );
    }
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#setValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object setValue( final Feature feature, final IPropertyType pt, final Object valueToSet )
  {
    return getConstValue( feature, (IRelationType) pt );
  }

  /**
   * @see org.kalypsodeegree.model.feature.IFeaturePropertyHandler#getValue(org.kalypsodeegree.model.feature.Feature,
   *      org.kalypso.gmlschema.property.IPropertyType, java.lang.Object)
   */
  public Object getValue( final Feature feature, final IPropertyType pt, final Object currentValue )
  {
    return getConstValue( feature, (IRelationType) pt );
  }

  @SuppressWarnings("unchecked") //$NON-NLS-1$
  /**
   * Produces the a feature of the following kind:
   * 
   * <pre>
   *   &lt;swe:RecordDefinition gml:id=&quot;RecordDefinition1157976616979291&quot;&gt;
   *      &lt;swe:component xlink:href=&quot;urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionStation&quot;/&gt;
   *      &lt;swe:component xlink:href=&quot;urn:ogc:gml:dict:kalypso:model:wspm:components#LengthSectionRunOff&quot;/&gt;
   *   &lt;/swe:RecordDefinition&gt;
   * </pre>
   */
  private Object getConstValue( final Feature parent, final IRelationType parentRelation )
  {
    final String id = parent.getId() + "_fixedResultDefinitionId"; //$NON-NLS-1$

    final IGMLSchema schema = parent.getFeatureType().getGMLSchema();
    final IFeatureType featureType = schema.getFeatureType( QNAME_RECORD_DEFINITION );
    final IFeatureType recordSchemaType = schema.getFeatureType( QNAME_RECORD_RECORDSCHEMA );

    final Feature feature = FeatureFactory.createFeature( parent, parentRelation, id, featureType, true, 0 );
    final FeatureList components = (FeatureList) feature.getProperty( QNAME_RECORD_COMPONENT );

    for( final String href : m_compHrefs )
      components.add( new XLinkedFeature_Impl( components.getParentFeature(), components.getParentFeatureTypeProperty(), recordSchemaType, href, "", "", "", "", "" ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

    return feature;
  }

}
