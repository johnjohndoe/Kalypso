/*
 * --------------- Kalypso-Header --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.gml.serialize;

import java.io.StringReader;
import java.io.StringWriter;

import javax.xml.bind.JAXBContext;
import javax.xml.bind.JAXBException;
import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypso.jwsdp.JaxbUtilities;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.extension.IMarshallingTypeHandler;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.xml.sax.InputSource;

/**
 * 
 * Helper for cloning objects
 * 
 * @author doemming
 */
public class CloneUtilities
{

  private static ITypeRegistry m_typeHandlerRegistry = MarshallingTypeRegistrySingleton.getTypeRegistry();

  /**
   * 
   * clones a object that has a marshaller and unmarshaller
   * 
   * @param value
   *          to clone
   * @param factory
   *          that can marshall and unmarshall the value
   * @return cloned object
   * @throws JAXBException
   */
  public static Object clone( final Object value, final JAXBContext jc ) throws JAXBException
  {
    // marshall it
    
    final Marshaller marshaller = JaxbUtilities.createMarshaller(jc);
    final StringWriter tmpBuffer = new StringWriter();
    marshaller.marshal( value, tmpBuffer );

    // unmarshall it
    final Unmarshaller unmarshaller = jc.createUnmarshaller();
    StringReader reader = new StringReader( tmpBuffer.toString() );
    final InputSource source = new InputSource(reader);
    return unmarshaller.unmarshal( source );
  }

  public static Feature clone( Feature featureToClone ) throws CloneNotSupportedException
  {
    Object[] properties = featureToClone.getProperties();
    Object[] clonedProperties = new Object[properties.length];
    IFeatureType featureType = featureToClone.getFeatureType();
    IPropertyType[] featureTypeProperties = featureType.getProperties();
    for( int i = 0; i < properties.length; i++ )
    {
      Object property = properties[i];

      if( property != null )
        clonedProperties[i] = cloneFeatureProperty( property, featureTypeProperties[i] );

    }
    return FeatureFactory.createFeature( featureToClone.getId(), featureType, clonedProperties );

  }

  public static Object cloneFeatureProperty( Object featureProperty, IPropertyType property )
      throws CloneNotSupportedException
  {
    final Class clazz = featureProperty.getClass();
    final ITypeHandler typeHandler = m_typeHandlerRegistry.getTypeHandlerForClassName( clazz );
    
    if( typeHandler == null )
      return FeatureFactory.createFeatureProperty( property,featureProperty );

    if( typeHandler instanceof IMarshallingTypeHandler )
      return ( (IMarshallingTypeHandler)typeHandler ).cloneObject( featureProperty );
    throw new CloneNotSupportedException( "Object: " + featureProperty.getClass().getName() + "\tis not clonable" );
  }
}
