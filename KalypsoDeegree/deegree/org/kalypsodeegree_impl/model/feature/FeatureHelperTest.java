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

import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.commons.xml.NS;
import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.IMarshallingTypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.TypeHandlerUtilities;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public final class FeatureHelperTest extends TestCase
{
  /**
   * @see junit.framework.TestCase#setUp()
   */
  @Override
  protected void setUp( ) throws Exception
  {
    final ITypeRegistry<IMarshallingTypeHandler> marshallingregistry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    TypeHandlerUtilities.registerXSDSimpleTypeHandler( marshallingregistry );
    // TypeHandlerUtilities.registerTypeHandlers( marshallingregistry );

    super.setUp();
  }

  public final void testCopyProperties( ) throws Exception
  {
    // zwei feature types erzeugen
    final String NAMESPACE = "namespace";
    final QName SOURCE_STRING_PROP = new QName( NAMESPACE, "sourceStringprop" );
    final QName SOURCE_DOUBLE_PROP = new QName( NAMESPACE, "sourceDoubleprop" );
    final QName SOURCE_INT_PROP = new QName( NAMESPACE, "sourceIntprop" );

    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final IMarshallingTypeHandler stringTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final IMarshallingTypeHandler integerTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "int" ) );
    // final IMarshallingTypeHandler longTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "long" ) );
    final IMarshallingTypeHandler doubleTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "double" ) );
    // final IMarshallingTypeHandler floatTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "float" )
    // );
    // final IMarshallingTypeHandler booleanTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "boolean"
    // ) );
    // final IMarshallingTypeHandler dateTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "date" ) );

    final QName sourceFeatureQName = new QName( NAMESPACE, "sourceFT" );
    final IPropertyType[] sourceProps = new IPropertyType[] {
    // GMLSchemaFactory.createValuePropertyType(name, valueQName, typeHandler, countTestCases(), countTestCases())
        GMLSchemaFactory.createValuePropertyType( SOURCE_STRING_PROP, stringTH, 1, 1, false ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_DOUBLE_PROP, doubleTH, 1, 1, false ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_INT_PROP, integerTH, 1, 1, false ) };
    final IFeatureType sourceFTP = GMLSchemaFactory.createFeatureType( sourceFeatureQName, sourceProps );

    // zwei features erzeugen
    final Feature sourcefeature = FeatureFactory.createFeature( null, null, "source", sourceFTP, new Object[] { "Hallo", new Double( 3.14 ), new Integer( 1 ) } );

    final Feature targetfeature = FeatureFactory.createFeature( null, null, "test", sourceFTP, true );

    // mapping erzeugen
    final Properties mapping = new Properties();
    mapping.setProperty( SOURCE_STRING_PROP.getLocalPart(), SOURCE_STRING_PROP.getLocalPart() );
    mapping.setProperty( SOURCE_DOUBLE_PROP.getLocalPart(), SOURCE_DOUBLE_PROP.getLocalPart() );

    // mappen
    FeatureHelper.copyProperties( sourcefeature, targetfeature, mapping );
    compareByMapping( sourcefeature, targetfeature, mapping );
  }

  private void compareByMapping( final Feature sourcefeature, final Feature targetfeature, final Properties mapping )
  {
    for( final Object element : mapping.entrySet() )
    {
      final Map.Entry entry = (Entry) element;
      final String sourceProp = (String) entry.getKey();
      final String targetProp = (String) entry.getValue();

      // TODO: was ist mit deep-copy?

      final Object sproperty = sourcefeature.getProperty( sourceProp );
      final Object tproperty = targetfeature.getProperty( targetProp );
      // immutable object können ruhig gleich bleiben
      if( !(sproperty instanceof String) && !(sproperty instanceof Number) )
        assertNotSame( sproperty, tproperty );
      assertEquals( sproperty, tproperty );
    }
  }
}