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

    final IPropertyType[] sourceProps = new IPropertyType[] {
    // GMLSchemaFactory.createValuePropertyType(name, valueQName, typeHandler, countTestCases(), countTestCases())
        GMLSchemaFactory.createValuePropertyType( SOURCE_STRING_PROP, stringTH.getTypeName(), stringTH, 1, 1, false ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_DOUBLE_PROP, doubleTH.getTypeName(), stringTH, 1, 1, false ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_INT_PROP, integerTH.getTypeName(), stringTH, 1, 1, false ) };
    final IFeatureType sourceFTP = GMLSchemaFactory.createFeatureType( new QName( NAMESPACE, "sourceFT" ), sourceProps );

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