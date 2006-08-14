package org.kalypsodeegree_impl.model.feature;

import java.util.Iterator;
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
import org.kalypso.gmlschema.types.ITypeHandler;
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

  public final void testCopyProperties( ) throws CloneNotSupportedException
  {
    // zwei feature types erzeugen
    final String NAMESPACE = "namespace";
    final QName SOURCE_STRING_PROP = new QName( NAMESPACE, "sourceStringprop" );
    final QName SOURCE_DOUBLE_PROP = new QName( NAMESPACE, "sourceDoubleprop" );
    final QName SOURCE_INT_PROP = new QName( NAMESPACE, "sourceIntprop" );

    final ITypeRegistry<IMarshallingTypeHandler> registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final ITypeHandler stringTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "string" ) );
    final ITypeHandler integerTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "int" ) );
    // final ITypeHandler longTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "long" ) );
    final ITypeHandler doubleTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "double" ) );
    // final ITypeHandler floatTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "float" ) );
    // final ITypeHandler booleanTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "boolean" ) );
    // final ITypeHandler dateTH = registry.getTypeHandlerForTypeName( new QName( NS.XSD_SCHEMA, "date" ) );

    final IPropertyType[] sourceProps = new IPropertyType[] {
    // GMLSchemaFactory.createValuePropertyType(name, valueQName, typeHandler, countTestCases(), countTestCases())
        GMLSchemaFactory.createValuePropertyType( SOURCE_STRING_PROP, stringTH.getTypeName(), stringTH, 1, 1, false ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_DOUBLE_PROP, doubleTH.getTypeName(), stringTH, 1, 1, false ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_INT_PROP, integerTH.getTypeName(), stringTH, 1, 1, false ) };
    final IFeatureType sourceFTP = GMLSchemaFactory.createFeatureType( new QName( NAMESPACE, "sourceFT" ), sourceProps );

    // zwei features erzeugen
    final Feature sourcefeature = FeatureFactory.createFeature( null, "source", sourceFTP, new Object[] { "Hallo", new Double( 3.14 ), new Integer( 1 ) } );

    final Feature targetfeature = FeatureFactory.createFeature( null, "test", sourceFTP, true );

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
    for( final Iterator pIt = mapping.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry) pIt.next();
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