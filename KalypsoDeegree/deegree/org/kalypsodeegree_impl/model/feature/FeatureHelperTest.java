package org.kalypsodeegree_impl.model.feature;

import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import junit.framework.TestCase;

import org.kalypso.gmlschema.GMLSchemaFactory;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.types.ITypeHandler;
import org.kalypso.gmlschema.types.ITypeRegistry;
import org.kalypso.gmlschema.types.MarshallingTypeRegistrySingleton;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author belger
 */
public final class FeatureHelperTest extends TestCase
{
  public final void testCopyProperties( ) throws CloneNotSupportedException
  {
    // zwei feature types erzeugen
    final String NS = "namespace";
    final QName SOURCE_STRING_PROP = new QName( NS, "sourceStringprop" );
    final QName SOURCE_DOUBLE_PROP = new QName( NS, "sourceDoubleprop" );
    final QName SOURCE_INT_PROP = new QName( NS, "sourceIntprop" );

    final ITypeRegistry registry = MarshallingTypeRegistrySingleton.getTypeRegistry();
    final ITypeHandler stringTH = registry.getTypeHandlerForClassName( String.class );
    final ITypeHandler integerTH = registry.getTypeHandlerForClassName( Integer.class );
    final ITypeHandler longTH = registry.getTypeHandlerForClassName( Long.class );
    final ITypeHandler doubleTH = registry.getTypeHandlerForClassName( Double.class );
    final ITypeHandler floatTH = registry.getTypeHandlerForClassName( Float.class );
    final ITypeHandler booleanTH = registry.getTypeHandlerForClassName( Boolean.class );
    final ITypeHandler dateTH = registry.getTypeHandlerForClassName( Date.class );

    final IPropertyType[] sourceProps = new IPropertyType[] {
    // GMLSchemaFactory.createValuePropertyType(name, valueQName, typeHandler, countTestCases(), countTestCases())
        GMLSchemaFactory.createValuePropertyType( SOURCE_STRING_PROP, stringTH.getTypeName()[0], stringTH, 1, 1 ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_DOUBLE_PROP, stringTH.getTypeName()[0], stringTH, 1, 1 ), //
        GMLSchemaFactory.createValuePropertyType( SOURCE_INT_PROP, stringTH.getTypeName()[0], stringTH, 1, 1 ) };
    final IFeatureType sourceFTP = GMLSchemaFactory.createFeatureType( new QName( NS, "sourceFT" ), sourceProps );

    // zwei features erzeugen
    final Feature sourcefeature = FeatureFactory.createFeature( "source", sourceFTP, new Object[] { "Hallo", new Double( 3.14 ), new Integer( 1 ) } );

    final Feature targetfeature = FeatureFactory.createDefaultFeature( "test", sourceFTP, false );

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