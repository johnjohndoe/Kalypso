package org.kalypsodeegree_impl.model.feature;

import java.util.Iterator;
import java.util.Map;
import java.util.Properties;
import java.util.Map.Entry;

import junit.framework.TestCase;

import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureType;
import org.kalypsodeegree.model.feature.FeatureTypeProperty;

/**
 * @author belger
 */
public final class FeatureHelperTest extends TestCase
{
  public final void testCopyProperties()
  {
    // zwei feature types erzeugen
    final String SOURCE_STRING_PROP = "sourceStringprop";
    final String SOURCE_DOUBLE_PROP = "sourceDoubleprop";
    final String SOURCE_INT_PROP = "sourceIntprop";

    final FeatureTypeProperty[] sourceProps = new FeatureTypeProperty[]
    {
        FeatureFactory
            .createFeatureTypeProperty( SOURCE_STRING_PROP, String.class.getName(), false ),
        FeatureFactory
            .createFeatureTypeProperty( SOURCE_DOUBLE_PROP, String.class.getName(), false ),
        FeatureFactory.createFeatureTypeProperty( SOURCE_INT_PROP, String.class.getName(), false ) };
    final FeatureType sourceFTP = FeatureFactory.createFeatureType( "sourceFT", null, sourceProps,
        null, null, null, null );

    // zwei features erzeugen
    final Feature sourcefeature = FeatureFactory.createFeature( "source", sourceFTP, new Object[]
    {
        "Hallo",
        new Double( 3.14 ),
        new Integer( 1 ) } );

    final Feature targetfeature = FeatureFactory.createDefaultFeature( "test", sourceFTP, false );

    // mapping erzeugen
    final Properties mapping = new Properties();
    mapping.setProperty( SOURCE_STRING_PROP, SOURCE_STRING_PROP );
    mapping.setProperty( SOURCE_DOUBLE_PROP, SOURCE_DOUBLE_PROP );

    // mappen
    FeatureHelper.copyProperties( sourcefeature, targetfeature, mapping );
    compareByMapping( sourcefeature, targetfeature, mapping );
  }

  private void compareByMapping( final Feature sourcefeature, final Feature targetfeature,
      final Properties mapping )
  {
    for( final Iterator pIt = mapping.entrySet().iterator(); pIt.hasNext(); )
    {
      final Map.Entry entry = (Entry)pIt.next();
      final String sourceProp = (String)entry.getKey();
      final String targetProp = (String)entry.getValue();

      // TODO: was ist mit deep-copy?
      
      final Object sproperty = sourcefeature.getProperty( sourceProp );
      final Object tproperty = targetfeature.getProperty( targetProp );
      // immutable object können ruhig gleich bleiben
      if( !( sproperty instanceof String ) && !(sproperty instanceof Number) )
        assertNotSame( sproperty, tproperty );
      assertEquals( sproperty, tproperty );
    }
  }
}