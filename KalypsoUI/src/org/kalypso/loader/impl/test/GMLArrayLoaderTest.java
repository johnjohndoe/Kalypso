package org.kalypso.loader.impl.test;

import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.HashMap;

import junit.framework.TestCase;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.graphics.FeatureLayer;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.JMSchema;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.xml.XMLTools;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author sbad0205
 */
public class GMLArrayLoaderTest extends TestCase
{

  public void testLoad()
  {
    final InputStream schemaInputStream = getClass().getResourceAsStream( "point.xsd" );

    final InputStream gmlInputStream = getClass().getResourceAsStream( "point.gml" );
    try
    {
      final JMSchema schema = new JMSchema( XMLTools.getAsDOM( schemaInputStream ) );
      final HashMap layerMap = new HashMap();
      final FeatureType[] types = schema.getFeatureTypes();

      final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
      final CS_CoordinateSystem layerCrs = org.deegree_impl.model.cs.Adapters.getDefault().export(
          csFac.getCSByName( "EPSG:4326" ) );
      for( int i = 0; i < types.length; i++ )
      {
        FeatureType type = types[i];
        layerMap.put( type, new KalypsoFeatureLayer( type.getName(), type, layerCrs ) );
      }

      //final InputStreamReader reader = new InputStreamReader(
      // file.getContents(), file.getCharset() );

      final InputStreamReader reader = new InputStreamReader( gmlInputStream );
      GMLDocument gml = new GMLDocument_Impl( reader );
      reader.close();

      GMLFeatureCollection gmlFC = gml.getRoot();
      GMLFeature[] gmlFeatures = gmlFC.getFeatures();

      for( int i = 0; i < gmlFeatures.length; i++ )
      {
        if( i % 10 == 0 )
        {
          final double v = ( i * 100 ) / gmlFeatures.length;
          System.out.println( "loaded " + v + "%" );
        }

        final Feature feature = FeatureFactory.createFeature( gmlFeatures[i], types );
        GMLHelper.checkCrs( feature, layerCrs );
        final FeatureLayer fl = (FeatureLayer)layerMap.get( feature.getFeatureType() );
        fl.addFeature( feature );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
      fail( e.getMessage() );
    }
    // 
  }
}