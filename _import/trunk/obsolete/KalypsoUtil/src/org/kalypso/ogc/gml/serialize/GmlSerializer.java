package org.kalypso.ogc.gml.serialize;

import java.io.StringReader;
import java.io.Writer;
import java.util.HashMap;
import java.util.Iterator;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.model.feature.FeatureFactory;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.JMSchema;
import org.kalypso.ogc.gml.KalypsoFeature;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.kalypso.util.progress.IProgressMonitor;
import org.kalypso.util.xml.XMLTools;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;
import org.xml.sax.InputSource;

/**
 * Helper - Klasse, um Gml zu lesen und zu schreiben
 * 
 * @author gernot
 */
public final class GmlSerializer
{
  private GmlSerializer()
  {
  // do not instantiatie this class
  }

  public final static KalypsoFeatureLayer[] deserialize( final InputSource schemaSource,
      final InputSource gmlSource, final CS_CoordinateSystem targetCrs,
      final IProgressMonitor monitor ) throws GmlSerializeException
  {
    monitor.beginTask( "GML wird geladen", 5000 );

    try
    {
      // load gml
      final GMLDocument gml = new GMLDocument_Impl( XMLTools.getAsDOM( gmlSource ) );
      monitor.worked( 1000 );

      // load schema
      final JMSchema schema = new JMSchema( XMLTools.getAsDOM( schemaSource ) );
      monitor.worked( 1000 );

      // create layers
      final HashMap layerMap = new HashMap();
      final FeatureType[] types = schema.getFeatureTypes();
      for( int i = 0; i < types.length; i++ )
      {
        final FeatureType type = types[i];
        layerMap.put( type, new KalypsoFeatureLayer( type.getName(), type, targetCrs ) );
      }
      monitor.worked( 1000 );

      // collect features
      final GMLFeatureCollection gmlFC = gml.getRoot();
      final GMLFeature[] gmlFeatures = gmlFC.getFeatures();
      for( int i = 0; i < gmlFeatures.length; i++ )
      {
        final Feature feature = FeatureFactory.createFeature( gmlFeatures[i], types );
        GMLHelper.checkCrs( feature, targetCrs );

        final KalypsoFeatureLayer fl = (KalypsoFeatureLayer)layerMap.get( feature.getFeatureType() );
        fl.addFeature( new KalypsoFeature( feature ) );
      }
      monitor.worked( 1000 );

      // optimize layers
      for( final Iterator iter = layerMap.values().iterator(); iter.hasNext(); )
      {
        final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)iter.next();
        layer.optimize();
      }
      monitor.worked( 1000 );

      return (KalypsoFeatureLayer[])layerMap.values().toArray(
          new KalypsoFeatureLayer[layerMap.size()] );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim laden von GML", e );
    }
  }

  public static KalypsoFeatureLayer[] deserialize( final StringReader schemaReader,
      final StringReader gmlReader, final CS_CoordinateSystem crs, final IProgressMonitor monitor )
      throws GmlSerializeException
  {
    return deserialize( new InputSource( schemaReader ), new InputSource( gmlReader ), crs, monitor );
  }

  public static void serialize( final Writer writer, final KalypsoFeatureLayer[] layers,
      final IProgressMonitor monitor ) throws GmlSerializeException
  {
    monitor.beginTask( "GML wird geschrieben", 2000 );

    try
    {
      // DOM erzeugen
      int featureCount = 0;
      for( int i = 0; i < layers.length; i++ )
        featureCount += layers[i].getSize();

      final GMLFeatureCollection gmlCollection = GMLFactory
          .createGMLFeatureCollection( "collection" );
      final GMLDocument gmlDoc = new GMLDocument_Impl();

      GM_Envelope boundingBox = null;
      for( int i = 0; i < layers.length; i++ )
      {
        final KalypsoFeatureLayer layer = layers[i];

        if( boundingBox == null )
          boundingBox = layer.getBoundingBox();
        else
          boundingBox.merge( layer.getBoundingBox() );

        final KalypsoFeature[] features = layer.getAllFeatures();
        for( int j = 0; j < features.length; j++ )
        {
          GMLFeature gmlFeature = GMLFactory.createGMLFeature( gmlDoc.getDocument(), features[j] );
          gmlFeature.setId( features[j].getId() );
          gmlCollection.addFeature( gmlFeature );
        }
      }

      if( boundingBox != null )
      {
        final double minx = boundingBox.getMin().getX();
        final double maxx = boundingBox.getMax().getX();
        final double miny = boundingBox.getMin().getY();
        final double maxy = boundingBox.getMax().getY();
        gmlCollection.setBoundingBox( minx, miny, maxx, maxy );
      }

      // muss NACH dem adden aller Features und dem setzen der Bounding Box
      // passieren
      gmlDoc.setRoot( gmlCollection );

      monitor.worked( 1000 );

      // DOM als GML schreiben
      final Document xmlDOM = gmlDoc.getDocument();

      final Transformer t = TransformerFactory.newInstance().newTransformer();
            t.transform( new DOMSource( xmlDOM ), new StreamResult( writer ) );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Schreiben des GML Stream", e );
    }

  }

}