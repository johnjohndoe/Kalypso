package org.kalypso.ogc.gml.serialize;

import java.io.Writer;
import java.net.URL;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLFeatureCollection;
import org.deegree.gml.GMLNameSpace;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.gml.GMLNameSpace_Impl;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.gml.schema.XMLHelper;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.GMLWorkspace_Impl;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.ogc.gml.GMLHelper;
import org.kalypso.ogc.gml.KalypsoFeatureLayer;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

/**
 * Helper - Klasse, um Gml zu lesen und zu schreiben
 * 
 * @author Belger
 */
public final class GmlSerializer
{
  private GmlSerializer()
  {
  // do not instantiate this class
  }

  /**
   * @deprecated use {@link #createGMLWorkspace(URL, URL)}instead.
   */
  public final static KalypsoFeatureLayer[] deserialize( final URL schemaURL, final URL gmlURL,
      final CS_CoordinateSystem targetCrs, final IProgressMonitor monitor )
      throws GmlSerializeException
  {
    if( monitor != null )
      monitor.beginTask( "GML wird geladen", 5000 );

    try
    {
      final GMLWorkspace workspace = createGMLWorkspace( gmlURL, schemaURL );

      // create layers
      final FeatureType[] fts = workspace.getFeatureTypes();
      final KalypsoFeatureLayer[] featureLayer = new KalypsoFeatureLayer[fts.length];

      for( int i = 0; i < featureLayer.length; i++ )
      {
        final FeatureType ft = fts[i];
        featureLayer[i] = new KalypsoFeatureLayer( ft.getName(), ft, targetCrs, workspace );
        Feature[] features = workspace.getFeatures( ft );
        for( int j = 0; j < features.length; j++ )
        {
          Feature feature = features[j];
          GMLHelper.checkCrs( feature, targetCrs );
          featureLayer[i].addFeature( feature );
          featureLayer[i].optimize();
        }
      }

      if( monitor != null )
        monitor.worked( 1000 );
      return featureLayer;

      //      // collect features
      //     	 final GMLFeatureCollection gmlFC = gml.getRoot();
      //      final GMLFeature[] gmlFeatures = gmlFC.getFeatures();
      //    	for( int i = 0; i < gmlFeatures.length; i++ )
      //      {
      //        final Feature feature = FeatureFactory.createFeature( gmlFeatures[i],
      // types );
      //
      //        final KalypsoFeatureLayer fl = (KalypsoFeatureLayer)layerMap.get(
      // feature.getFeatureType() );

      //      }
      //      if(monitor!=null)
      //             monitor.worked( 1000 );
      //
      //      // optimize layers
      //      for( final Iterator iter = layerMap.values().iterator();
      // iter.hasNext(); )
      //      {
      //        final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)iter.next();
      //        layer.optimize();
      //      }
      //      if(monitor!=null)
      //              monitor.worked( 1000 );

      //      return (KalypsoFeatureLayer[])layerMap.values().toArray(
      //          new KalypsoFeatureLayer[layerMap.size()] );
      //    	return null;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      throw new GmlSerializeException( "Fehler beim laden von GML", e );
    }
  }

  //  public static KalypsoFeatureLayer[] deserialize( final StringReader
  // schemaReader,
  //      final StringReader gmlReader, final CS_CoordinateSystem crs, final
  // IProgressMonitor monitor )
  //      throws GmlSerializeException
  //  {
  //    return deserialize( new InputSource( schemaReader ), new InputSource(
  // gmlReader ), crs, monitor );
  //  }

  public static void serialize( final Writer writer, final KalypsoFeatureLayer[] layers,
      final IProgressMonitor monitor ) throws GmlSerializeException
  {
    if( monitor != null )
      monitor.beginTask( "GML wird geschrieben", 2000 );

    try
    {
      // DOM erzeugen
      int featureCount = 0;
      for( int i = 0; i < layers.length; i++ )
        featureCount += layers[i].getSize();

      final GMLFeatureCollection gmlCollection = GMLFactory
          .createGMLFeatureCollection( "collection" );
      final GMLDocument gmlDoc = getGmlDocument( gmlCollection );

      GM_Envelope boundingBox = null;
      for( int i = 0; i < layers.length; i++ )
      {
        final KalypsoFeatureLayer layer = layers[i];

        if( boundingBox == null )
          boundingBox = layer.getBoundingBox();
        else
        {

          boundingBox.merge( layer.getBoundingBox() );
        }
        final Feature[] features = layer.getAllFeatures();
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

      if( monitor != null )
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

  public static void serializeFeature( final Writer writer, final Feature feature,
      final IProgressMonitor monitor ) throws GmlSerializeException
  {
    if( monitor != null )
      monitor.beginTask( "GML wird geschrieben", 2000 );
    try
    {
      final GMLDocument gmlDoc = new GMLDocument_Impl();
      GMLFeature gmlFeature = GMLFactory.createGMLFeature( gmlDoc.getDocument(), feature );
      gmlDoc.setRoot( gmlFeature );

      GMLNameSpace gmlNameSpace = new GMLNameSpace_Impl( "xmlns:gml=http://www.opengis.net/gml" );
      GMLNameSpace xlinkNameSpace = new GMLNameSpace_Impl(
          "xmlns:xlink=http://www.w3.org/1999/xlink" );
      gmlDoc.addNameSpace( gmlNameSpace );
      gmlDoc.addNameSpace( xlinkNameSpace );

      if( monitor != null )
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

  private static GMLDocument getGmlDocument( GMLFeatureCollection col )
  {
    GMLDocument result = new GMLDocument_Impl();
    result.setRoot( col );
    GMLNameSpace gmlNameSpace = new GMLNameSpace_Impl( "xmlns:gml=http://www.opengis.net/gml" );
    GMLNameSpace xlinkNameSpace = new GMLNameSpace_Impl( "xmlns:xlink=http://www.w3.org/1999/xlink" );
    result.addNameSpace( gmlNameSpace );
    result.addNameSpace( xlinkNameSpace );
    return result;
  }

  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final URL schemaURL )
      throws Exception
  {
    // load schema
    final GMLSchema schema = new GMLSchema( schemaURL );
    // load gml
    final GMLDocument_Impl gml = new GMLDocument_Impl( XMLHelper.getAsDOM( gmlURL ) );
    final GMLFeature gmlFeature = gml.getRootFeature();
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gmlFeature, types );

    return new GMLWorkspace_Impl( schema, feature );
    //    GMLHelper.checkCrs( feature, targetCrs );

    // optimize layers
    //      for( final Iterator iter = layerMap.values().iterator(); iter.hasNext();
    // )
    //      {
    //        final KalypsoFeatureLayer layer = (KalypsoFeatureLayer)iter.next();
    //        layer.optimize();
    //      }
    //      if( monitor != null )
    //        monitor.worked( 1000 );
    //
    //      return (KalypsoFeatureLayer[])layerMap.values().toArray(
    //          new KalypsoFeatureLayer[layerMap.size()] );
    //    }
    //    catch( final Exception e )
    //    {
    //      e.printStackTrace();
    //
    //      throw new GmlSerializeException( "Fehler beim laden von GML", e );

  }

}