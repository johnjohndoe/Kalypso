package org.kalypso.ogc.gml.serialize;

import java.io.Writer;
import java.net.URL;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;

import org.deegree.gml.GMLDocument;
import org.deegree.gml.GMLFeature;
import org.deegree.gml.GMLNameSpace;
import org.deegree.model.feature.Feature;
import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.GMLWorkspace;
import org.deegree_impl.gml.GMLDocument_Impl;
import org.deegree_impl.gml.GMLFactory;
import org.deegree_impl.gml.GMLNameSpace_Impl;
import org.deegree_impl.gml.schema.GMLSchema;
import org.deegree_impl.gml.schema.XMLHelper;
import org.deegree_impl.model.feature.FeatureFactory;
import org.deegree_impl.model.feature.GMLWorkspace_Impl;
import org.eclipse.core.runtime.IProgressMonitor;
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

    return new GMLWorkspace_Impl( types, feature, gmlURL );
  }

}