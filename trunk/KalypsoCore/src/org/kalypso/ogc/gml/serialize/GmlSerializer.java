package org.kalypso.ogc.gml.serialize;

import java.io.Writer;
import java.net.URL;

import javax.xml.transform.OutputKeys;
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
import org.kalypso.java.net.IUrlResolver;
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

  public static void serializeWorkspace( final Writer writer, final GMLWorkspace workspace,
      final IProgressMonitor monitor ) throws GmlSerializeException
  {
    if( monitor != null )
      monitor.beginTask( "GML wird geschrieben", 2000 );
    try
    {
      final GMLDocument gmlDoc = new GMLDocument_Impl();
      GMLFeature gmlFeature = GMLFactory.createGMLFeature( gmlDoc.getDocument(), workspace
          .getRootFeature() );
      gmlDoc.setRoot( gmlFeature );

      final GMLNameSpace gmlNameSpace = new GMLNameSpace_Impl(
          "xmlns:gml=http://www.opengis.net/gml" );
      final GMLNameSpace xlinkNameSpace = new GMLNameSpace_Impl(
          "xmlns:xlink=http://www.w3.org/1999/xlink" );
      final GMLNameSpace xsiNameSpace = new GMLNameSpace_Impl(
          "xmlns:xsi=http://www.w3.org/2001/XMLSchema-instance"
       );
      gmlDoc.addNameSpace( gmlNameSpace );
      gmlDoc.addNameSpace( xlinkNameSpace );
      gmlDoc.addNameSpace( xsiNameSpace );

      workspace.getContext();
      final String schemaLoc = workspace.getSchemaLocation();
      if( schemaLoc != null )
        gmlDoc.setSchemaLocation( schemaLoc );
      
      if( monitor != null )
        monitor.worked( 1000 );

      // DOM als GML schreiben
      final Document xmlDOM = gmlDoc.getDocument();
      final Transformer t = TransformerFactory.newInstance().newTransformer();
   
      t.setOutputProperty("{http://xml.apache.org/xslt}indent-amount", "2");
      t.setOutputProperty(OutputKeys.INDENT, "yes");
      
      t.transform( new DOMSource( xmlDOM ), new StreamResult( writer ) );
    }
    catch( final Exception e )
    {
      throw new GmlSerializeException( "Fehler beim Schreiben des GML Stream", e );
    }
    finally
    {
      if( monitor != null )
        monitor.done();
    }
  }

  /**
   * @deprecated use {@link #createGMLWorkspace(URL, IUrlResolver)}instead.
   */
  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final URL schemaURL )
      throws Exception
  {
    // load gml
    final GMLDocument_Impl gml = new GMLDocument_Impl( XMLHelper.getAsDOM( gmlURL ) );

    final GMLFeature gmlFeature = gml.getRootFeature();

    // load schema
    final GMLSchema schema = new GMLSchema( schemaURL );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gmlFeature, types );

    return new GMLWorkspace_Impl( types, feature, gmlURL, schemaURL.toString() );
  }

  public static GMLWorkspace createGMLWorkspace( final URL gmlURL, final IUrlResolver urlResolver )
      throws Exception
  {
    // load gml
    final GMLDocument_Impl gml = new GMLDocument_Impl( XMLHelper.getAsDOM( gmlURL ) );

    // load schema
    final String schemaLocationName = gml.getSchemaLocationName();
    if( schemaLocationName == null || schemaLocationName.length() == 0 )
      throw new Exception( "Keine 'schemaLocation' in gml spezifiziert." );

    final URL schemaLocation = urlResolver.resolveURL( gmlURL, schemaLocationName );
    final GMLSchema schema = new GMLSchema( schemaLocation );

    // create feature and workspace gml
    final FeatureType[] types = schema.getFeatureTypes();
    final Feature feature = FeatureFactory.createFeature( gml.getRootFeature(), types );

    return new GMLWorkspace_Impl( types, feature, gmlURL, schemaLocationName );
  }

}