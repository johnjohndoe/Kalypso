package org.kalypso.ogc.gml.loader;

import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.io.StringReader;
import java.io.StringWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Properties;

import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.deegree.services.wfs.capabilities.FeatureType;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.eclipse.core.runtime.IProgressMonitor;
import org.kalypso.java.util.PropertiesHelper;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.gml.GMLFeature;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.xml.XMLTools;
import org.kalypsodeegree_impl.gml.GMLDocument_Impl;
import org.kalypsodeegree_impl.gml.schema.GMLSchema;
import org.kalypsodeegree_impl.gml.schema.GMLSchemaCache;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;
import org.kalypsodeegree_impl.model.feature.GMLWorkspace_Impl;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;
import org.w3c.dom.Document;

public class WfsLoader extends AbstractLoader
{

  private String featureType;

  /**
   * Loads a WFS DataSource from the given URL
   * 
   * @param source
   *          the href-tag from the gmt-file 'ex:
   *          http://localhost:8080/deegreewfs#river' where river denotes the
   *          feature to be loaded
   * @param context
   *          the URL form the map context (here the path to the associated gmt
   *          file)
   */
  protected Object loadIntern( String source, URL context, IProgressMonitor monitor )
      throws LoaderException
  {

    try
    {
      monitor.beginTask( "WFS laden", 1000 );
      URL url = null;
      final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );
      String path = sourceProps.getProperty( "URL" );

      featureType = sourceProps.getProperty( "FEATURE" );
      if( path != null )
      {
        url = new URL( path );
      }

      //          String[] array = StringExtend.toArray(source, "#", true);
      //
      //          if (array[0].length() > 0 && array[0].startsWith("http://"))
      //          {
      //              url = new URL(array[0]);
      //          }
      //          if (array[1].length() > 0)
      //              featureType = array[1];

      // get wfs capabiliets
      //          WFSCapabilities wfsCaps = getCapabilites(url);

      // check if the requested featureType is available on the server
      // (consistancy check of gmt-file)
      //          if (!isRequestedFTavailable(wfsCaps.getFeatureTypeList()
      //                  .getFeatureTypes(), featureType))
      //              ;
      //          throw new Exception("Requested FeatureType = " + featureType
      //                  + "is not availabel on the specified Server " + url);
      // Format (Property names of Feature, handel of the request (actual
      // system time), version of the wfs protocol, featureType, Filter )

      //          WFSQuery[] query =
      //          { WFSProtocolFactory.createQuery(null, HANDEL, VERSION,
      //                  featureType, null) };
      //
      //          WFSGetFeatureRequest getfeatureRequest = WFSProtocolFactory
      //                  .createWFSGetFeatureRequest(VERSION, featureType + HANDEL,
      //                          null, null, FORMAT, HANDEL, null, 1000, 0, query);
      //          RemoteWFService service = new RemoteWFService(wfsCaps);
      //          service.doService(getfeatureRequest);
      //          InputStream is = con.getInputStream();
      URLConnection con = url.openConnection();
      con.setDoOutput( true );
      con.setDoInput( true );

      // write request to the WFS server

      PrintStream ps = new PrintStream( con.getOutputStream() );
      ps.print( buildGetFeatureRequest() );
      ps.close();

      //        read response from the WFS server
      InputStream is = con.getInputStream();
//      StreamUtilities.streamCopy(is, new FileOutputStream(new File("D:/temp/wfs-response.xml")));
//      if(1+1==2)
//        return null;
      //Transform response (add default namespace)
      Document document = XMLTools.parse( new InputStreamReader( is ) );
      // Use the static TransformerFactory.newInstance() method to instantiate
      // a TransformerFactory. The javax.xml.transform.TransformerFactory
      // system property setting determines the actual class to instantiate --
      // org.apache.xalan.transformer.TransformerImpl.
      TransformerFactory tFactory = TransformerFactory.newInstance();

      // Use the TransformerFactory to instantiate a Transformer that will work
      // with
      // the stylesheet you specify. This method call also processes the
      // stylesheet
      // into a compiled Templates object.
      URL xsltURL = getClass().getResource( "resources/transformGetFeature.xsl" );
      // TODO this is a big hack around a deegree bug, solve this bug
      Transformer transformer = tFactory.newTransformer( new StreamSource( xsltURL.openStream() ) );

      // Use the Transformer to apply the associated Templates object to an XML
      // document
      // (foo.xml) and write the output to a file (foo.out).
      StringWriter sw = new StringWriter();
      transformer.transform( new DOMSource( document ), new StreamResult( sw ) );

      document = XMLTools.parse( new StringReader( sw.toString() ) );

      // load gml
      final GMLDocument_Impl gmlDoc = new GMLDocument_Impl( document );

      final GMLFeature gmlFeature = gmlDoc.getRootFeature();
      URL schemaURL = new URL( url
          + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName=" + featureType );
      // load schema
      final GMLSchema schema = GMLSchemaCache.getSchema( schemaURL );

      // create feature and workspace gml
      final org.kalypsodeegree.model.feature.FeatureType[] types = schema.getFeatureTypes();
      final Feature feature = FeatureFactory.createFeature( gmlFeature, types );

      // nicht die echte URL der schemaLocation, sondern dass, was im gml steht!
      final String schemaLocationName = gmlDoc.getSchemaLocationName();

      final CommandableWorkspace workspace = new CommandableWorkspace( new GMLWorkspace_Impl(
          types, feature, context, schemaLocationName, schema.getTargetNS(), schema
              .getNamespaceMap() ) );

      //      FileWriter writer = new FileWriter( "d://workspace.gml" );
      //      GmlSerializer.serializeWorkspace( writer, workspace );

      try
      {

        final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
        workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(),
            FeatureVisitor.DEPTH_INFINITE );
        workspace.accept( new ResortVisitor(), workspace.getRootFeature(),
            FeatureVisitor.DEPTH_INFINITE );

      }
      catch( final Throwable t )
      {
        t.printStackTrace();
      }
      return workspace;

    }
    catch( final LoaderException le )
    {
      le.printStackTrace();
      throw le;

    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new LoaderException( "Konnte GML von WFS nicht laden", e );
    }
    finally
    {
      monitor.done();

    }
  }

  private boolean isRequestedFTavailable( FeatureType[] availableFeatureTypes,
      String requestedFeatureType )
  {
    for( int i = 0; i < availableFeatureTypes.length; i++ )
    {
      FeatureType featureType = availableFeatureTypes[i];
      featureType.getName().equals( requestedFeatureType );
      return true;
    }
    return false;
  }

  private WFSCapabilities getCapabilites( String url )
  {
    WFSCapabilities caps = null;
    try
    {
      final URL urlGetCap = new URL( url + "?"
          + "SERVICE=WFS&VERSION=1.0.0&REQUEST=GetCapabilities" );
      final URLConnection conGetCap = urlGetCap.openConnection();
      conGetCap.addRequestProperty( "SERVICE", "WFS" );
      conGetCap.addRequestProperty( "VERSION", "1.0.0" );
      conGetCap.addRequestProperty( "REQUEST", "GetCapabilities" );
      InputStream isGetCap = conGetCap.getInputStream();
      Reader reader = new InputStreamReader( isGetCap );
      caps = WFSCapabilitiesFactory.createCapabilities( reader );
    }
    catch( MalformedURLException urle )
    {
      urle.printStackTrace();
      // TODO
      // MessageDialog urlMessage = new MessageDialog(null, "Loading WFS
      // Capabilites",null , "Fehler beim Laden des WFS Themas",
      // MessageDialog.ERROR, 0, null);
    }
    catch( IOException ioe )
    {
      ioe.printStackTrace();
      // TODO MessageDialog
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // TODO MessageDialog
    }

    return caps;
  }

  public String getDescription()
  {
    return "WFS Layer";
  }

  private String buildGetFeatureRequest()
  {
    StringBuffer sb = new StringBuffer();
    sb.append( "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>\n" );
    sb.append( "<GetFeature outputFormat=\"GML2\" xmlns:gml=\"http://www.opengis.net/gml\">\n" );
    sb.append( "<Query typeName=\"" + featureType + "\">\n" );
    sb.append( "<Filter>\n" );
    sb.append( "</Filter>\n" );
    sb.append( "</Query>" );
    sb.append( "</GetFeature>" );
    return sb.toString();
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL,
   *      org.eclipse.core.runtime.IProgressMonitor, java.lang.Object)
   */
  public void save( final String source, final URL context, final IProgressMonitor monitor,
      final Object data ) throws LoaderException
  {
    // TODO implementation of a transactional WFS
    throw new LoaderException( "Data can not be saved to a remote WFS, not implemented yet!" );
  }

  private void dumpToFile( String filename, InputStream is )
  {

    try
    {
      FileWriter fileWriterGetCap = new FileWriter( filename, true );
      int i = 0;
      while( ( i = is.read() ) >= 0 )
      {
        fileWriterGetCap.write( (char)i );
      }
      fileWriterGetCap.close();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      // TODO: handle exception
    }
  }
}