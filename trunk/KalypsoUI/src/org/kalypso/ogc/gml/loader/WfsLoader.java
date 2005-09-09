package org.kalypso.ogc.gml.loader;

import java.io.BufferedInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.Reader;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLConnection;
import java.util.Properties;

import org.apache.commons.io.IOUtils;
import org.deegree.services.capabilities.DCPType;
import org.deegree.services.capabilities.HTTP;
import org.deegree.services.capabilities.Protocol;
import org.deegree.services.wfs.capabilities.Capability;
import org.deegree.services.wfs.capabilities.GetFeature;
import org.deegree.services.wfs.capabilities.Request;
import org.deegree.services.wfs.capabilities.WFSCapabilities;
import org.deegree_impl.services.wfs.capabilities.WFSCapabilitiesFactory;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.dialogs.SaveAsDialog;
import org.kalypso.commons.java.util.PropertiesHelper;
import org.kalypso.loader.AbstractLoader;
import org.kalypso.loader.LoaderException;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypso.ui.ImageProvider;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypsodeegree.model.feature.FeatureVisitor;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree_impl.model.feature.visitors.ResortVisitor;
import org.kalypsodeegree_impl.model.feature.visitors.TransformVisitor;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * 
 * @author Kuepferle
 */
public class WfsLoader extends AbstractLoader
{
  private String m_featureType;

  private URL m_schemaURL;

  private boolean m_postProtocol = false;

  private boolean m_getProtocol = false;

  private URL m_url = null;

  /**
   * Loads a WFS DataSource from the given URL
   * 
   * @param source
   *          the href-tag from the gmt-file 'ex: http://localhost:8080/deegreewfs#river' where river denotes the
   *          feature to be loaded
   * @param context
   *          the URL form the map context (here the path to the associated gmt file)
   */
  protected Object loadIntern( String source, URL context, IProgressMonitor monitor ) throws LoaderException
  {
    BufferedInputStream inputStream = null;
    PrintStream ps = null;
    try
    {
      monitor.beginTask( "WFS laden", 1000 );
      final Properties sourceProps = PropertiesHelper.parseFromString( source, '#' );
      final String path = sourceProps.getProperty( "URL" );

      m_featureType = sourceProps.getProperty( "FEATURE" );

      if( path != null )
      {
        m_url = new URL( path );
      }
      m_schemaURL = new URL( m_url + "?SERVICE=WFS&VERSION=1.0.0&REQUEST=DescribeFeatureType&typeName=" + m_featureType );
//      GMLSchema schema = GMLSchemaCatalog.getSchema( m_schemaURL );

      //
      //          if (array[0].length() > 0 && array[0].startsWith("http://"))
      //          {
      //              url = new URL(array[0]);
      //          }
      //          if (array[1].length() > 0)
      //              featureType = array[1];
      //TODO put capabilites into a file cache (similar to GMLSchemaCache ) -> performance
      //       get wfs capabiliets to check which protocol types are supported by the service
      final WFSCapabilities wfsCaps = getCapabilites( m_url );
      final Capability capability = wfsCaps.getCapability();
      final Request request = capability.getRequest();
      final GetFeature getFeature = request.getGetFeature();
      final DCPType[] type = getFeature.getDCPType();
      for( int i = 0; i < type.length; i++ )
      {
        final DCPType dcpt = type[i];
        final Protocol protocol = dcpt.getProtocol();
        if( protocol instanceof HTTP )
        {
          final URL[] getOnlineResources = ( (HTTP)protocol ).getGetOnlineResources();
          final URL[] postOnlineResources = ( (HTTP)protocol ).getPostOnlineResources();
          if( getOnlineResources.length > 0 )
            m_getProtocol = true;
          if( postOnlineResources.length > 0 )
            m_postProtocol = true;
        }
      }

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
      final URLConnection con = m_url.openConnection();
      con.setDoOutput( true );
      con.setDoInput( true );

      // write request to the WFS server
      ps = new PrintStream( con.getOutputStream() );
      ps.print( buildGetFeatureRequestPOST() );
      //      if( m_getProtocol )
      //      {
      //        ps.print( buildGetFeatureRequestGET() );
      //      }
      //      else
      //      {
      //        ps.print( buildGetFeatureRequestPOST() );
      //      }

      //read response from the WFS server and create a GMLWorkspace
      inputStream = new BufferedInputStream( con.getInputStream() );
//      writeInputStreamToFile( "d://temp//getfeature_deegree_wfs.xml", inputStream );
      final GMLWorkspace workspace = GmlSerializer.createGMLWorkspace( inputStream, m_schemaURL );
      inputStream.close();

      final CS_CoordinateSystem targetCRS = KalypsoGisPlugin.getDefault().getCoordinatesSystem();
      workspace.accept( new TransformVisitor( targetCRS ), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );
      workspace.accept( new ResortVisitor(), workspace.getRootFeature(), FeatureVisitor.DEPTH_INFINITE );

      return new CommandableWorkspace( workspace );
    }
    catch( IOException e )
    {
      e.printStackTrace();
      throw new LoaderException( "Der InputStream is korumpiert. GML kann nicht geladen werden" );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      //      tryToLoadAsFeatureCollection();
      throw new LoaderException( "Konnte GML von WFS nicht laden", e );
    }
    finally
    {
      monitor.done();
      IOUtils.closeQuietly( ps );
      IOUtils.closeQuietly( inputStream );
    }
  }

  private WFSCapabilities getCapabilites( URL url )
  {
    WFSCapabilities caps = null;
    try
    {
      final URL urlGetCap = new URL( url + "?" + "SERVICE=WFS&VERSION=1.0.0&REQUEST=GetCapabilities" );
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

  private String buildGetFeatureRequestPOST()
  {
    StringBuffer sb = new StringBuffer();
    sb.append( "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n" );// iso-8859-1 
    sb.append( "<GetFeature outputFormat=\"GML2\" xmlns:gml=\"http://www.opengis.net/gml\">\n" );
    sb.append( "<Query typeName=\"" + m_featureType + "\">\n" );
    sb.append( "<Filter>\n" );
    sb.append( "</Filter>\n" );
    sb.append( "</Query>" );
    sb.append( "</GetFeature>" );
    return sb.toString();
  }

  /**
   * @return
   */
  private String buildGetFeatureRequestGET()
  {
    return m_url + "?REQUEST=GetFeature&SERVICE=wfs&VERSION=1.0.0&typename=" + m_featureType;
  }

  /**
   * @see org.kalypso.loader.ILoader#save(java.lang.String, java.net.URL, org.eclipse.core.runtime.IProgressMonitor,
   *      java.lang.Object)
   */
  public void save( final String source, final URL context, final IProgressMonitor monitor, final Object data )
  {
    // TODO implementation of a transactional WFS
    if( data instanceof CommandableWorkspace )
    {
      Display display = new Display();
      MessageDialog md = new MessageDialog( new Shell( display ), "Speichern der Daten vom WFS",
          ( ImageProvider.IMAGE_STYLEEDITOR_SAVE                           ).createImage(), "Sollen die Daten Lokal gespeichrt werden?",
          MessageDialog.QUESTION, new String[]
          {
              "Ja",
              "Nein" }, 0 );
      int result = md.open();
      try
      {
        if( result == 0 )
        {
          IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
          SaveAsDialog dialog = new SaveAsDialog( new Shell( display ) );
          dialog.open();
          IPath targetPath = root.getLocation().append( dialog.getResult() );
          //          IFile file = root.getFile( targetPath );
          FileWriter fw = new FileWriter( targetPath.toFile().toString() );
          GmlSerializer.serializeWorkspace( fw, (GMLWorkspace)data );
          ResourcesPlugin.getWorkspace().getRoot().refreshLocal( IResource.DEPTH_INFINITE, monitor );
        }
        else if( result == 1 )
        {

          MessageDialog.openError( new Shell( display ), "Operation not supported",
              "Saving a feature at a remote location is not supported" );
        }

      }
      catch( IOException e )
      {
        e.printStackTrace();
      }
      catch( GmlSerializeException e )
      {
        e.printStackTrace();
      }
      catch( CoreException e )
      {
        e.printStackTrace();
      }
      finally
      {
        display.dispose();
      }
    }
  }

  // not used
  // better use: ioutils.copy( );
  // and close stream in finally block
  /**
   * This method is just for debuging, to write an imput stream to a file
   */
  private void writeInputStreamToFile( String filename, InputStream is )
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