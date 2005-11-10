package org.kalypso.services.metadoc.impl;

import java.io.File;
import java.io.InputStream;
import java.io.StringReader;
import java.net.URL;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;

import org.apache.commons.configuration.PropertiesConfiguration;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.commons.java.net.UrlResolverSingleton;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.metadoc.IMetaDocService;

/**
 * Serverside business for the metadoc framework.
 * <p>
 * The properties that can be defined in the configuration file are:
 * <ul>
 * <li>COMMITER (required) contains the full classname of the commiter to use
 * <li>COMMITER.DIR (optional) when present, this is the directory where files are created. It is up to the
 * implementation to delete the file once document has been commited. If not present in the configuration, a default
 * temporary directory is created, and deleted once the jvm shuts down.
 * 
 * @author schlienger
 */
public class KalypsoMetaDocService implements IMetaDocService
{
  private final static String PROP_COMMITER = "COMMITER";
  private final static String PROP_COMMITER_DIR = "COMMITER.DIR";

  private final File m_tmpDir;

  private final Logger m_logger;

  private final Properties m_props = new Properties();

  private IMetaDocCommiter m_commiter;

  /**
   * Constructs service instance
   */
  public KalypsoMetaDocService() throws RemoteException
  {
    m_logger = Logger.getLogger( KalypsoMetaDocService.class.getName() );

    init();

    // directory where documents are temporarely stored can be specified in
    // the properties of the service with the property COMMITER.DIR
    if( m_props.getProperty( PROP_COMMITER_DIR ) != null )
      m_tmpDir = new File( m_props.getProperty( PROP_COMMITER_DIR ) );
    else
    {
      // if not specified, we create a default temp-dir in the service temp dir
      // which is deleted once the jvm stops
      m_tmpDir = FileUtilities.createNewTempDir( "Documents", ServiceConfig.getTempDir() );
      m_tmpDir.deleteOnExit();
    }
  }

  /**
   * Initialize this service
   */
  private final void init() throws RemoteException
  {
    InputStream stream = null;
    try
    {
      final URL confLocation = ServiceConfig.getConfLocation();
      final URL confUrl = UrlResolverSingleton.resolveUrl( confLocation, "IMetaDocService/metadocService.properties" );
      stream = confUrl.openStream();

      m_props.load( stream );

      // try to instanciate our commiter
      final String className = m_props.getProperty( PROP_COMMITER );
      m_commiter = (IMetaDocCommiter)ClassUtilities.newInstance( className, IMetaDocCommiter.class, getClass()
          .getClassLoader() );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( "KalypsoMetaDocService", "init", e );

      throw new RemoteException( "Exception in init()", e );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  /**
   * @see org.kalypso.services.metadoc.IMetaDocService#prepareNewDocument(java.lang.String)
   */
  public Map prepareNewDocument( final String username ) throws RemoteException
  {
    final String user = ( username == null || username.length() == 0 ) ? "Autor" : username;

    try
    {
      final Map metadata = new HashMap();

      m_props.put( IMetaDocCommiter.KEY_AUTOR, user );

      m_commiter.prepareMetainf( m_props, metadata );

      return metadata;
    }
    catch( final MetaDocException e )
    {
      m_logger.log( Level.WARNING, "Error while preparing new document", e );
      throw new RemoteException( "Error while preparing new document", e );
    }
  }

  /**
   * @see org.kalypso.services.metadoc.IMetaDocService#commitNewDocument(java.util.Map, javax.activation.DataHandler,
   *      java.lang.String, java.lang.String)
   */
  public void commitNewDocument( final Map metadata, final DataHandler data, final String preferredFilename,
      final String metadataExtensions ) throws RemoteException
  {
    try
    {
      // fix: delete the whitespace from the preferredFilename
      final File docFile = File.createTempFile( "document", StringUtils.deleteWhitespace( preferredFilename ), m_tmpDir );
      FileUtilities.makeFileFromStream( false, docFile, data.getInputStream() );

      final PropertiesConfiguration mExConf = new PropertiesConfiguration();
      if( metadataExtensions != null )
      {
        final StringReader reader = new StringReader( metadataExtensions );
        mExConf.load( reader );
        reader.close();
      }

      m_commiter.commitDocument( m_props, metadata, docFile, mExConf );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( "KalypsoMetaDocService", "commitNewDocument", e );

      throw new RemoteException( "commitNewDocument", e );
    }
  }

  /**
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion()
  {
    return 0;
  }
}