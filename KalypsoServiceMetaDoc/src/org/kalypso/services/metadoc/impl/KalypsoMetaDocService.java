package org.kalypso.services.metadoc.impl;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.util.HashMap;
import java.util.Properties;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;
import javax.jws.WebService;

import org.apache.commons.configuration.BaseConfiguration;
import org.apache.commons.configuration.Configuration;
import org.apache.commons.configuration.MapConfiguration;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang.StringUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.contribs.java.net.UrlResolverSingleton;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.impl.MetaDocException;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.metadoc.DocumentBean;
import org.kalypso.services.metadoc.IMetaDocService;
import org.kalypso.services.metadoc.PrepareBean;

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
@WebService
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
  public KalypsoMetaDocService( ) throws MetaDocException
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
  private final void init( ) throws MetaDocException
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
      m_commiter = (IMetaDocCommiter) ClassUtilities.newInstance( className, IMetaDocCommiter.class, getClass().getClassLoader() );
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( "KalypsoMetaDocService", "init", e );

      throw new MetaDocException( "Exception in init()", e );
    }
    finally
    {
      IOUtils.closeQuietly( stream );
    }
  }

  public PrepareBean prepareNewDocument( final String username ) throws MetaDocException
  {
    final String user = (username == null || username.length() == 0) ? "Autor" : username;

    try
    {
      final HashMap<Object, Object> metadata = new HashMap<Object, Object>();

      m_props.put( IMetaDocCommiter.KEY_AUTOR, user );

      m_commiter.prepareMetainf( m_props, metadata );

      return new PrepareBean( metadata );
    }
    catch( final MetaDocException e )
    {
      m_logger.log( Level.WARNING, "Error while preparing new document", e );
      throw e;
    }
  }

  public void commitNewDocument( final DocumentBean docBean, final DataHandler data ) throws MetaDocException
  {
    try
    {
      // fix: delete the whitespace from the preferredFilename
      final File docFile = File.createTempFile( "document", StringUtils.deleteWhitespace( docBean.getPreferredFilename() ), m_tmpDir );
      FileUtilities.makeFileFromStream( false, docFile, data.getInputStream() );

      final Configuration mdConf;
      if( docBean.getMetadataExtensions() != null )
        mdConf = new MapConfiguration( docBean.getMetadataExtensions() );
      else
        mdConf = new BaseConfiguration();

      m_commiter.commitDocument( m_props, docBean.getMetadata(), docFile, docBean.getDocumentIdentifier(), docBean.getDocumentCategory(), mdConf );
    }
    catch( final IOException e )
    {
      m_logger.throwing( "KalypsoMetaDocService", "commitNewDocument", e );

      throw new MetaDocException( "commitNewDocument", e );
    }
  }

  public int getServiceVersion( )
  {
    return 0;
  }
}