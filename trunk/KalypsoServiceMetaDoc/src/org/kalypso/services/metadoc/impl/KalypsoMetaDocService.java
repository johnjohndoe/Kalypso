package org.kalypso.services.metadoc.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.metadoc.DocBean;
import org.kalypso.services.metadoc.IMetaDocService;

/**
 * MetaDocService
 * 
 * @author schlienger
 */
public class KalypsoMetaDocService implements IMetaDocService
{
  private final static String PROP_PUBLISH_DIR = "PUBLISH_DIR";

  private final Logger m_logger;

  private File m_publishDir;

  /**
   * Constructs service instance
   * 
   * @throws RemoteException
   */
  public KalypsoMetaDocService( ) throws RemoteException
  {
    m_logger = Logger.getLogger( KalypsoMetaDocService.class.getName() );

    try
    {
      m_logger.addHandler( new FileHandler( ServiceConfig.getTempDir()
          + "/IMetaDocService%g.log", 10000000, 10, true ) );
    }
    catch( Exception e ) // generic Exception caught for simplicity
    {
      e.printStackTrace();
      throw new RemoteException(
          "Exception in KalypsoMetaDocService.constructor()", e );
    }

    init();
  }

  /**
   * initialize this service
   * 
   * @throws RemoteException
   */
  private void init( ) throws RemoteException
  {
    final File conf = new File( ServiceConfig.getConfDir(),
        "IMetaDocService/metadocService.properties" );

    final Properties props;
    InputStream stream = null;
    try
    {
      stream = new FileInputStream( conf );

      props = new Properties();
      props.load( stream );

      final String path = props.getProperty( PROP_PUBLISH_DIR );
      if( path == null )
        throw new IllegalStateException("Invalid configuration: publish-directory is null");
      
      m_publishDir = new File( path );
      if( !m_publishDir.exists() )
      {
        m_logger.warning( "Publish-directory <" + path + "> doesn't exist. Will try to create it." );
        boolean b = m_publishDir.mkdirs();
        
        if( !b )
          throw new IllegalStateException( "Invalid configuration: publish-directory does not exist and could not be created: " + path );
        
        m_logger.info( "Publish-directory <" + path + "> successfully created." );
      }
    }
    catch( Exception e ) // generic exception caught for simplicity
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
  public DocBean prepareNewDocument( String extension ) throws RemoteException
  {
    if( extension == null || extension.length() == 0 )
      extension = "tmp";
    
    final File f;
    try
    {
      f = File.createTempFile( "doc", extension, m_publishDir );

      final DocBean db = new DocBean( f.getAbsolutePath() );
      
      return db;
    }
    catch( IOException e )
    {
      m_logger.throwing( "KalypsoMetaDocService", "prepareNewDocument", e );

      throw new RemoteException( "prepareNewDocument", e );
    }
  }

  /**
   * @see org.kalypso.services.metadoc.IMetaDocService#commitNewDocument(org.kalypso.services.metadoc.DocBean)
   */
  public void commitNewDocument( final DocBean mdb ) throws RemoteException
  {
    // TODO Auto-generated method stub

  }
}