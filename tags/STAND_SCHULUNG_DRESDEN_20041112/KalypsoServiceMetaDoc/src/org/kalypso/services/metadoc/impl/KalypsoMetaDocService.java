package org.kalypso.services.metadoc.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Logger;

import org.apache.commons.io.IOUtils;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.java.lang.reflect.ClassUtilities;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.beans.DocBean;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.metadoc.IMetaDocService;

/**
 * MetaDocService
 * 
 * @author schlienger
 */
public class KalypsoMetaDocService implements IMetaDocService
{
  private final static String PROP_PUBLISH_DIR = "PUBLISH_DIR";
  private final static String PROP_COMMITER = "COMMITER";

  private final File m_tmpDir;
  
  private final Logger m_logger;

  private final Properties m_props = new Properties();
  
  private IMetaDocCommiter m_commiter;

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

    m_tmpDir = FileUtilities.createNewTempDir( "Documents", ServiceConfig.getTempDir() );
    m_tmpDir.deleteOnExit();

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

    InputStream stream = null;
    try
    {
      stream = new FileInputStream( conf );

      m_props.load( stream );
      
      // try to instanciate our commiter
      final String className = m_props.getProperty( PROP_COMMITER );
      m_commiter = (IMetaDocCommiter) ClassUtilities.newInstance( className, IMetaDocCommiter.class, getClass().getClassLoader() );
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
      f = File.createTempFile( "doc", extension, m_tmpDir );
      m_logger.info( "preparing file: " + f.getAbsolutePath() );

      final DocBean db = new DocBean( f.getAbsolutePath() );
      
      m_commiter.prepareMetainf( m_props, db );
      
      return db;
    }
    catch( final Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( "KalypsoMetaDocService", "prepareNewDocument", e );

      throw new RemoteException( "prepareNewDocument", e );
    }
  }

  /**
   * @see org.kalypso.services.metadoc.IMetaDocService#rollbackNewDocument(org.kalypso.metadoc.beans.DocBean)
   */
  public void rollbackNewDocument( final DocBean mdb )
  {
    final File f = new File( mdb.getLocation() );
    
    if( f.exists() )
      f.delete();
  }
  
  /**
   * @see org.kalypso.services.metadoc.IMetaDocService#commitNewDocument(org.kalypso.metadoc.beans.DocBean)
   */
  public void commitNewDocument( final DocBean mdb ) throws RemoteException
  {
    try
    {
      final File docFile = new File( mdb.getLocation() );
      
      m_commiter.commitDocument( m_props, mdb );
      
      // delete temp file
      docFile.delete();
    }
    catch( Exception e ) // generic exception caught for simplicity
    {
      m_logger.throwing( "KalypsoMetaDocService", "commitNewDocument", e );

      throw new RemoteException( "commitNewDocument", e );
    }
  }

  /**
   * @see org.kalypso.services.IKalypsoService#getServiceVersion()
   */
  public int getServiceVersion( )
  {
    return 0;
  }
}