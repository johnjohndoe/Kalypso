package org.kalypso.services.metadoc.impl;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.logging.FileHandler;
import java.util.logging.Level;
import java.util.logging.Logger;

import javax.activation.DataHandler;

import org.apache.commons.io.IOUtils;
import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.contribs.java.lang.reflect.ClassUtilities;
import org.kalypso.metadoc.IMetaDocCommiter;
import org.kalypso.metadoc.MetaDocException;
import org.kalypso.services.common.ServiceConfig;
import org.kalypso.services.metadoc.IMetaDocService;

/**
 * MetaDocService
 * 
 * @author schlienger
 */
public class KalypsoMetaDocService implements IMetaDocService
{
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
  public KalypsoMetaDocService() throws RemoteException
  {
    m_logger = Logger.getLogger( KalypsoMetaDocService.class.getName() );

    try
    {
      m_logger.addHandler( new FileHandler( ServiceConfig.getTempDir() + "/IMetaDocService%g.log", 10000000, 1, true ) );
    }
    catch( Exception e ) // generic Exception caught for simplicity
    {
      e.printStackTrace();
      throw new RemoteException( "Exception in KalypsoMetaDocService.constructor()", e );
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
  private void init() throws RemoteException
  {
    final File conf = new File( ServiceConfig.getConfDir(), "IMetaDocService/metadocService.properties" );

    InputStream stream = null;
    try
    {
      stream = new FileInputStream( conf );

      m_props.load( stream );

      // try to instanciate our commiter
      final String className = m_props.getProperty( PROP_COMMITER );
      m_commiter = (IMetaDocCommiter)ClassUtilities.newInstance( className, IMetaDocCommiter.class, getClass()
          .getClassLoader() );
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
   *      java.lang.String)
   */
  public void commitNewDocument( final Map metadata, final DataHandler data, final String fileExtension )
      throws RemoteException
  {
    try
    {
      final File docFile = File.createTempFile( "metadoc-tmp", fileExtension, m_tmpDir );
      FileUtilities.makeFileFromStream( false, docFile, data.getInputStream() );

      m_commiter.commitDocument( m_props, metadata, docFile );
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
  public int getServiceVersion()
  {
    return 0;
  }
}