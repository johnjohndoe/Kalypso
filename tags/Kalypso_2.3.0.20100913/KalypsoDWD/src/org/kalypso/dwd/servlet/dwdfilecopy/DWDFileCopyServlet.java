package org.kalypso.dwd.servlet.dwdfilecopy;

import java.io.File;
import java.util.Timer;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.apache.commons.vfs.cache.DefaultFilesCache;
import org.apache.commons.vfs.impl.DefaultFileSystemManager;
import org.apache.commons.vfs.provider.ftp.FtpFileProvider;
import org.apache.commons.vfs.provider.local.DefaultLocalFileProvider;
import org.apache.commons.vfs.provider.url.UrlFileProvider;

/**
 * Handles the copy of DWD-Files from a source folder to some dest folder.
 */
public class DWDFileCopyServlet extends HttpServlet
{
  /**
   * amount of DWDCopyTask instances that should be started <br>
   * optional, default is 1
   */
  public final static String PARAM_INSTANCES = "instances";

  /**
   * time period between DWD-Copy loops in milliseconds (semicolon separated if more than one instance) <br>
   * required
   */
  public final static String PARAM_PERIOD = "period";

  /**
   * source path of the directory where dwd files are located (semicolon separated if more than one instance) <br>
   * required
   */
  public final static String PARAM_SOURCE_PATH = "source";

  /**
   * prefix used for filtering files from source folder (semicolon separated if more than one instance) <br>
   * required
   */
  public final static String PARAM_SOURCE_PREFIX = "source_prefix";

  /**
   * format of the filenames in order to extract date from it (semicolon separated if more than one instance) <br>
   * required
   */
  public final static String PARAM_SOURCE_FORMAT = "source_format";

  /**
   * whether source dwd-files should be deleted or not once handled (semicolon separated if more than one instance) <br>
   * optional, default is false
   */
  public final static String PARAM_SOURCE_DELETE = "source_delete";

  /**
   * dest path of the directory where to copy the dwd files (semicolon separated if more than one instance) <br>
   * required
   */
  public final static String PARAM_DEST_FILENAME = "dest";

  /**
   * whether contents of the dest file should be updated or not, with the date of the DWD forecast file (semicolon
   * separated if more than one instance) <br>
   * optional, default is false
   */
  public final static String PARAM_DEST_UPDATE = "dest_update";

  /**
   * whether more debug output should be produced. Can be 'true' or 'false'. Only ONE parameter is supported.
   */
  public final static String PARAM_DEBUG = "debug";

  protected static final Logger LOG = Logger.getLogger( DWDFileCopyServlet.class.getName() );

  private Timer[] m_timers = new Timer[0];

  private int instances;

  private String[] periods;

  private String[] sources;

  private String[] sources_prefix;

  private String[] sources_format;

  private String[] sources_delete;

  private String[] dests;

  private String[] dests_update;
  private String shall_debug;

  public DWDFileCopyServlet( )
  {
    super();

    LOG.info( "Constructor called" );
  }

  @Override
  public void init( ) throws ServletException
  {
    super.init();

    LOG.info( "Starting, reading parameters..." );

    readParameters();

    LOG.info( "Preparing start of " + instances + " instance(s) of DWDCopyTask..." );

    try
    {
      m_timers = new Timer[instances];
      for( int i = 0; i < m_timers.length; i++ )
      {
        LOG.info( "Starting timer #" + i + " with parameters: PERIOD=" + periods[i] + " SOURCE_PATH=" + sources[i] + " PREFIX=" + sources_prefix[i] + " FORMAT=" + sources_format[i] + " DELETE="
            + sources_delete[i] + " DEST_FILENAME=" + dests[i] + " DEST_UPDATE=" + dests_update[i] );

        final int period = Integer.valueOf( periods[i] ).intValue();
        final String srcFormat = sources_format[i];
        final boolean srcDel = Boolean.valueOf( sources_delete[i] ).booleanValue();
        final File destName = new File( dests[i] );
        final boolean destUpdate = Boolean.valueOf( dests_update[i] ).booleanValue();
        final boolean debug = Boolean.valueOf( shall_debug ).booleanValue();

        /* Initialize the DefaultFileSystemManager. */
        DWDFileCopyServlet.LOG.info( "Init FileSystemManager ... " );

        /* FileSystemManager initialisieren. */
        final DefaultFileSystemManager fsManager = new DefaultFileSystemManager();

        fsManager.addProvider( "file", new DefaultLocalFileProvider() );
        fsManager.addProvider( "ftp", new FtpFileProvider() );
        fsManager.setDefaultProvider( new UrlFileProvider() );
        fsManager.setFilesCache( new DefaultFilesCache() );
        fsManager.init();

        m_timers[i] = new Timer( true );
        m_timers[i].schedule( new DWDCopyTask( sources[i], fsManager, srcFormat, srcDel, destName, destUpdate, debug ), 0, period );

        LOG.info( "Timer #" + i + " started." );
      }
    }
    catch( Exception e )
    {
      LOG.warning("Error: " + e.getLocalizedMessage());
    }
  }

  /**
   * Read the parameters and handle optional ones
   */
  private void readParameters( )
  {
    final String pInstances = getInitParameter( PARAM_INSTANCES );
    if( pInstances != null )
      instances = Integer.valueOf( pInstances ).intValue();
    else
      instances = 1;

    periods = getInitParameter( PARAM_PERIOD ).split( ";", instances );
    sources = getInitParameter( PARAM_SOURCE_PATH ).split( ";", instances );
    sources_prefix = getInitParameter( PARAM_SOURCE_PREFIX ).split( ";", instances );
    sources_format = getInitParameter( PARAM_SOURCE_FORMAT ).split( ";", instances );

    final String pSrcDelete = getInitParameter( PARAM_SOURCE_DELETE );
    if( pSrcDelete != null )
      sources_delete = pSrcDelete.split( ";", instances );
    else
    {
      sources_delete = new String[instances];
      java.util.Arrays.fill( sources_delete, "false" );
    }

    dests = getInitParameter( PARAM_DEST_FILENAME ).split( ";", instances );

    final String pDestUpdate = getInitParameter( PARAM_DEST_UPDATE );
    if( pDestUpdate != null )
      dests_update = pDestUpdate.split( ";", instances );
    else
    {
      dests_update = new String[instances];
      java.util.Arrays.fill( dests_update, "false" );
    }
    
    shall_debug = getInitParameter( PARAM_DEBUG );
  }

  @Override
  public void destroy( )
  {
    LOG.info( "Stopping... Cancelling " + m_timers.length + " timer(s)..." );

    for( int i = 0; i < m_timers.length; i++ )
    {
      m_timers[i].cancel();
      LOG.info( "Timer #" + i + " cancelled." );
    }

    super.destroy();
  }
}