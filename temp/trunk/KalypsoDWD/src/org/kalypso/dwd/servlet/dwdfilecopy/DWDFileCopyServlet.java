package org.kalypso.dwd.servlet.dwdfilecopy;

import java.io.File;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.Timer;
import java.util.TimerTask;
import java.util.logging.Logger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;

import org.apache.commons.io.FileUtils;
import org.kalypso.dwd.DWDException;
import org.kalypso.dwd.DWDRasterHelper;

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

  private static final Logger LOG = Logger.getLogger( DWDFileCopyServlet.class.getName() );

  private Timer[] m_timers = new Timer[0];

  private int instances;
  private String[] periods;
  private String[] sources;
  private String[] sources_prefix;
  private String[] sources_format;
  private String[] sources_delete;
  private String[] dests;
  private String[] dests_update;

  public DWDFileCopyServlet()
  {
    super();

    LOG.info( "Constructor called" );
  }

  public void init() throws ServletException
  {
    super.init();

    LOG.info( "Starting, reading parameters..." );

    readParameters();

    LOG.info( "Preparing start of " + instances + " instance(s) of DWDCopyTask..." );

    m_timers = new Timer[instances];
    for( int i = 0; i < m_timers.length; i++ )
    {
      LOG.info( "Starting timer #" + i + " with parameters: PERIOD=" + periods[i] + " SOURCE_PATH=" + sources[i]
          + " PREFIX=" + sources_prefix[i] + " FORMAT=" + sources_format[i] + " DELETE=" + sources_delete[i]
          + " DEST_FILENAME=" + dests[i] + " DEST_UPDATE=" + dests_update[i] );

      final int period = Integer.valueOf( periods[i] ).intValue();
      final File srcDir = new File( sources[i] );
      final String srcPrefix = sources_prefix[i];
      final String srcFormat = sources_format[i];
      final boolean srcDel = Boolean.valueOf( sources_delete[i] ).booleanValue();
      final File destName = new File( dests[i] );
      final boolean destUpdate = Boolean.valueOf( dests_update[i] ).booleanValue();

      m_timers[i] = new Timer( true );
      m_timers[i].schedule( new DWDCopyTask( srcDir, srcPrefix, srcFormat, srcDel, destName, destUpdate ), 0, period );

      LOG.info( "Timer #" + i + " started." );
    }
  }

  /**
   * Read the parameters and handle optional ones
   */
  private void readParameters()
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
  }

  public void destroy()
  {
    LOG.info( "Stopping... Cancelling " + m_timers.length + " timer(s)..." );

    for( int i = 0; i < m_timers.length; i++ )
    {
      m_timers[i].cancel();
      LOG.info( "Timer #" + i + " cancelled." );
    }

    super.destroy();
  }

  /**
   * Handles the copy of the dwd files
   */
  private static class DWDCopyTask extends TimerTask
  {
    private final File m_srcDir;
    private final File m_destFile;
    private final String m_srcPrefix;
    private final SimpleDateFormat m_dateFormat;
    private final boolean m_srcDel;
    private final boolean m_destUpdate;

    public DWDCopyTask( final File srcDir, final String srcPrefix, final String srcFormat, final boolean srcDel,
        final File destName, final boolean destUpdate )
    {
      m_srcDir = srcDir;
      m_srcPrefix = srcPrefix;
      m_srcDel = srcDel;
      m_dateFormat = new SimpleDateFormat( srcFormat );
      m_destFile = destName;
      m_destUpdate = destUpdate;
    }

    public void run()
    {
      final File file = DWDRasterHelper.getNewestFile( m_srcDir, m_srcPrefix, m_dateFormat, m_srcDel );
      if( file == null )
        return;

      // looping twice over this code in the case an exception
      // occurs, we try it again...
      for( int i = 0; i < 2; i++ )
      {
        try
        {
          // if dest file either does not exist or is not up to date, overwrite with current DWD forecast
          if( !m_destFile.exists() || m_destFile.lastModified() < file.lastModified() )
          {
            final File dwdDest;
            if( m_destUpdate )
              dwdDest = new File( m_destFile.getParentFile(), file.getName() );
            else
              dwdDest = m_destFile;
           
            LOG.info( "Copying DWD-File \"" + file.getName() + "\" to: " + dwdDest.getAbsolutePath() );
            FileUtils.copyFile( file, dwdDest );
            
            // update file contents
            if( m_destUpdate )
            {
              LOG.info( "Updating " + m_destFile.getName() + " from " + dwdDest );
              DWDRasterHelper.updateDWDFileContents( dwdDest, m_destFile, m_dateFormat );
              
              m_destFile.setLastModified( file.lastModified() );
              
              final boolean deleted = dwdDest.delete();
              if( !deleted )
                LOG.warning( "Could not delete temp DWD-File \"" + dwdDest.getName() + "\"" );
            }
          }

          // delete source file if flag is set
          if( m_srcDel )
          {
            LOG.info( "Deleting " + file.getName() );
            final boolean deleted = file.delete();
            if( !deleted )
              LOG.warning( "Could not delete DWD-File \"" + file.getName() + "\"" );
          }

          // no exception, so end loop here
          return;
        }
        catch( final IOException e )
        {
          LOG.warning( "Could not copy DWD-File \"" + file.getName() + "\" to folder: " + m_destFile.getAbsolutePath()
              + " due to: " + e.getLocalizedMessage() );
        }
        catch( final DWDException e )
        {
          LOG.warning( "DWD-File could not be updated: " + e.getLocalizedMessage() );
        }
        
        try
        {
          // make some pause before continuing
          Thread.sleep( 500 );
        }
        catch( final InterruptedException ignored )
        {
          // empty
        }
      }
    }
  }
}