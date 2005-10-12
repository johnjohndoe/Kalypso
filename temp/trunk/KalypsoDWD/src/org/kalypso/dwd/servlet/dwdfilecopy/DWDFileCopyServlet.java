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
import org.kalypso.dwd.DWDRasterHelper;

/**
 * Handles the copy of DWD-Files from a source folder to some dest folder.
 */
public class DWDFileCopyServlet extends HttpServlet
{
  /** amount of DWDCopyTask instances that should be started */
  public final static String PARAM_INSTANCES = "instances";

  /** time period between DWD-Copy loops in milliseconds (semicolon separated if more than one instance) */
  public final static String PARAM_PERIOD = "period";

  /** source path of the directory where dwd files are located (semicolon separated if more than one instance) */
  public final static String PARAM_SOURCE_PATH = "source";

  /** prefix used for filtering files from source folder (semicolon separated if more than one instance) */
  public final static String PARAM_SOURCE_PREFIX = "source_prefix";
  
  /** format of the filenames in order to extract date from it (semicolon separated if more than one instance) */
  public final static String PARAM_SOURCE_FORMAT = "source_format";
  
  /** whether source dwd-files should be deleted once handled or not (semicolon separated if more than one instance) */
  public final static String PARAM_SOURCE_DELETE = "source_delete";
  
  /** dest path of the directory where to copy the dwd files  (semicolon separated if more than one instance) */
  public final static String PARAM_DEST_FILENAME = "dest";

  private static final Logger LOG = Logger.getLogger( DWDFileCopyServlet.class.getName() );

  private Timer[] m_timers = new Timer[0];

  public DWDFileCopyServlet()
  {
    super();
    
    LOG.info( "Constructor called" );
  }
  
  public void init() throws ServletException
  {
    super.init();
    
    LOG.info( "Starting, reading parameters..." );

    final int instances = Integer.valueOf( getInitParameter( PARAM_INSTANCES ) ).intValue();
    final String[] periods = getInitParameter( PARAM_PERIOD ).split( ";", instances );
    final String[] sources = getInitParameter( PARAM_SOURCE_PATH ).split( ";", instances );
    final String[] sources_prefix = getInitParameter( PARAM_SOURCE_PREFIX ).split( ";", instances );
    final String[] sources_format = getInitParameter( PARAM_SOURCE_FORMAT ).split( ";", instances );
    final String[] sources_delete = getInitParameter( PARAM_SOURCE_DELETE ).split( ";", instances );
    final String[] dests = getInitParameter( PARAM_DEST_FILENAME ).split( ";", instances );

    LOG.info( "Preparing start of " + instances + " instance(s) of DWDCopyTask..." );

    m_timers = new Timer[instances];
    for( int i = 0; i < m_timers.length; i++ )
    {
      final int period = Integer.valueOf( periods[i] ).intValue();
      final File srcDir = new File( sources[i] );
      final String srcPrefix = sources_prefix[i];
      final String srcFormat = sources_format[i];
      final boolean srcDel = Boolean.valueOf( sources_delete[i] ).booleanValue();
      final File destName = new File( dests[i] );

      LOG.info( "Starting timer #" + i + " with parameters: PERIOD=" + period + " SOURCE_PATH=" + sources[i]
          + " DEST_PATH=" + dests[i] );

      m_timers[i] = new Timer( true );
      m_timers[i].schedule( new DWDCopyTask( srcDir, destName, srcPrefix, srcFormat, srcDel ), 0, period );

      LOG.info( "Timer #" + i + " started." );
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

    public DWDCopyTask( final File srcDir, final File destDir, final String srcPrefix, final String srcFormat, final boolean srcDel )
    {
      m_srcDir = srcDir;
      m_destFile = destDir;
      m_srcPrefix = srcPrefix;
      m_srcDel = srcDel;
      m_dateFormat = new SimpleDateFormat( srcFormat );
    }

    public void run()
    {
      final File file = DWDRasterHelper.getNewestFile( m_srcDir, m_srcPrefix, m_dateFormat, m_srcDel );
      if( file == null )
        return;

      // update file contents
      // TODO...in the test case of psi, update dates

      // looping twice over this code in the case an exception
      // occurs, we try it again...
      for( int i = 0; i < 2; i++ )
      {
        try
        {
          // check if dest file is up to date
          // TODO... compare file date
          
          LOG.info( "Copying DWD-File \"" + file.getName() + "\" to: " + m_destFile.getAbsolutePath() );
          FileUtils.copyFile( file, m_destFile );

          LOG.info( "Deleting DWD-File..." );
          boolean deleted = file.delete();
          if( !deleted )
            LOG.warning( "Could not delete DWD-File \"" + file.getName() + "\"" );

          // no exception, so end loop here
          return;
        }
        catch( final IOException e )
        {
          LOG.warning( "Could not copy DWD-File \"" + file.getName() + "\" to folder: " + m_destFile.getAbsolutePath()
              + " due to: " + e.getLocalizedMessage() );
        }
      }
    }
  }
}