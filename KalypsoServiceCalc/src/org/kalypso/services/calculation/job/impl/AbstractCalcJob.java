package org.kalypso.services.calculation.job.impl;

import java.io.File;
import java.io.IOException;
import java.util.Collection;
import java.util.Vector;
import java.util.logging.Logger;

import org.apache.commons.io.FileUtils;
import org.kalypso.java.io.FileUtilities;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobDataBean;

/**
 * @author Belger
 */
public abstract class AbstractCalcJob implements ICalcJob
{
  private Logger m_logger = Logger.getLogger( ICalcJob.class.getName() );
  
  private String m_message = "Warte auf Ausführung...";
  
  private int m_progress = -1;
  
  private Collection m_results = new Vector();

  private boolean m_canceled = false;
  
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#cancel()
   */
  public void cancel()
  {
    m_canceled = true;
  }
  
  public boolean isCanceled()
  {
    return m_canceled;
  }
  
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getProgress()
   */
  public final int getProgress()
  {
    return m_progress;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getMessage()
   */
  public final String getMessage()
  {
    return m_message;
  }
  
  protected final void setMessage( final String message )
  {
    m_message = message;
  }

  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#getResults()
   */
  public final CalcJobDataBean[] getResults()
  {
    m_logger.info( "Getting results" );
    synchronized( m_results )
    {
      return (CalcJobDataBean[])m_results.toArray( new CalcJobDataBean[m_results.size()] );
    }
  }

  protected void progress( final int work )
  {
    m_progress = Math.min( m_progress + work, 100 );
  }
  
  /**
   * @see org.kalypso.services.calculation.job.ICalcJob#disposeJob()
   */
  public void disposeJob()
  {
    // normalerweise nichts zu tun
  }

  protected void addResult( final CalcJobDataBean bean )
  {
    m_logger.info( "Adding result: " + bean.getPath() );
    
    synchronized( m_results )
    {
      m_results.add( bean );
    }
  }

  /**
   * Kopiert eine Datei in den Ausgabeordner und f?gt die entsprechende Bean zur
   * Ausgabe hinzu.
   * 
   * Die Pfade werden wie folgt angelegt:
   * 
   * Das Resultfile wird relativ zu resultdir aufgel?st und unter dem gleichen
   * rleativen Pfad unter das Outputdir abgelegt: z.B.: resultdir
   * C:\tmp\kalypsonatest\exe\ resultfile:
   * C:\tmp\kalypsonatest\exe\out_we.nat\950901.bof Ablage im utputdir:
   * C:\tmp\kalypsonatest\output\out_we.nat\950901.bof pfad in der Bean:
   * .\out_we.nat\950901.bof
   *  
   */
  protected void copyResult( final File resultdir, final File resultfile, final File outputdir, final String id, final String description )
  {
    final String relativePathTo = FileUtilities.getRelativePathTo( resultdir, resultfile );
    final File outputfile = new File( outputdir, relativePathTo );
  
    try
    {
      FileUtils.copyFile( resultfile, outputfile );
      addResult( new CalcJobDataBean( id, description, "." + relativePathTo ) );
    }
    catch( IOException e )
    {
      e.printStackTrace();
    }
  }
}