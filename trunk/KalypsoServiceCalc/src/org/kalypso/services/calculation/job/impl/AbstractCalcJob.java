/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
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

  public final void addResult( final CalcJobDataBean bean )
  {
    m_logger.info( "Adding result: " + bean.getPath() );

    synchronized( m_results )
    {
      m_results.add( bean );
    }
  }

  // TODO: so sollte es sein!
//  /**
//   * Fügt eine Datei zur Ausgabe hinzu.
//   * 
//   * @param file Diese Datei oder dieses Verzeichnis werden zurück an den Server gegeben.
//   * @param id Die ID für die Bean
//   * @param description Der Name der Bean
//   * @param targetPath Das Ergebnis wird in diesen Pfad innnerhalb der Rechenvarianten zurückgeschrieben.
//   */
//  protected void addResult( final File file, final String id, final String description, final String targetPath )
//  {
//    final DataSource source = new FileDataSource( file );
//    final DataHandler data = new DataHandler( source );
//    addResult( new CalcJobDataBean( id, description, targetPath, data ) );
//  }
  
  /**
   * Kopiert eine Datei in den Ausgabeordner und fügt die entsprechende Bean zur
   * Ausgabe hinzu.
   * 
   * Die Pfade werden wie folgt angelegt:
   * 
   * Das Resultfile wird relativ zu resultdir aufgel?st und unter dem gleichen
   * rleativen Pfad unter das Outputdir abgelegt: z.B.:
   * <ul>
   * <li>
   * resultdir: C:\tmp\kalypsonatest\exe\
   * </li>
   * <li>
   * resultfile: C:\tmp\kalypsonatest\exe\out_we.nat\950901.bof
   * </li>
   * <li>
   * Ablage im utputdir: C:\tmp\kalypsonatest\output\out_we.nat\950901.bof
   * </li>
   * <li>
   * Pfad in der Bean: .\out_we.nat\950901.bof
   * </li>
   *  </ul>
   */
  protected void copyResult( final File resultdir, final File resultfile, final File outputdir,
      final String id, final String description )
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