/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraﬂe 22
 *  21073 Hamburg, Germany
 *  http://www.tuhh.de/wb
 * 
 *  and
 *  
 *  Bjoernsen Consulting Engineers (BCE)
 *  Maria Trost 3
 *  56070 Koblenz, Germany
 *  http://www.bjoernsen.de
 * 
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
 * 
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 * 
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 * 
 *  Contact:
 * 
 *  E-Mail:
 *  belger@bjoernsen.de
 *  schlienger@bjoernsen.de
 *  v.doemming@tuhh.de
 *   
 *  ---------------------------------------------------------------------------*/
package org.kalypso.floodrisk.process.impl;

import java.io.File;

import org.kalypso.commons.java.io.FileUtilities;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.calculation.service.impl.ModelspecData;
import org.kalypso.services.common.ServiceConfig;

/**
 * LocalCalcJobThread
 * <p>
 * Thread to run the local calcJob
 * 
 * @see org.kalypso.services.calculation.service.impl.CalcJobThread
 * 
 * created by
 * @author Nadja Peiler (19.05.2005)
 */
public class LocalCalcJobThread extends Thread
{

  private CalcJobInfoBean m_jobBean;

  private ProcessResultEater m_resultEater;

  private ICalcJob m_job;

  private ProcessDataProvider m_inputProvider;

  /**
   * @see org.kalypso.services.calculation.service.impl.CalcJobThread#CalcJobThread(java.lang.String, java.lang.String,
   *      java.lang.String, org.kalypso.services.calculation.job.ICalcJob,
   *      org.kalypso.services.calculation.service.impl.ModelspecData, javax.activation.DataHandler,
   *      org.kalypso.services.calculation.service.CalcJobClientBean[],
   *      org.kalypso.services.calculation.service.CalcJobClientBean[])
   * 
   * @param id
   *          id of the calcJob
   * @param description
   *          description of the calcJob
   * @param typeID
   *          typeID of the process
   * @param job
   *          the job to process
   * @param modelspec
   *          ModelspecData(specification) of the calcJob, input- and output-ids and descriptions
   * @param input
   *          input-beans
   * @param output
   *          output-beans
   * @throws CalcJobServiceException
   */
  public LocalCalcJobThread( final String id, final String description, final String typeID, final ICalcJob job,
      final ModelspecData modelspec, final CalcJobClientBean[] input, final CalcJobClientBean[] output )
      throws CalcJobServiceException
  {
    m_job = job;

    m_jobBean = new CalcJobInfoBean( "" + id, description, typeID, ICalcServiceConstants.WAITING, -1, "" );
    m_inputProvider = new ProcessDataProvider( input );
    m_resultEater = new ProcessResultEater( output );

    modelspec.checkInput( m_inputProvider );
  }

  /**
   * @see java.lang.Thread#run()
   */
  public void run()
  {
    m_jobBean.setState( ICalcServiceConstants.RUNNING );

    final String jobID = m_jobBean.getId();
    try
    {
      System.out.println( "Calling run for ID: " + jobID );

      final File tmpdir = FileUtilities.createNewTempDir( "CalcJob-" + jobID + "-", ServiceConfig.getTempDir() );
      m_resultEater.addFile( tmpdir );

      m_job.run( tmpdir, m_inputProvider, m_resultEater, m_jobBean );

      System.out.println( "Run finished for ID: " + jobID );

      if( m_jobBean.isCanceled() )
        System.out.println( "JOB exited because it was canceled: " + jobID );
      else
      {
        m_jobBean.setState( ICalcServiceConstants.FINISHED );
        System.out.println( "JOB exited normaly: " + jobID );
      }
    }
    catch( final Throwable t )
    {
      System.out.println( "JOB exited with exception: " + jobID );
      t.printStackTrace();

      m_jobBean.setMessage( t.getLocalizedMessage() );
      m_jobBean.setState( ICalcServiceConstants.ERROR );
    }
  }

  /**
   * 
   * @return CalcJobInfoBean with current results
   *  
   */
  public CalcJobInfoBean getJobBean()
  {
    m_jobBean.setCurrentResults( m_resultEater.getCurrentResults() );

    return m_jobBean;
  }

  /**
   * 
   * @return calcJob
   *  
   */
  public ICalcJob getJob()
  {
    return m_job;
  }

  /**
   * 
   * @return the ids of the current results
   *  
   */
  public String[] getCurrentResults()
  {
    return m_resultEater.getCurrentResults();
  }

  /**
   * called, when the calcJob is disposed (canceled)
   */
  public void dispose()
  {
    m_resultEater.disposeResults();
    m_resultEater.disposeFiles();
  }

}