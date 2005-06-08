/*
 * --------------- Kalypso-Header
 * --------------------------------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ---------------------------------------------------------------------------------------------------
 */
package org.kalypso.floodrisk.process.impl;

import java.io.File;

import org.kalypso.java.io.FileUtilities;
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
 * 
 * created by
 * 
 * @author Nadja Peiler (19.05.2005)
 */
public class LocalCalcJobThread extends Thread
{

  private CalcJobInfoBean m_jobBean;

  private ProcessResultEater m_resultEater;

  private ICalcJob m_job;

  private ProcessDataProvider m_inputProvider;

  public LocalCalcJobThread( final String id, final String description, final String typeID,
      final ICalcJob job, final ModelspecData modelspec,
      final CalcJobClientBean[] input, final CalcJobClientBean[] output )
      throws CalcJobServiceException
  {
    m_job = job;

    m_jobBean = new CalcJobInfoBean( "" + id, description, typeID, ICalcServiceConstants.WAITING,
        -1, "" );
    m_inputProvider = new ProcessDataProvider( input );
    m_resultEater = new ProcessResultEater( output );
    
    modelspec.checkInput(m_inputProvider);
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

      final File tmpdir = FileUtilities.createNewTempDir( "CalcJob-" + jobID + "-", ServiceConfig
          .getTempDir() );
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

  public CalcJobInfoBean getJobBean()
  {
    m_jobBean.setCurrentResults( m_resultEater.getCurrentResults() );

    return m_jobBean;
  }
  
  public ICalcJob getJob()
  {
    return m_job;
  }
  
  public String[] getCurrentResults()
  {
    return m_resultEater.getCurrentResults();
  }
  
  public void dispose()
  {
    m_resultEater.disposeResults();
    m_resultEater.disposeFiles();
  }

}