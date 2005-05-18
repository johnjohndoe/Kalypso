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
package org.kalypso.services.calculation.service.impl;

import java.io.File;

import javax.activation.DataHandler;

import org.kalypso.java.io.FileUtilities;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.calculation.job.ICalcDataProvider;
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.job.ICalcResultPacker;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypso.services.calculation.service.CalcJobServiceException;
import org.kalypso.services.common.ServiceConfig;


final class CalcJobThread extends Thread
{
  private final ICalcJob m_job;

  private final CalcJobInfoBean m_jobBean;

  private final ICalcDataProvider m_inputData;

  private final ICalcResultPacker m_resultPacker;

  public CalcJobThread( final String id, final String description, final String typeID, final ICalcJob job, final ModelspecData modelspec, final DataHandler zipHandler, final CalcJobClientBean[] input, final CalcJobClientBean[] output ) throws CalcJobServiceException
  {
    m_job = job;
    
    m_jobBean = new CalcJobInfoBean( "" + id, description, typeID, ICalcServiceConstants.WAITING, -1, "" );
    m_inputData = new JarCalcDataProvider( zipHandler, input );
    m_resultPacker = new DefaultCalcResultEater( modelspec, output );

    modelspec.checkInput( m_inputData );
  }

  public void dispose()
  {
    m_resultPacker.disposeFiles();
  }

  public ICalcJob getJob()
  {
    return m_job;
  }
  
  public CalcJobInfoBean getJobBean()
  {
    m_jobBean.setCurrentResults( m_resultPacker.getCurrentResults() );

    return m_jobBean;
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
      CalcJobService_impl_Queued.LOGGER.info( "Calling run for ID: " + jobID );

      final File tmpdir = FileUtilities.createNewTempDir( "CalcJob-" + jobID + "-", ServiceConfig
          .getTempDir() );
      m_resultPacker.addFile( tmpdir );
      
      m_job.run( tmpdir, m_inputData, m_resultPacker, m_jobBean );

      CalcJobService_impl_Queued.LOGGER.info( "Run finished for ID: " + jobID );

      if( m_jobBean.isCanceled() )
        CalcJobService_impl_Queued.LOGGER.info( "JOB exited because it was canceled: " + jobID );
      else
      {
        m_jobBean.setState( ICalcServiceConstants.FINISHED );
        CalcJobService_impl_Queued.LOGGER.info( "JOB exited normaly: " + jobID );
      }
    }
    catch( final Throwable t )
    {
      CalcJobService_impl_Queued.LOGGER.warning( "JOB exited with exception: " + jobID );
      t.printStackTrace();

      m_jobBean.setMessage( t.getLocalizedMessage() );
      m_jobBean.setState( ICalcServiceConstants.ERROR );
    }
  }

  public DataHandler packCurrentResults() throws CalcJobServiceException
  {
    return m_resultPacker.packCurrentResults();
  }

  public String[] getCurrentResults()
  {
    return m_resultPacker.getCurrentResults();
  }
}