/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.rmi.RemoteException;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.model.xml.ModeldataType;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.proxy.CalcJobClientBean;
import org.kalypso.services.proxy.CalcJobInfoBean;
import org.kalypso.services.proxy.ICalculationService;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * LocalCalcJobHandler
 * <p>
 * Handler for local calculation jobs
 * 
 * @see org.kalypso.ui.nature.calcjob.CalcJobHandler
 * 
 * created by
 * 
 * @author Nadja Peiler (19.05.2005)
 */
public class LocalCalcJobHandler
{
  private ModeldataType m_modelData;

  private final CoreException m_cancelException = new CoreException( new Status( IStatus.CANCEL, KalypsoGisPlugin
      .getId(), 0, "Berechnung wurde vom Benutzer abgebrochen", null ) );

  private String m_jobID;

  private ICalculationService m_calcService;

  /**
   * @see org.kalypso.ui.nature.calcjob.CalcJobHandler#CalcJobHandler(ModeldataType, ICalculationService)
   * 
   * @param modelData
   * @param calcService
   *          LocalCalculationService
   */
  public LocalCalcJobHandler( final ModeldataType modelData, final ICalculationService calcService )
  {
    m_modelData = modelData;
    m_calcService = calcService;
  }

  /**
   * @see org.kalypso.ui.nature.calcjob.CalcJobHandler#runJob(IFolder, IProgressMonitor)
   * 
   * @param project
   *          project to calculate, instead of calculationFolder
   * @param monitor
   * @return
   * @throws CoreException
   *  
   */
  public IStatus runJob( final IProject project, final IProgressMonitor monitor ) throws CoreException
  {

    try
    {
      m_jobID = startCalcJob( project, monitor );

      if( monitor.isCanceled() )
        throw m_cancelException;

      final SubProgressMonitor calcMonitor = new SubProgressMonitor( monitor, 80,
          SubProgressMonitor.PREPEND_MAIN_LABEL_TO_SUBTASK );
      calcMonitor.beginTask( "Berechnung wird durchgeführt", 100 );
      //int oldProgess = 0;
      while( true )
      {
        final CalcJobInfoBean bean = m_calcService.getJob( m_jobID );

        boolean bStop = false;
        switch( bean.getState() )
        {
        case ICalcServiceConstants.FINISHED:
        case ICalcServiceConstants.ERROR:
        case ICalcServiceConstants.CANCELED:
          bStop = true;
          break;

        default:
          break;
        }

        if( bStop )
          break;

        try
        {
          Thread.sleep( 1000 );
        }
        catch( final InterruptedException e1 )
        {
          e1.printStackTrace();

          throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Kritischer Fehler", e1 ) );
        }

        int progress = bean.getProgress();
        if( progress != -1 )
          calcMonitor.worked( progress );
        calcMonitor.setTaskName( bean.getMessage() );
        //oldProgess = progress;

        // ab hier bei cancel nicht mehr zurückkehren, sondern
        // erstmal den Job-Canceln und warten bis er zurückkehrt
        if( monitor.isCanceled() )
          m_calcService.cancelJob( m_jobID );
      }

      calcMonitor.done();

      final CalcJobInfoBean jobBean = m_calcService.getJob( m_jobID );

      // Abhängig von den Ergebnissen was machen
      switch( jobBean.getState() )
      {
      case ICalcServiceConstants.FINISHED:
        project.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 10 ) );
        final String finishText = jobBean.getFinishText();
        final String message = finishText == null ? "" : finishText;
        return new Status( jobBean.getFinishStatus(), KalypsoGisPlugin.getId(), 0, message, null );

      case ICalcServiceConstants.CANCELED:
        throw m_cancelException;

      case ICalcServiceConstants.ERROR:
        throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Rechenvorgang fehlerhaft:\n"
            + jobBean.getMessage(), null ) );

      default:
        // darf eigentlich nie vorkommen
        throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Rechenvorgang fehlerhaft:\n"
            + jobBean.getMessage(), null ) );
      }
    }
    catch( final RemoteException e )
    {
      e.printStackTrace();
      throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Fehler beim Aufruf des Rechendienstes", e ) );
    }
    catch( final CoreException e )
    {
      throw e;
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * get input and output, then start the calcJob by calling the startJob(typeID, description, dataHandler, input,
   * output)-method of localCalculationService
   * 
   * @param project
   * @param monitor
   * @return
   * @throws CoreException
   *  
   */
  private String startCalcJob( final IProject project, final IProgressMonitor monitor ) throws CoreException
  {
    try
    {
      //prepare input
      CalcJobClientBean[] input = getInput( project );

      //prepare output
      CalcJobClientBean[] output = getOutput( project );

      monitor.worked( 10 );
      final CalcJobInfoBean bean;
      try
      {
        //dataHandler = null
        bean = m_calcService.startJob( m_modelData.getTypeID(), "Description", null, input, output );
        return bean.getId();
      }
      catch( RemoteException e )
      {
        e.printStackTrace();
        return null;
      }
    }
    finally
    {
      //monitor.done();
    }
  }

  /**
   * create the inputdata-beans from ModelDataType (modeldata.xml)
   * 
   * @param project
   * @return
   * @throws CoreException
   *  
   */
  private CalcJobClientBean[] getInput( IProject project ) throws CoreException
  {
    List inputList = m_modelData.getInput();
    CalcJobClientBean[] input = new CalcJobClientBean[inputList.size()];
    for( int i = 0; i < inputList.size(); i++ )
    {
      ModeldataType.InputType inputItem = (ModeldataType.InputType)inputList.get( i );

      String inputPath = inputItem.getPath();
      if( !inputItem.isRelativeToCalcCase() )
      {
        IResource inputResource = project.findMember( inputPath );
        if( inputResource == null )
          throw new CoreException( KalypsoGisPlugin.createErrorStatus( "Konnte Input-Resource nicht finden: "
              + inputPath + "\nÜberprüfen Sie die Modellspezifikation.", null ) );
        IPath projectRelativePath = inputResource.getLocation();
        inputPath = projectRelativePath.toString();
      }

      CalcJobClientBean calcJobDataBean = new CalcJobClientBean( inputItem.getId(), inputPath );
      input[i] = calcJobDataBean;
    }
    return input;
  }

  /**
   * create the outputdata-beans from ModelDataType (modeldata.xml)
   * 
   * @param project
   * @return
   * @throws CoreException
   *  
   */
  private CalcJobClientBean[] getOutput( IProject project ) throws CoreException
  {
    List outputList = m_modelData.getOutput();
    CalcJobClientBean[] output = new CalcJobClientBean[outputList.size()];
    for( int i = 0; i < outputList.size(); i++ )
    {
      ModeldataType.OutputType outputItem = (ModeldataType.OutputType)outputList.get( i );

      String outputPath = outputItem.getPath();
      String resultPath = outputPath;
      if( !outputItem.isRelativeToCalcCase() )
      {
        resultPath = project.getLocation() + "/" + outputPath;
      }

      CalcJobClientBean calcJobDataBean = new CalcJobClientBean( outputItem.getId(), resultPath );
      output[i] = calcJobDataBean;
    }
    return output;
  }
}