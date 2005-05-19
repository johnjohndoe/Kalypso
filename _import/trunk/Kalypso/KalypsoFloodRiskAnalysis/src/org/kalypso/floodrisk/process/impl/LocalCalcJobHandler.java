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
import org.kalypso.services.calculation.job.ICalcJob;
import org.kalypso.services.calculation.service.CalcJobClientBean;
import org.kalypso.services.calculation.service.CalcJobInfoBean;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * LocalCalcJobHandler
 * <p>
 * 
 * created by
 * 
 * @author Nadja Peiler (19.05.2005)
 */
public class LocalCalcJobHandler
{
  private ModeldataType m_modelData;

  private final CoreException m_cancelException = new CoreException( new Status( IStatus.CANCEL,
      KalypsoGisPlugin.getId(), 0, "Berechnung wurde vom Benutzer abgebrochen", null ) );

  private String m_jobID;

  private ICalcJob m_calcJob;
  
  private LocalCalculationService calcService = new LocalCalculationService();

  public LocalCalcJobHandler( final ModeldataType modelData, final ICalcJob calcJob )
  {
    m_modelData = modelData;
    m_calcJob = calcJob;
  }

  public IStatus runJob( final IProject project, final IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( m_modelData.getTypeID() + "wird berechnet.", 5000 );
    try
    {
      m_jobID = startCalcJob( project, new SubProgressMonitor( monitor, 1000 ) );

      if( monitor.isCanceled() )
        throw m_cancelException;

      final SubProgressMonitor calcMonitor = new SubProgressMonitor( monitor, 2000 );
      calcMonitor.beginTask( "Berechnung wird durchgeführt", 100 );
      int oldProgess = 0;
      while( true )
      {
        final CalcJobInfoBean bean = calcService.getJob( m_jobID );

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
        calcMonitor.worked( progress - oldProgess );
        oldProgess = progress;

        // ab hier bei cancel nicht mehr zurückkehren, sondern
        // erstmal den Job-Canceln und warten bis er zurückkehrt
        if( monitor.isCanceled() )
          calcService.cancelJob( m_jobID );
      }

      calcMonitor.done();

      final CalcJobInfoBean jobBean = calcService.getJob( m_jobID );

      // Abhängig von den Ergebnissen was machen
      switch( jobBean.getState() )
      {
      case ICalcServiceConstants.FINISHED:
        project.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 500 ) );
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
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Aufruf des Rechendienstes", e ) );
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

  private String startCalcJob( final IProject project, final IProgressMonitor monitor )
      throws CoreException
  {
    try
    {
      final List inputList = m_modelData.getInput();
      monitor.beginTask( "Eingangsdaten für Berechnung vorbereiten", inputList.size() );

      //prepare input
      CalcJobClientBean[] input = getInput( project );

      //prepare output
      CalcJobClientBean[] output = getOutput( project );

      final CalcJobInfoBean bean = calcService.startLocalJob( m_modelData.getTypeID(), "Description",
          m_calcJob, input, output );
      return bean.getId();
    }
    finally
    {
      monitor.done();
    }
  }

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
          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Konnte Input-Resource nicht finden: " + inputPath
                  + "\nÜberprüfen Sie die Modellspezifikation.", null ) );
        IPath projectRelativePath = inputResource.getLocation();
        inputPath = projectRelativePath.toString();
      }

      CalcJobClientBean calcJobDataBean = new CalcJobClientBean( inputItem.getId(), inputPath );
      input[i] = calcJobDataBean;
    }
    return input;
  }

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