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
package org.kalypso.ui.nature.calcjob;

import java.io.BufferedInputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.rmi.RemoteException;
import java.rmi.ServerException;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

import javax.activation.DataHandler;
import javax.activation.DataSource;
import javax.activation.FileDataSource;

import org.apache.commons.io.IOUtils;
import org.bce.eclipse.core.runtime.StatusUtilities;
import org.eclipse.core.resources.IContainer;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.kalypso.java.util.zip.ZipUtilities;
import org.kalypso.model.xml.ModeldataType;
import org.kalypso.model.xml.ModeldataType.ClearAfterCalcType;
import org.kalypso.services.calculation.common.ICalcServiceConstants;
import org.kalypso.services.proxy.CalcJobClientBean;
import org.kalypso.services.proxy.CalcJobInfoBean;
import org.kalypso.services.proxy.ICalculationService;
import org.kalypso.ui.KalypsoGisPlugin;

/**
 * @author belger
 */
public class CalcJobHandler
{
  private final ModeldataType m_modelspec;

  private final CoreException m_cancelException = new CoreException( new Status( IStatus.CANCEL,
      KalypsoGisPlugin.getId(), 0, "Berechnung wurde vom Benutzer abgebrochen", null ) );

  private String m_jobID = null;

  private final ICalculationService m_calcService;

  public CalcJobHandler( final ModeldataType modelspec, final ICalculationService calcService )
  {
    m_modelspec = modelspec;
    m_calcService = calcService;
  }

  public IStatus runJob( final IFolder calcCaseFolder, final IProgressMonitor monitor )
      throws CoreException
  {
    monitor.beginTask( "Berechnung wird durchgeführt für Variante: " + calcCaseFolder.getName(),
        5000 );
    try
    {
      // Daten zum Service schieben
      m_jobID = startCalcJob( calcCaseFolder, new SubProgressMonitor( monitor, 1000 ) );

      if( monitor.isCanceled() )
        throw m_cancelException;

      final SubProgressMonitor calcMonitor = new SubProgressMonitor( monitor, 2000 );
      calcMonitor.beginTask( "Berechnung wird durchgeführt", 100 );
      int oldProgess = 0;
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
        calcMonitor.worked( progress - oldProgess );
        oldProgess = progress;

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
        // Ergebniss abholen
        final DataHandler handler = m_calcService.transferCurrentResults( m_jobID );

        // clear results as defined in modelspec
        clearResults( calcCaseFolder, new SubProgressMonitor( monitor, 500 ) );

        final IProject project = calcCaseFolder.getProject();

        //just unzip files
        InputStream inputStream = null;
        try
        {
          inputStream = new BufferedInputStream( handler.getInputStream() );
          ZipUtilities.unzip( inputStream, project.getLocation().toFile() );
          monitor.worked( 1000 );
        }
        finally
        {
          IOUtils.closeQuietly( inputStream );
        }

        project.refreshLocal( IResource.DEPTH_INFINITE, new SubProgressMonitor( monitor, 500 ) );
        final String finishText = jobBean.getFinishText();
        final String message = finishText == null ? "" : finishText;
        return StatusUtilities.createMultiStatusFromMessage( jobBean.getFinishStatus(), KalypsoGisPlugin.getId(),
            0, message, System.getProperty("line.separator"), null );

      case ICalcServiceConstants.CANCELED:
        throw m_cancelException;

      case ICalcServiceConstants.ERROR:
      {
        final IStatus status = StatusUtilities.createMultiStatusFromMessage( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, jobBean.getMessage(), System.getProperty( "line.separator" ), null ); 
        throw new CoreException( status );
      }

      default:
      {
        // darf eigentlich nie vorkommen
        final IStatus status = StatusUtilities.createMultiStatusFromMessage( IStatus.ERROR, KalypsoGisPlugin.getId(), 0, jobBean.getMessage(), System.getProperty( "line.separator" ), null ); 
        throw new CoreException( status );
      }
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
    catch( final IOException e )
    {
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Fehler beim Zurückladen der Ergebnisse", e ) );
    }
    finally
    {
      //            try
      //            {
      //              calcService.disposeJob( job.getId() );
      //            }
      //            catch( final RemoteException e1 )
      //            {
      //              e1.printStackTrace();
      //      
      //              throw new CoreException( KalypsoGisPlugin.createErrorStatus(
      //                  "Kritischer Fehler bei Löschen des Rechen-Jobs", e1 ) );
      //            }
      monitor.done();
    }
  }

  private void clearResults( final IFolder calcCaseFolder, final IProgressMonitor monitor )
      throws CoreException
  {
    try
    {
      final IProject project = calcCaseFolder.getProject();

      final List clearList = m_modelspec.getClearAfterCalc();
      monitor.beginTask( "Alte Ergebnisse werden gelöscht", clearList.size() );

      for( final Iterator clearIt = clearList.iterator(); clearIt.hasNext(); )
      {
        final ModeldataType.ClearAfterCalcType clearType = (ClearAfterCalcType)clearIt.next();

        final boolean relToCalc = clearType.isRelativeToCalcCase();
        final String path = clearType.getPath();
        final IResource resource = relToCalc ? calcCaseFolder.findMember( path ) : project
            .findMember( path );
        if( resource != null )
          resource.delete( false, new SubProgressMonitor( monitor, 1 ) );
      }
    }
    finally
    {
      monitor.done();
    }
  }

  private String startCalcJob( final IFolder calcCaseFolder, final IProgressMonitor monitor )
      throws CoreException
  {
    try
    {
      final List inputList = m_modelspec.getInput();
      monitor.beginTask( "Eingangsdaten für Berechnungsdienst vorbereiten", inputList.size() );

      // write files to zip
      final File zipFile = File.createTempFile( "CalcJobData_", ".zip" );

      final CalcJobClientBean[] input = zipData( calcCaseFolder, monitor, inputList, zipFile );
      final CalcJobClientBean[] output = createOutputBeans( calcCaseFolder );

      final DataSource jarSource = new FileDataSource( zipFile );
      final DataHandler jarHandler = new DataHandler( jarSource );

      final CalcJobInfoBean bean = m_calcService.startJob( m_modelspec.getTypeID(), "Description",
          jarHandler, input, output );
      return bean.getId();
    }
    catch( final ServerException se )
    {
      throw new CoreException(
          KalypsoGisPlugin
              .createErrorStatus(
                  "Fehler beim Starten der Berechnung. Kontrollieren Sie die Konfiguration des Rechendienstes.",
                  se ) );
    }
    catch( final IOException e )
    {
      throw new CoreException( KalypsoGisPlugin.createErrorStatus(
          "Eingangsdaten konnten nicht erzeugt werden.", e ) );
    }
    finally
    {
      monitor.done();
    }
  }

  private CalcJobClientBean[] createOutputBeans( final IFolder calcCaseFolder )
  {
    final List list = m_modelspec.getOutput();
    final CalcJobClientBean[] output = new CalcJobClientBean[list.size()];
    int count = 0;
    for( final Iterator iter = list.iterator(); iter.hasNext(); )
    {
      final ModeldataType.OutputType ot = (ModeldataType.OutputType)iter.next();

      final String outpath = ot.getPath();
      final boolean relCalcCase = ot.isRelativeToCalcCase();
      final String path = relCalcCase ? calcCaseFolder.getProjectRelativePath() + "/" + outpath
          : outpath;

      output[count++] = new CalcJobClientBean( ot.getId(), path );
    }

    return output;
  }

  private CalcJobClientBean[] zipData( final IFolder calcCaseFolder,
      final IProgressMonitor monitor, final List inputList, final File zipFile )
      throws FileNotFoundException, CoreException, IOException
  {
    ZipResourceVisitor zipper = null;
    try
    {
      zipper = new ZipResourceVisitor( zipFile );

      final IProject project = calcCaseFolder.getProject();

      final List inputBeanList = new ArrayList();
      for( final Iterator iter = inputList.iterator(); iter.hasNext(); )
      {
        final ModeldataType.InputType input = (ModeldataType.InputType)iter.next();
        final String inputPath = input.getPath();

        // alles relativ zum Projekt auflösen!
        final IContainer baseresource = input.isRelativeToCalcCase() ? (IContainer)calcCaseFolder
            : (IContainer)project;
        final IResource inputResource = baseresource.findMember( inputPath );
        if( inputResource == null )
        {
          if( input.isOptional() )
            continue;

          throw new CoreException( KalypsoGisPlugin.createErrorStatus(
              "Konnte Input-Resource nicht finden: " + inputPath
                  + "\nÜberprüfen Sie die Modellspezifikation.", null ) );
        }

        final IPath projectRelativePath = inputResource.getProjectRelativePath();
        inputResource.accept( zipper );

        final CalcJobClientBean calcJobDataBean = new CalcJobClientBean( input.getId(),
            projectRelativePath.toString() );
        inputBeanList.add( calcJobDataBean );

        monitor.worked( 1 );
      }

      return (CalcJobClientBean[])inputBeanList
          .toArray( new CalcJobClientBean[inputBeanList.size()] );
    }
    finally
    {
      if( zipper != null )
        zipper.close();
    }
  }
}