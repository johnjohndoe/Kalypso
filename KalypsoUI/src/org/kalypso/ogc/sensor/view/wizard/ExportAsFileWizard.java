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
package org.kalypso.ogc.sensor.view.wizard;

import java.io.File;
import java.io.FileOutputStream;

import org.apache.commons.io.IOUtils;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.contribs.eclipse.core.resources.ProjectUtilities;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.jface.wizard.FileSelectWizardPage;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.request.ObservationRequest;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.zml.Observation;

/**
 * Wizard for exporting a Repository ZML as a file in the local filesystem
 * 
 * @author schlienger
 */
public class ExportAsFileWizard extends Wizard
{
  private DateRangeInputWizardPage m_page1;

  private FileSelectWizardPage m_page2;

  private final IObservation m_obs;

  protected IProject m_project = null;

  public ExportAsFileWizard( final IObservation obs )
  {
    m_obs = obs;

    final IDialogSettings settings = KalypsoGisPlugin.getDefault().getDialogSettings();

    IDialogSettings section = settings.getSection( "ExportAsFileWizard" ); //$NON-NLS-1$
    if( section == null )
      section = settings.addNewSection( "ExportAsFileWizard" ); //$NON-NLS-1$

    setDialogSettings( section );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#addPages()
   */
  public void addPages()
  {
    super.addPages();

    final IProject[] projects = ProjectUtilities.getSelectedProjects();
    String fileName = "";

    if( projects.length > 0 )
    {
      m_project = projects[0];
      fileName = ResourceUtilities.makeFileFromPath( m_project.getFullPath() ).getAbsolutePath();
    }
    else
      fileName = ResourcesPlugin.getWorkspace().getRoot().getLocation().toOSString() + File.separator + "datei.zml";

    m_page1 = new DateRangeInputWizardPage();
    m_page2 = new FileSelectWizardPage( "fileselect", fileName, new String[]
    { "*.zml", "*.xml" } );

    addPage( m_page1 );
    addPage( m_page2 );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#createPageControls(org.eclipse.swt.widgets.Composite)
   */
  public void createPageControls( Composite pageContainer )
  {
    //super.createPageControls( pageContainer );

    setWindowTitle( "Als Datei exportieren" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  public boolean performFinish()
  {
    final DateRange dateRange = m_page1.getDateRange();
    final String filePath = m_page2.getFilePath();

    FileOutputStream outs = null;
    try
    {
      final Observation ot = ZmlFactory.createXML( m_obs, new ObservationRequest( dateRange ) );

      outs = new FileOutputStream( new File( filePath ) );
      ZmlFactory.getMarshaller().marshal( ot, outs );
      outs.close();
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      MessageDialog.openError( getShell(), "Datei kann nicht erzeugt werden", e.getLocalizedMessage() );

      return false;
    }
    finally
    {
      IOUtils.closeQuietly( outs );
    }

    if( m_project != null )
    {
      final Job refreshJob = new Job( "Projekt " + m_project.getName() + " aktualisieren" )
      {
        protected IStatus run( IProgressMonitor monitor )
        {
          try
          {
            m_project.refreshLocal( IResource.DEPTH_INFINITE, monitor );
          }
          catch( CoreException e )
          {
            e.printStackTrace();

            return StatusUtilities.statusFromThrowable( e );
          }

          return Status.OK_STATUS;
        }
      };

      refreshJob.schedule();
    }

    return true;
  }
}
