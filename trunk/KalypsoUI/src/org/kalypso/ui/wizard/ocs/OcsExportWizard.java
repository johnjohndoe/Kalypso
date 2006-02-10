/*--------------- Kalypso-Header ---------------------------------------

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
package org.kalypso.ui.wizard.ocs;

import java.util.List;

import javax.xml.rpc.ServiceException;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IExportWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.ide.IDE;
import org.kalypso.contribs.eclipse.core.runtime.MultiStatus;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.services.ocs.repository.ServiceRepositoryObservation;
import org.kalypso.services.sensor.impl.KalypsoObservationService;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.wizard.ocs.idtable.IdStruct;

/**
 * OcsExportWizard
 * 
 * @author schlienger
 */
public class OcsExportWizard extends Wizard implements IExportWizard
{
  private IStructuredSelection m_selection;

  private OcsExportWizardResourcesPage m_resPage;

  private OcsExportWizardIdentifiersPage m_idPage;

  public OcsExportWizard()
  {
    final IDialogSettings settings = KalypsoGisPlugin.getDefault().getDialogSettings();

    IDialogSettings section = settings.getSection( "OcsExportWizard" ); //$NON-NLS-1$
    if( section == null )
      section = settings.addNewSection( "OcsExportWizard" ); //$NON-NLS-1$

    setDialogSettings( section );
  }

  /**
   * @see org.eclipse.jface.wizard.Wizard#performFinish()
   */
  public boolean performFinish()
  {
    final IdStruct[] structs = m_idPage.getResourcesToExport();
    KalypsoObservationService srv = null;
    try
    {
      srv = KalypsoGisPlugin.getDefault().getObservationServiceProxy();
    }
    catch( final ServiceException e )
    {
      e.printStackTrace();
      MessageDialog.openError( getShell(), "Kalypso Server steht nicht zur Verfügung", e.getLocalizedMessage() );
      return false;
    }

    final MultiStatus ms = new MultiStatus( IStatus.ERROR, KalypsoGisPlugin.getId(), 0,
        "Fehler sind während der Export aufgetreten" );

    for( int i = 0; i < structs.length; i++ )
    {
      try
      {
        ServiceRepositoryObservation.setValuesFor( structs[i].getFile(), structs[i].getId(), srv );
      }
      catch( final SensorException e )
      {
        ms.addMessage( "Export von " + structs[i].getFile().getName() + " fehlerhaft: " + e.getLocalizedMessage(), e );
      }
    }

    if( !ms.isOK() )
    {
      ErrorDialog.openError( getShell(), "Fehler sind während der Export aufgetreten", "Siehe Details", ms );
      return false;
    }

    return true;
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#addPages()
   */
  public void addPages()
  {
    m_resPage = new OcsExportWizardResourcesPage( m_selection );
    m_idPage = new OcsExportWizardIdentifiersPage();

    addPage( m_resPage );
    addPage( m_idPage );
  }

  /**
   * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench,
   *      org.eclipse.jface.viewers.IStructuredSelection)
   */
  public void init( IWorkbench workbench, IStructuredSelection currentSelection )
  {
    m_selection = currentSelection;
    List selectedResources = IDE.computeSelectedResources( currentSelection );
    if( !selectedResources.isEmpty() )
    {
      m_selection = new StructuredSelection( selectedResources );
    }

    setWindowTitle( "Zeitreihenexport Wizard" );
    setNeedsProgressMonitor( true );
  }

  /**
   * @see org.eclipse.jface.wizard.IWizard#getNextPage(org.eclipse.jface.wizard.IWizardPage)
   */
  public IWizardPage getNextPage( IWizardPage page )
  {
    if( page == m_resPage )
      m_idPage.setResourcesToExport( m_resPage.getSelectedResources() );

    return super.getNextPage( page );
  }
}