/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.wizards.imports.baseMap;

import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.IDialogSettings;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.IHandlerService;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypso1d2d.pjt.Kalypso1d2dProjectPlugin;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.views.map.MapView;
import org.kalypso.ui.wizards.i18n.Messages;

import de.renew.workflow.contexts.ICaseHandlingSourceProvider;

/**
 * FIXME: wrong dependency: this wizard/handler is referenced from risk, flood as well; so they will not work of 1d2d is
 * not present!
 *
 *
 * @author Dejan Antanaskovic, <a href="mailto:dejan.antanaskovic@tuhh.de">dejan.antanaskovic@tuhh.de</a>
 * @author Madanagopal
 */
public class ImportBaseMapWizard extends Wizard implements INewWizard
{
  private IStructuredSelection initialSelection;

  IFolder m_scenarioFolder;

  private ImportBaseMapWizardMainPage m_pageMain;

  protected ImportBaseMapImportImgPage m_pageImportImg;

  protected ImportBaseMapImportShpPage m_pageImportShp;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportBaseMapWizard( )
  {
    /* Get the dialog settings. */
    final IDialogSettings dialogSettings = getDialogSettings();

    /* If not available, add a section inside the settings of the plugin. */
    if( dialogSettings == null )
    {
      final IDialogSettings settings = Kalypso1d2dProjectPlugin.getDefault().getDialogSettings();

      /* Cannot do anything, if even the plugin has no settings. */
      if( settings == null )
        return;

      /* If available, check, if there is a section from this wizard. */
      IDialogSettings section = settings.getSection( "IMPORT_WMS_WIZARD" ); //$NON-NLS-1$
      if( section == null )
      {
        /* There is none available, add a new one. */
        section = settings.addNewSection( "IMPORT_WMS_WIZARD" ); //$NON-NLS-1$
      }

      /* Finally set it. */
      setDialogSettings( section );
    }
  }

  /**
   * @param workbench
   *          the current workbench
   * @param selection
   *          the current object selection
   */
  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    final IHandlerService handlerService = (IHandlerService) workbench.getService( IHandlerService.class );
    final IEvaluationContext context = handlerService.getCurrentState();
    m_scenarioFolder = (IFolder) context.getVariable( ICaseHandlingSourceProvider.ACTIVE_CASE_FOLDER_NAME );

    initialSelection = selection;
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapWizard.0" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    m_pageMain = new ImportBaseMapWizardMainPage();
    m_pageImportImg = new ImportBaseMapImportImgPage();
    m_pageImportShp = new ImportBaseMapImportShpPage();
    m_pageImportImg.init( initialSelection );
    m_pageImportShp.init( initialSelection );
    addPage( m_pageMain );
    addPage( m_pageImportImg );
    addPage( m_pageImportShp );
  }

  private IImportBaseMapPage getSelectedPage( )
  {
    final IWizardPage currentPage = getContainer().getCurrentPage();
    if( currentPage instanceof IImportBaseMapPage )
      return (IImportBaseMapPage) currentPage;

    return null;
  }

  @Override
  public boolean canFinish( )
  {
    final IWizardPage currentPage = getContainer().getCurrentPage();

    if( currentPage instanceof IImportBaseMapPage )
      return currentPage.isPageComplete();

    return false;
  }

  @Override
  public boolean performFinish( )
  {
    final MapView mapView = (MapView) PlatformUI.getWorkbench().getActiveWorkbenchWindow().getActivePage().findView( MapView.ID );
    if( mapView == null )
    {
      // StatusUtilities.createWarningStatus( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.ImportBaseMapWizard.6" ) ); //$NON-NLS-1$
      return false;
    }

    final IKalypsoLayerModell mapModell = mapView.getMapPanel().getMapModell();
    final IImportBaseMapPage selectedPage = getSelectedPage();
    final IImportBaseMapOperation operation = selectedPage.createOperation( mapView, mapModell, m_scenarioFolder );

    if( !operation.checkPreconditions( getShell(), getWindowTitle() ) )
      return false;

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
      StatusDialog.open( getShell(), status, getWindowTitle() );

    return !status.matches( IStatus.ERROR );
  }
}