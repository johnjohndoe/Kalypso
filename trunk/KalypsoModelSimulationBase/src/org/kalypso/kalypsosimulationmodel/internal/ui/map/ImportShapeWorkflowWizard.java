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
package org.kalypso.kalypsosimulationmodel.internal.ui.map;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.IWorkbench;
import org.kalypso.afgui.scenarios.ScenarioHelper;
import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.kalypsosimulationmodel.internal.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoLayerModell;
import org.kalypso.ui.wizard.AbstractDataImportWizard;

/**
 * Import wizard for shape files into the workflow. The shape is copied to a fixed folder and added as new theme into
 * the map.<br/>
 * Files that are already present are overwritten.
 * 
 * @author Gernot Belger
 */
public class ImportShapeWorkflowWizard extends AbstractDataImportWizard
{
  private IStructuredSelection m_initialSelection;

  private ImportBaseMapImportShpPage m_pageImportShp;

  /**
   * Construct a new instance and initialize the dialog settings for this instance.
   */
  public ImportShapeWorkflowWizard( )
  {
    setNeedsProgressMonitor( true );
    setWindowTitle( Messages.getString( "org.kalypso.ui.wizards.imports.baseMap.BaseMapWizard.0" ) ); //$NON-NLS-1$

    // /* Get the dialog settings. */
    // final IDialogSettings dialogSettings = getDialogSettings();
    //
    // /* If not available, add a section inside the settings of the plugin. */
    // if( dialogSettings == null )
    // {
    // final IDialogSettings settings = KalypsoModelSimulationBase.getDefault().getDialogSettings();
    //
    // /* Cannot do anything, if even the plugin has no settings. */
    // if( settings == null )
    // return;
    //
    // /* If available, check, if there is a section from this wizard. */
    //      IDialogSettings section = settings.getSection( "IMPORT_WMS_WIZARD" ); //$NON-NLS-1$
    // if( section == null )
    // {
    // /* There is none available, add a new one. */
    //        section = settings.addNewSection( "IMPORT_WMS_WIZARD" ); //$NON-NLS-1$
    // }
    //
    // /* Finally set it. */
    // setDialogSettings( section );
    // }
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
    m_initialSelection = selection;
  }

  @Override
  public void addPages( )
  {
    m_pageImportShp = new ImportBaseMapImportShpPage();

    m_pageImportShp.init( m_initialSelection );

    addPage( m_pageImportShp );
  }

  @Override
  public boolean performFinish( )
  {
    final IKalypsoLayerModell mapModell = getMapModel();
    final ICommandTarget commandTarget = getCommandTarget();

    final IFolder scenarioFolder = ScenarioHelper.getScenarioFolder();

    final IImportBaseMapOperation operation = m_pageImportShp.createOperation( commandTarget, mapModell, scenarioFolder );

    if( !operation.checkPreconditions( getShell(), getWindowTitle() ) )
      return false;

    final IStatus status = RunnableContextHelper.execute( getContainer(), true, false, operation );
    if( !status.isOK() )
      StatusDialog.open( getShell(), status, getWindowTitle() );

    return !status.matches( IStatus.ERROR );
  }
}