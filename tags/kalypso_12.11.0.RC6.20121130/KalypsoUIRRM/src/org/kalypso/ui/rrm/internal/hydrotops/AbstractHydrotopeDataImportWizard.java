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
package org.kalypso.ui.rrm.internal.hydrotops;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.IWizardContainer;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gml.ui.commands.importshape.ImportShapeWizardPage;
import org.kalypso.model.hydrology.binding.parameter.Parameter;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;

/**
 * Encapsulates common code for landuse, geology, pedology import.
 * 
 * @author Gernot Belger
 */
public abstract class AbstractHydrotopeDataImportWizard extends Wizard implements IWorkbenchWizard
{
  private ImportShapeWizardPage m_wizardPage;

  private CommandableWorkspace m_dataWorkspace;

  public AbstractHydrotopeDataImportWizard( )
  {
    setNeedsProgressMonitor( true );
  }

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_dataWorkspace = HydrotopesHelper.findWorkspaceForImport( selection );

    final String[] properties = getProperties();
    final String description = getDescription();

    m_wizardPage = new ImportShapeWizardPage( "shapePage", properties ); //$NON-NLS-1$
    m_wizardPage.setDescription( description ); //$NON-NLS-1$

    addPage( m_wizardPage );
  }

  @Override
  public final boolean performFinish( )
  {
    final Shell shell = m_wizardPage.getShell();
    final String windowTitle = getWindowTitle();

    final Parameter parameter = HydrotopesHelper.findParameterModel( shell, windowTitle, m_dataWorkspace );
    if( parameter == null )
      return true;

    final IStatus execute = executeOperation( parameter );
    StatusDialog.open( shell, execute, windowTitle );
    if( execute.matches( IStatus.ERROR ) )
      return false;

    try
    {
      m_dataWorkspace.fireModellEvent( new FeatureStructureChangeModellEvent( m_dataWorkspace, m_dataWorkspace.getRootFeature(), (Feature[]) null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );

      /* Make sure data is dirty */
      m_dataWorkspace.postCommand( new EmptyCommand( StringUtils.EMPTY, false ) );
      return true;
    }
    catch( final Exception e )
    {
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("AbstractHydrotopeDataImportWizard_1"), e ); //$NON-NLS-1$
      StatusDialog.open( shell, status, windowTitle );
      return false;
    }
  }

  private IStatus executeOperation( final Parameter parameter )
  {
    try
    {
      final ICoreRunnableWithProgress op = createImportOperation( m_wizardPage, m_dataWorkspace, parameter );

      final IWizardContainer container = getContainer();
      return RunnableContextHelper.execute( container, true, true, op );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
      return e.getStatus();
    }
  }

  protected abstract ICoreRunnableWithProgress createImportOperation( ImportShapeWizardPage wizardPage, final GMLWorkspace dataWorkspace, final Parameter parameter ) throws CoreException;

  protected abstract String[] getProperties( );

  protected abstract String getDescription( );
}