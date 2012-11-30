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
package org.kalypso.model.wspm.tuhh.ui.wizards.center.line;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWizard;
import org.kalypso.contribs.eclipse.jface.operation.RunnableContextHelper;
import org.kalypso.contribs.eclipse.jface.wizard.ArrayChooserPage;
import org.kalypso.contribs.java.util.Arrays;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.gml.ui.util.SelectionFeatureExtractor;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ui.editor.gmleditor.part.GMLLabelProvider;

/**
 * A tool that derives the centerline of a river from a set of cross sections, using the lowest point of each section.
 *
 * @author Gernot Belger
 */
public class DeriveCenterlineWizard extends Wizard implements IWorkbenchWizard
{
  private final DeriveCenterlineData m_data = new DeriveCenterlineData();

  private ArrayChooserPage m_selectionPage;

  @Override
  public void init( final IWorkbench workbench, final IStructuredSelection selection )
  {
    m_data.init( getDialogSettings() );

    /* Find selected water bodies */
    final SelectionFeatureExtractor<WspmWaterBody> extractor = new SelectionFeatureExtractor<>( WspmWaterBody.class );
    extractor.addSelection( selection );

    final WspmWaterBody[] availableWaterBodies = extractor.getAvailableElements();
    final WspmWaterBody[] selectedWaterBodies = extractor.getSelectedElements();
    m_data.setSelectedWaterBodies( selectedWaterBodies );

    m_data.setWorkspace( findWorkspace( selection, selectedWaterBodies ) );

    m_selectionPage = new ArrayChooserPage( availableWaterBodies, new Object[] {}, selectedWaterBodies, 1, "wbselection", Messages.getString( "DeriveCenterlineWizard_0" ), null ); //$NON-NLS-1$ //$NON-NLS-2$
    m_selectionPage.setLabelProvider( new GMLLabelProvider() );
    m_selectionPage.setDescription( Messages.getString( "DeriveCenterlineWizard_1" ) ); //$NON-NLS-1$
  }

  @Override
  public void addPages( )
  {
    addPage( m_selectionPage );

    // TODO: options?
  }

  @Override
  public boolean performFinish( )
  {
    final Object[] chosen = m_selectionPage.getChoosen();
    final WspmWaterBody[] chosenWaterBodies = Arrays.castArray( chosen, new WspmWaterBody[chosen.length] );
    m_data.setSelectedWaterBodies( chosenWaterBodies );

    final DeriverCenterlineOperation operation = new DeriverCenterlineOperation( m_data );
    final IStatus status = RunnableContextHelper.execute( getContainer(), true, true, operation );
    if( status != Status.OK_STATUS )
      StatusDialog.open( getShell(), status, getWindowTitle() );

    return !status.matches( IStatus.ERROR );
  }

  private CommandableWorkspace findWorkspace( final IStructuredSelection selection, final WspmWaterBody[] selectedWaterBodies )
  {
    if( selection instanceof IFeatureSelection )
    {
      final IFeatureSelection fs = (IFeatureSelection) selection;

      for( final WspmWaterBody waterBody : selectedWaterBodies )
      {
        final CommandableWorkspace workspace = fs.getWorkspace( waterBody );
        if( workspace != null )
          return workspace;
      }
    }

    return null;
  }
}