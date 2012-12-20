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
package org.kalypso.ui.rrm.internal.simulations.actions;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.InputDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.contribs.eclipse.utils.NewFilenameValidator;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;

/**
 * @author Dirk Kuch
 */
public class RenameRrmCalcualtionAction extends Action implements IUpdateable
{

  private final RrmCalculationResult m_calculation;

  private final ResultManagementView m_view;

  public RenameRrmCalcualtionAction( final ResultManagementView view, final RrmCalculationResult calculation )
  {
    super( Messages.getString("RenameRrmCalcualtionAction.0") ); //$NON-NLS-1$

    m_view = view;

    setToolTipText( getText() );
    m_calculation = calculation;
  }

  @Override
  public void update( )
  {
    // TODO current?!?
  }

  @Override
  public void run( )
  {
    final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();

    final String msg = String.format( Messages.getString("RenameRrmCalcualtionAction.1"), m_calculation.getName() ); //$NON-NLS-1$

    final IFolder srcFolder = m_calculation.getFolder();
    final InputDialog inputDialog = new InputDialog( shell, getText(), msg, m_calculation.getName(), new NewFilenameValidator( srcFolder, true ) );
    final int confirmed = inputDialog.open();
    if( confirmed != Window.OK )
      return;

    try
    {
      final String target = inputDialog.getValue();
      final IFolder targetFolder = ((IFolder) srcFolder.getParent()).getFolder( target );
      srcFolder.move( targetFolder.getFullPath(), true, new NullProgressMonitor() );
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    /* update tree */
    m_view.refresh();
    m_view.getTreeViewer().expandToLevel( 2 );
  }

  @Override
  public ImageDescriptor getImageDescriptor( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.EDIT_STATION );
  }
}
