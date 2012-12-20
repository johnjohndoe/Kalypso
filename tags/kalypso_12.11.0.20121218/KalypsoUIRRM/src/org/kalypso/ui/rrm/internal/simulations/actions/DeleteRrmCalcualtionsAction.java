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
package org.kalypso.ui.rrm.internal.simulations.actions;

import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.action.Action;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.kalypso.contribs.eclipse.jface.wizard.IUpdateable;
import org.kalypso.model.hydrology.project.RrmCalculationResult;
import org.kalypso.model.hydrology.project.RrmSimulation;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;

/**
 * @author Dirk Kuch
 */
public class DeleteRrmCalcualtionsAction extends Action implements IUpdateable
{

  private final ResultManagementView m_view;

  private final RrmSimulation m_simulation;

  public DeleteRrmCalcualtionsAction( final RrmSimulation simulation, final ResultManagementView view )
  {
    super( Messages.getString("DeleteRrmCalcualtionsAction_0") ); //$NON-NLS-1$
    setToolTipText( Messages.getString("DeleteRrmCalcualtionsAction_1") ); //$NON-NLS-1$

    m_simulation = simulation;
    m_view = view;
  }

  @Override
  public void update( )
  {
    final RrmCalculationResult[] results = m_simulation.getCalculationResults();
    for( final RrmCalculationResult result : results )
    {
      if( result.getFolder().exists() )
      {
        setEnabled( true );
        return;
      }
    }

    setEnabled( false );
  }

  @Override
  public void run( )
  {
    final Shell shell = PlatformUI.getWorkbench().getDisplay().getActiveShell();
    final boolean confirmed = MessageDialog.openConfirm( shell, Messages.getString("DeleteRrmCalcualtionsAction_2"), Messages.getString("DeleteRrmCalcualtionsAction_3") ); //$NON-NLS-1$ //$NON-NLS-2$
    if( !confirmed )
      return;

    final RrmCalculationResult[] calculations = m_simulation.getCalculationResults();

    try
    {
      for( final RrmCalculationResult calculation : calculations )
      {
        final IFolder folder = calculation.getFolder();
        folder.delete( true, new NullProgressMonitor() );
      }
    }
    catch( final CoreException e )
    {
      e.printStackTrace();
    }

    /* update tree */
    m_view.refresh();
  }

  @Override
  public ImageDescriptor getImageDescriptor( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.DELETE );
  }
}
