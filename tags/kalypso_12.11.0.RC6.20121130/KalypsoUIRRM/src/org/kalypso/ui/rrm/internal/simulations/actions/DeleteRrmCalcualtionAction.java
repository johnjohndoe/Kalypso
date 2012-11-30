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
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.results.view.ResultManagementView;

/**
 * @author Dirk Kuch
 */
public class DeleteRrmCalcualtionAction extends Action implements IUpdateable
{

  private final RrmCalculationResult[] m_calculations;

  private final ResultManagementView m_view;

  public DeleteRrmCalcualtionAction( final ResultManagementView view, final RrmCalculationResult... calculations )
  {
    super( Messages.getString( "DeleteRrmCalcualtionAction_0" ) ); //$NON-NLS-1$

    m_view = view;

    setToolTipText( Messages.getString( "DeleteRrmCalcualtionAction_1" ) ); //$NON-NLS-1$
    m_calculations = calculations;
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

    for( final RrmCalculationResult calculation : m_calculations )
    {
      final boolean confirmed = MessageDialog.openConfirm( shell, Messages.getString( "DeleteRrmCalcualtionAction_2" ), String.format( Messages.getString( "DeleteRrmCalcualtionAction_3" ), calculation.getName() ) ); //$NON-NLS-1$ //$NON-NLS-2$
      if( !confirmed )
        return;

      try
      {
        final IFolder folder = calculation.getFolder();
        folder.delete( true, new NullProgressMonitor() );
      }
      catch( final CoreException e )
      {
        e.printStackTrace();
      }
    }

    /* update tree */
    m_view.refresh();
    m_view.getTreeViewer().expandToLevel( 2 );
  }

  @Override
  public ImageDescriptor getImageDescriptor( )
  {
    return UIRrmImages.id( UIRrmImages.DESCRIPTORS.DELETE );
  }
}
