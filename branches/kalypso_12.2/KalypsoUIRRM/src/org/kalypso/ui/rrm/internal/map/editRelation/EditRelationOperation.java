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
package org.kalypso.ui.rrm.internal.map.editRelation;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.commons.command.ICommand;
import org.kalypso.core.status.StatusDialog;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ui.rrm.internal.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public class EditRelationOperation implements Runnable
{
  private static final String STR_TITLE_REMOVE_RELATION = Messages.getString("EditRelationOperation_0"); //$NON-NLS-1$

  private final EditRelationData m_data;

  private final Shell m_shell;

  private final EditRelationWidget m_widget;

  public EditRelationOperation( final EditRelationData data, final Shell shell, final EditRelationWidget widget )
  {
    m_data = data;
    m_shell = shell;
    m_widget = widget;
  }

  public void execute( )
  {
    m_shell.getDisplay().asyncExec( this );
  }

  @Override
  public void run( )
  {
    try
    {
      doRun();
    }
    finally
    {
      m_widget.reset();
    }
  }

  protected void doRun( )
  {
    final Feature sourceFeature = m_data.getSourceFeature();
    final Feature targetFeature = m_data.getTargetFeature();
    if( sourceFeature == null || targetFeature == null )
      return;

    final IEditRelationType relation = m_data.getRelation();
    final EditRelationInput input = m_data.getInput();
    if( relation == null || input == null )
      return;

    final EditRelationMode mode = m_data.getModificationMode();
    if( relation.validate( sourceFeature, targetFeature, mode ) != null )
      return;

    final CommandableWorkspace workspace = input.getWorkspace();
    switch( mode )
    {
      case ADD:
      {
        final ICommand command = relation.getAddCommand( m_shell, sourceFeature, targetFeature );
        executeCommand( workspace, command, Messages.getString("EditRelationOperation_1") ); //$NON-NLS-1$
        break;
      }

      case REMOVE:
      {
        if( !MessageDialog.openConfirm( m_shell, STR_TITLE_REMOVE_RELATION, Messages.getString("EditRelationOperation_2") ) ) //$NON-NLS-1$
          return;

        final ICommand command = relation.getRemoveCommand( sourceFeature, targetFeature );
        executeCommand( workspace, command, STR_TITLE_REMOVE_RELATION );
        break;
      }
    }
  }

  private void executeCommand( final CommandableWorkspace workspace, final ICommand command, final String title )
  {
    try
    {
      if( command != null )
        workspace.postCommand( command );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      final IStatus status = new Status( IStatus.ERROR, KalypsoUIRRMPlugin.getID(), Messages.getString("EditRelationOperation.0"), e ); //$NON-NLS-1$
      StatusDialog.open( m_shell, status, title );
    }
  }
}
