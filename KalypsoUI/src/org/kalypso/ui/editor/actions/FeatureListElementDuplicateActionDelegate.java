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
package org.kalypso.ui.editor.actions;

import java.util.List;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.dialogs.ErrorDialog;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IObjectActionDelegate;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.i18n.Messages;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.gmleditor.util.command.AddFeatureCommand;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree_impl.model.feature.FeatureHelper;

/**
 * Implements the 'duplicate feature' popup menu.
 * 
 * @author Gernot Belger
 */
public class FeatureListElementDuplicateActionDelegate implements IObjectActionDelegate
{
  private IWorkbenchPart m_targetPart;

  private Feature m_selectedFeature = null;

  private CommandableWorkspace m_workspace = null;

  private IFeatureSelection m_selection;

  /**
   * @see org.eclipse.ui.IObjectActionDelegate#setActivePart(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IWorkbenchPart)
   */
  public void setActivePart( final IAction action, final IWorkbenchPart targetPart )
  {
    m_targetPart = targetPart;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    if( m_selectedFeature == null )
      return;

    final IRelationType rt = m_selectedFeature.getParentRelation();
    if( rt != null )
    {
      final Feature parent = m_selectedFeature.getParent();
      final List list = (List) parent.getProperty( rt );
      if( list != null )
      {
        try
        {
          final IFeatureSelectionManager selectionManager = m_selection.getSelectionManager();
          final Feature newFeature = FeatureHelper.cloneFeature( parent, rt, m_selectedFeature );
          final int pos = list.indexOf( m_selectedFeature ) + 1;
          final AddFeatureCommand command = new AddFeatureCommand( m_workspace, parent, rt, pos, newFeature, selectionManager, true, false );
          
          m_workspace.postCommand( command );
        }
        catch( final Exception e )
        {
          final IStatus status = StatusUtilities.createStatus( IStatus.ERROR, "", e ); //$NON-NLS-1$
          KalypsoGisPlugin.getDefault().getLog().log( status );

          // we are in the ui-thread so we get a shell here
          final Shell shell = m_targetPart.getSite().getShell();
          if( shell != null )
            ErrorDialog.openError( shell, action.getText(), Messages.getString("org.kalypso.ui.editor.actions.FeatureListElementDuplicateActionDelegate.1"), status ); //$NON-NLS-1$
        }
      }
    }
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_selection = null;
    m_workspace = null;
    m_selectedFeature = null;

    action.setEnabled( false );

    /* The action will be enabled iff we have a feature inside a list. */
    if( !selection.isEmpty() && selection instanceof IFeatureSelection )
    {
      m_selection = (IFeatureSelection) selection;
      m_selectedFeature = FeatureSelectionHelper.getFirstFeature( m_selection );
      m_workspace = m_selection.getWorkspace( m_selectedFeature );

      final IRelationType rt = m_selectedFeature.getParentRelation(  );
      if( rt != null )
      {
        final List list = (List) m_selectedFeature.getParent().getProperty( rt );
        if( list != null )
          action.setEnabled( true );
      }
    }
  }
}
