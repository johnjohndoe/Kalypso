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
package org.kalypso.ui.editor.gmleditor.util.actions;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeContentProvider;
import org.eclipse.jface.viewers.TreeViewer;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ui.editor.gmleditor.ui.GMLContentProvider;
import org.kalypso.ui.editor.gmleditor.ui.GmlEditor;

/**
 * @author Gernot Belger
 */
public class GoIntoActionDelegate implements IEditorActionDelegate
{
  private GmlEditor m_targetEditor;

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    m_targetEditor = (GmlEditor) targetEditor;
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#run(org.eclipse.jface.action.IAction)
   */
  public void run( final IAction action )
  {
    final TreeViewer treeViewer = m_targetEditor.getTreeView().getTreeViewer();
    final IContentProvider cp = treeViewer.getContentProvider();
    if( !(cp instanceof GMLContentProvider) )
      return;

    final GMLContentProvider contentProvider = (GMLContentProvider) cp;
    final IStructuredSelection selection = (IStructuredSelection) treeViewer.getSelection();
    contentProvider.goInto( selection.getFirstElement() );

    m_targetEditor.fireDirty();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    final IStructuredSelection structSel = (IStructuredSelection) selection;
    if( m_targetEditor == null )
    {
      action.setEnabled( false );
      return;
    }

    final TreeViewer treeViewer = m_targetEditor.getTreeView().getTreeViewer();
    final ITreeContentProvider cp = (ITreeContentProvider) treeViewer.getContentProvider();
    action.setEnabled( structSel.size() == 1 && cp.hasChildren( structSel.getFirstElement() ) );
  }
}
