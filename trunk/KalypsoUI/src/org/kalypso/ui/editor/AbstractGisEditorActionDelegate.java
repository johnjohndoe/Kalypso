/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor;

import org.eclipse.jface.action.IAction;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorActionDelegate;
import org.eclipse.ui.IEditorPart;
import org.kalypso.ui.editor.gistableeditor.GisTableEditor;
import org.kalypso.ui.editor.gmleditor.ui.GMLEditor;
import org.kalypso.ui.editor.mapeditor.GisMapEditor;
import org.kalypsodeegree.model.feature.event.ModellEvent;
import org.kalypsodeegree.model.feature.event.ModellEventListener;

/**
 * @author belger
 */
public abstract class AbstractGisEditorActionDelegate implements IEditorActionDelegate,
    ModellEventListener
{
  private AbstractEditorPart m_editor;

  private IAction m_action;

  public static int c = 0;

  /**
   * @see org.eclipse.ui.IEditorActionDelegate#setActiveEditor(org.eclipse.jface.action.IAction,
   *      org.eclipse.ui.IEditorPart)
   */
  public void setActiveEditor( final IAction action, final IEditorPart targetEditor )
  {
    m_action = action;
    // disconnect old editor
    if( m_editor != null )
    {
      if( m_editor instanceof GisTableEditor )
        ( (GisTableEditor)m_editor ).getLayerTable().removeModellListener( this );
      if( m_editor instanceof GisMapEditor )
      {
        ( (GisMapEditor)m_editor ).getMapPanel().removeModellListener(this);
      }
      if( m_editor instanceof GMLEditor )
      {
        ((GMLEditor) m_editor).getTreeView().removeModellListener(this);
      }
    }
    m_editor = (AbstractEditorPart)targetEditor;

    // connect new editor
    if( m_editor != null )
    {
      if( m_editor instanceof GisTableEditor )
        ( (GisTableEditor)m_editor ).getLayerTable().addModellListener( this );
      if( m_editor instanceof GisMapEditor )
      {
        ( (GisMapEditor)m_editor ).getMapPanel().addModellListener(this);
      }
      if( m_editor instanceof GMLEditor )
      {
        ((GMLEditor) m_editor).getTreeView().addModellListener(this);
      }
    }

    refreshAction();
  }

  /**
   * @see org.eclipse.ui.IActionDelegate#selectionChanged(org.eclipse.jface.action.IAction,
   *      org.eclipse.jface.viewers.ISelection)
   */
  public void selectionChanged( final IAction action, final ISelection selection )
  {
    m_action = action;
  }

  protected abstract void refreshAction();

  /**
   * make cast to your editor
   */
  protected AbstractEditorPart getEditor()
  {
    return m_editor;
  }

  protected IAction getAction()
  {
    return m_action;
  }

  /**
   * @param action
   * @see org.eclipse.ui.IActionDelegate2#init(org.eclipse.jface.action.IAction)
   */
  public void init( final IAction action )
  {
    m_action = action;
  }

  /**
   * @see org.kalypsodeegree.model.feature.event.ModellEventListener#onModellChange(org.kalypsodeegree.model.feature.event.ModellEvent)
   */
  public void onModellChange( final ModellEvent modellEvent )
  {
    refreshAction();
  }
}