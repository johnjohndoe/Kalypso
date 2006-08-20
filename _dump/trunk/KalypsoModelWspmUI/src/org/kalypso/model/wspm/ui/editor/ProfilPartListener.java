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
package org.kalypso.model.wspm.ui.editor;

import org.eclipse.jface.action.IAction;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IEditorSite;
import org.eclipse.ui.IPartListener2;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchPartReference;
import org.eclipse.ui.actions.ActionFactory;
import org.eclipse.ui.operations.OperationHistoryActionHandler;
import org.eclipse.ui.operations.UndoRedoActionGroup;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.ui.profil.operation.ProfilUndoContext;
import org.kalypso.model.wspm.ui.view.IProfilViewPart;

public final class ProfilPartListener implements IPartListener2
{
  private IProfilchartEditorListener m_editorListener = new IProfilchartEditorListener()
  {
    public void onProfilChanged( final ProfilchartEditor editor, final IProfil newprofil )
    {
      editor.getSite().getShell().getDisplay().asyncExec( new Runnable()
      {

        public void run( )
        {
          createUndoRedoGroup( editor );
        }
      } );
    }
  };

  private ProfilchartEditor m_editor = null;

  private UndoRedoActionGroup m_group = null;

  public void dispose( )
  {
    clearEditor();
  }

  public void partActivated( final IWorkbenchPartReference partRef )
  {
    onPartActivated( partRef.getPart( false ) );
  }

  public void partBroughtToTop( IWorkbenchPartReference partRef )
  {
  }

  public void partClosed( final IWorkbenchPartReference partRef )
  {
    final IWorkbenchPart part = partRef.getPart( false );
    final IWorkbenchPage page = part.getSite().getPage();

    if( part instanceof ProfilchartEditor && page != null )
      onPartActivated( page.getActiveEditor() );
  }

  public void partDeactivated( final IWorkbenchPartReference partRef )
  {
  }

  public void partOpened( final IWorkbenchPartReference partRef )
  {
    final IWorkbenchPart part = partRef.getPart( false );
    if( part instanceof ProfilchartEditor )
    {
      final IWorkbenchPage page = partRef.getPage();
      final IViewReference[] viewReferences = page.getViewReferences();
      for( int i = 0; i < viewReferences.length; i++ )
      {
        final IViewReference reference = viewReferences[i];
        final IViewPart view = reference.getView( false );
        if( view instanceof IProfilViewPart )
          ((IProfilViewPart)view).setProfilchartEditor( (ProfilchartEditor)part );
      }
    }
  }

  public void partHidden( IWorkbenchPartReference partRef )
  {
  }

  public void partVisible( IWorkbenchPartReference partRef )
  {
  }

  public void partInputChanged( IWorkbenchPartReference partRef )
  {
  }

  public void onPartActivated( final IWorkbenchPart part )
  {
    if( part != null )
    {
      IActionBars actionBars = null;
      if( part instanceof ProfilchartEditor )
      {
        final ProfilchartEditor profilchartEditor = (ProfilchartEditor)part;
        actionBars = (profilchartEditor).getEditorSite().getActionBars();
        setEditor( profilchartEditor );
      }
      else if( part instanceof IProfilViewPart )
      {
        final IProfilViewPart profilView = (IProfilViewPart)part;
        profilView.setProfilchartEditor( m_editor );

        actionBars = profilView.getViewSite().getActionBars();
      }

      // ensure, that ProfilPartViews have the same menus, toolbar and statusline item as
      // ProfilChartEditor

      // refresh global actions, they don't do it themself
      refreshGroupAction( actionBars, ActionFactory.UNDO );
      refreshGroupAction( actionBars, ActionFactory.REDO );

      if( actionBars != null )
        actionBars.updateActionBars();
    }
  }

  private void clearEditor( )
  {
    if( m_editor != null )
    {
      m_editor.removeProfilchartEditorListener( m_editorListener );
      m_editor = null;
    }
  }

  private void clearGroup( )
  {
    if( m_group != null )
    {
      m_group.dispose();
      m_group = null;
    }
  }

  private void refreshGroupAction( final IActionBars actionBars, final ActionFactory actionFactory )
  {
    if( actionBars != null )
    {
      final IAction handler = actionBars.getGlobalActionHandler( actionFactory.getId() );
      if( handler instanceof OperationHistoryActionHandler )
        ((OperationHistoryActionHandler)handler).update();
    }
  }

  private void setEditor( final IEditorPart targetEditor )
  {
    if( m_editor == targetEditor )
      return;

    clearEditor();
    clearGroup();

    if( targetEditor instanceof ProfilchartEditor )
    {
      m_editor = (ProfilchartEditor)targetEditor;
      m_editor.addProfilchartEditorListener( m_editorListener );

      createUndoRedoGroup( m_editor );
    }
  }

  protected void createUndoRedoGroup( final ProfilchartEditor editor )
  {
    clearGroup();

    final IEditorSite site = editor.getEditorSite();
    m_group = new UndoRedoActionGroup( site, new ProfilUndoContext( editor.getProfil() ), true );
    final IActionBars actionBars = site.getActionBars();
    m_group.fillActionBars( actionBars );
    actionBars.updateActionBars();
  }
}
