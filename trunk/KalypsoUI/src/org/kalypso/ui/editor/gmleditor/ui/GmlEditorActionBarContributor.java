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
package org.kalypso.ui.editor.gmleditor.ui;

import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.actions.ActionContext;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.kalypso.i18n.Messages;
import org.kalypso.ui.editor.actions.FeatureSelectionActionGroup;

/**
 * Contributes to the actions bars of the {@link org.kalypso.ui.editor.gmleditor.GmlEditor}.
 * 
 * @author Gernot Belger
 */
public class GmlEditorActionBarContributor extends EditorActionBarContributor
{
  private final FeatureSelectionActionGroup m_featureSelectionActionGroup = new FeatureSelectionActionGroup()
  {
    /**
     * @see org.kalypso.ui.editor.actions.FeatureSelectionActionGroup#createSubMenuManager(org.eclipse.jface.action.IMenuManager)
     */
    @Override
    protected IMenuManager createSubMenuManager( final IMenuManager menuManager )
    {
      final IMenuManager manager = menuManager.findMenuUsingPath( "org.kalypso.ui.editors.treeeditor.menu" ); //$NON-NLS-1$
      if( manager == null )
        return null;

      manager.remove( "selectionMenuManager" ); //$NON-NLS-1$

      final IMenuManager newSmm = new MenuManager( Messages.getString("org.kalypso.ui.editor.gmleditor.ui.GmlEditorActionBarContributor.2"), "selectionMenuManager" ); //$NON-NLS-1$ //$NON-NLS-2$
      manager.appendToGroup( "selection", newSmm ); //$NON-NLS-1$
      return newSmm;
    }
  };

  private final ISelectionChangedListener m_selectionListener = new ISelectionChangedListener()
  {
    public void selectionChanged( final SelectionChangedEvent event )
    {
      handleSelectionChanged( event.getSelectionProvider() );
    }
  };

  private ShowDescriptionStatusLineItem m_statusLineItem;

  private IEditorPart m_targetEditor;

  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#init(org.eclipse.ui.IActionBars)
   */
  @Override
  public void init( final IActionBars bars )
  {
    super.init( bars );

    m_featureSelectionActionGroup.setContext( new ActionContext( StructuredSelection.EMPTY ) );
    m_featureSelectionActionGroup.fillActionBars( bars );
  }

  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_targetEditor != null )
    {
      final ISelectionProvider selectionProvider = m_targetEditor.getSite().getSelectionProvider();
      if( selectionProvider != null )
        selectionProvider.removeSelectionChangedListener( m_selectionListener );
    }

    m_featureSelectionActionGroup.dispose();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#contributeToStatusLine(org.eclipse.jface.action.IStatusLineManager)
   */
  @Override
  public void contributeToStatusLine( final IStatusLineManager statusLineManager )
  {
    m_statusLineItem = new ShowDescriptionStatusLineItem( "descriptionItem", 70 ); //$NON-NLS-1$
    statusLineManager.add( m_statusLineItem );
  }

  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#setActiveEditor(org.eclipse.ui.IEditorPart)
   */
  @Override
  public void setActiveEditor( final IEditorPart targetEditor )
  {
    if( m_targetEditor != null )
    {
      final ISelectionProvider selectionProvider = m_targetEditor.getSite().getSelectionProvider();
      if( selectionProvider != null )
        selectionProvider.removeSelectionChangedListener( m_selectionListener );
    }

    m_targetEditor = targetEditor;
    m_featureSelectionActionGroup.setPart( m_targetEditor );

    if( m_targetEditor != null )
    {
      final ISelectionProvider selectionProvider = m_targetEditor.getSite().getSelectionProvider();
      if( selectionProvider != null )
        selectionProvider.addSelectionChangedListener( m_selectionListener );
    }

    if( m_statusLineItem != null )
      m_statusLineItem.setActiveEditor( targetEditor );

    m_featureSelectionActionGroup.updateActionBars();
  }

  protected void handleSelectionChanged( final ISelectionProvider provider )
  {
    m_featureSelectionActionGroup.getContext().setSelection( provider.getSelection() );
    m_featureSelectionActionGroup.updateActionBars();
  }
}
