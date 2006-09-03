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
package org.kalypso.ui.editor.actions;

import org.eclipse.jface.action.ActionContributionItem;
import org.eclipse.jface.action.IAction;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.action.SubMenuManager;
import org.eclipse.jface.action.SubToolBarManager;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.ISelectionProvider;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.actions.ActionGroup;
import org.eclipse.ui.internal.ObjectActionContributorManager;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.selection.FeatureSelectionHelper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;
import org.kalypsodeegree.model.feature.Feature;

/**
 * This action group is used to show actions depending on a selected feature.
 * <p>
 * It is filled from the org.eclipse.ui.....popupMenu extension point.
 * 
 * @author Gernot Belger
 */
public class FeatureSelectionActionGroup extends ActionGroup
{
  private final ISelectionProvider m_provider = new ISelectionProvider()
  {
    public void addSelectionChangedListener( ISelectionChangedListener listener )
    {
    }

    public ISelection getSelection( )
    {
      return getContext().getSelection();
    }

    public void removeSelectionChangedListener( ISelectionChangedListener listener )
    {
    }

    public void setSelection( final ISelection selection )
    {
    }
  };

  private SubToolBarManager m_toolbarSubManager;

  private IWorkbenchPart m_part = null;

  private IMenuManager m_menuSubManager;

  private IActionBars m_actionBars;

  public void setPart( final IWorkbenchPart part )
  {
    m_part = part;
  }

  /**
   * @see org.eclipse.ui.actions.ActionGroup#fillActionBars(org.eclipse.ui.IActionBars)
   */
  @Override
  public void fillActionBars( final IActionBars actionBars )
  {
    m_actionBars = actionBars;

    final IMenuManager menuManager = actionBars.getMenuManager();
    fillMenu( menuManager );

    fillToolbar( actionBars.getToolBarManager() );
    fillStatusLine( actionBars.getStatusLineManager() );

    actionBars.updateActionBars();
  }

  private void fillMenu( final IMenuManager menuManager )
  {
  }

  private void fillToolbar( final IToolBarManager toolBarManager )
  {
    /* Shoudl be called only once */
    if( m_toolbarSubManager != null )
      throw new IllegalStateException( "Call this class only once" );

    m_toolbarSubManager = createSubToolbarManager( toolBarManager );
  }

  private void fillStatusLine( final IStatusLineManager statusLineManager )
  {
  }

  /**
   * @see org.eclipse.ui.actions.ActionGroup#updateActionBars()
   */
  @Override
  public void updateActionBars( )
  {
    updateMenu();
    updateToolbar();
  }

  private void updateMenu( )
  {
    if( m_menuSubManager == null )
    {
      final IMenuManager menuManager = m_actionBars.getMenuManager();
      m_menuSubManager = createSubMenuManager( menuManager );

    }

    if( m_menuSubManager == null )
      return;

    m_menuSubManager.removeAll();

    if( m_part == null )
      return;

    addNewSubMenu( m_menuSubManager );
    ObjectActionContributorManager.getManager().contributeObjectActions( m_part, m_menuSubManager, m_provider );
  }

  private void addNewSubMenu( final IMenuManager manager )
  {
    final ISelection selection = getContext().getSelection();
    if( !(selection instanceof IFeatureSelection) )
      return;

    final IFeatureSelection fs = (IFeatureSelection) selection;
    final IFeatureSelectionManager selectionManager = fs.getSelectionManager();
    final Feature feature = FeatureSelectionHelper.getFirstFeature( selectionManager );
    final CommandableWorkspace workspace = fs.getWorkspace( feature );

    final IMenuManager newMenuManager = FeatureActionUtilities.createFeatureNewMenu( fs, workspace, selectionManager );
    manager.add( newMenuManager );

    // add additions seperator: if not, eclipse whines
    manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
  }

  private void updateToolbar( )
  {
    if( m_toolbarSubManager == null )
      return;

    m_toolbarSubManager.removeAll();

    if( m_part == null )
      return;

    /* first, fill the actions into a fake manager */
    final MenuManager fakeManager = new MenuManager();
    ObjectActionContributorManager.getManager().contributeObjectActions( m_part, fakeManager, m_provider );

    /* now translate the contributions from the manager into the toolbar */
    final IContributionItem[] items = fakeManager.getItems();
    for( final IContributionItem item : items )
    {
      if( item instanceof ActionContributionItem )
      {
        final ActionContributionItem aci = (ActionContributionItem) item;

        final IAction action = aci.getAction();
        /* Only add items if they have an image, because else we get problems with editor toolbars. */
        if( action.getImageDescriptor() != null )
        {
          final ActionContributionItem newAci = new ActionContributionItem( action );
          newAci.setMode( aci.getMode() );
          newAci.setVisible( aci.isVisible() );
          m_toolbarSubManager.add( newAci );
        }
      }
    }

    /* release the fake manager */
    fakeManager.removeAll();
    fakeManager.dispose();

    m_toolbarSubManager.update( true );
  }

  /**
   * @see org.eclipse.ui.actions.ActionGroup#dispose()
   */
  @Override
  public void dispose( )
  {
    if( m_toolbarSubManager != null )
    {
      m_toolbarSubManager.disposeManager();
      m_toolbarSubManager = null;
    }

    if( m_menuSubManager != null )
    {
      m_menuSubManager.dispose();
      if( m_menuSubManager instanceof SubMenuManager )
        ((SubMenuManager) m_menuSubManager).disposeManager();

      m_menuSubManager = null;
    }
  }

  /**
   * Creates the sub-menu where the selection-dependent contribution items are added.
   * <p>
   * Default implementation just returns null, so no additional menu-items are shown.
   * </p>
   * <p>
   * Intended to be overwritten by clients.
   * </p>
   */
  protected IMenuManager createSubMenuManager( final IMenuManager menuManager )
  {
    return null;
  }

  /**
   * Creates the sub-toolbar where the selection-dependent contribution items are added.
   * <p>
   * Default implementation just creates a sub-toolbar and makes it visible.
   * </p>
   * <p>
   * Intended to be overwritten by clients.
   * </p>
   */
  protected SubToolBarManager createSubToolbarManager( final IToolBarManager toolBarManager )
  {
    final SubToolBarManager subToolBarManager = new SubToolBarManager( toolBarManager );
    subToolBarManager.setVisible( true );
    return subToolBarManager;
  }

}
