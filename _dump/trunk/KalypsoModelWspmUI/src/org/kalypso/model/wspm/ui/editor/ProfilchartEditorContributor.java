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
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchActionConstants;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.part.EditorActionBarContributor;
import org.kalypso.contribs.eclipse.ui.actions.RetargetActionManager;
import org.kalypso.contribs.eclipse.ui.actions.RetargetInfoComparator;
import org.kalypso.contribs.eclipse.ui.actions.RetargetActionManager.RetargetInfo;
import org.kalypso.model.wspm.ui.KalypsoModelWspmUIImages;
import org.kalypso.model.wspm.ui.Messages;
import org.kalypso.model.wspm.ui.editor.actions.ChartActionHandler;
import org.kalypso.model.wspm.ui.editor.actions.ShowTableAction;
import org.kalypso.model.wspm.ui.view.chart.action.ProfilChartActionsEnum;
import org.kalypso.model.wspm.ui.view.chart.action.StatusPosContributionItem;

/**
 * @author Belger
 */
public class ProfilchartEditorContributor extends EditorActionBarContributor
{
  public static final String RETARGET_INSERT = "com.bce.profil.eclipse.editor.globalAction.insert"; //$NON-NLS-1$

  public static final String RETARGET_OPEN_TABLE = "com.bce.profil.eclipse.editor.globalAction.openTable"; //$NON-NLS-1$

  public static final String RETARGET_MAXIMIZE = "com.bce.profil.eclipse.editor.globalAction.maximizeChart"; //$NON-NLS-1$

  private final ProfilPartListener m_profilPartListener = new ProfilPartListener();

  private final StatusPosContributionItem m_statusLineItem = new StatusPosContributionItem( "pos" ); //$NON-NLS-1$

  private final RetargetActionManager m_retargetManager = new RetargetActionManager();

  private static final String MENU_ID_PROFIL = "com.bce.profil.wspprofilapp.profileditor"; //$NON-NLS-1$

  public ProfilchartEditorContributor( )
  {
    final RetargetInfo insertPointInfo = m_retargetManager.addRetargetInfo( new RetargetActionManager.RetargetInfo( RETARGET_INSERT, Messages.ProfilchartEditorContributor_5, IAction.AS_PUSH_BUTTON ) );
    insertPointInfo.getRetargetAction().setToolTipText( Messages.ProfilchartEditorContributor_6 );
    insertPointInfo.setMenuPath( IWorkbenchActionConstants.M_EDIT + IWorkbenchActionConstants.SEP + IWorkbenchActionConstants.CUT_EXT );

    final RetargetInfo showTableInfo = m_retargetManager.addRetargetInfo( new RetargetActionManager.RetargetInfo( RETARGET_OPEN_TABLE, Messages.ProfilchartEditorContributor_7, IAction.AS_PUSH_BUTTON ) );
    showTableInfo.setActionHandler( new ShowTableAction() );
    showTableInfo.getRetargetAction().setToolTipText( Messages.ProfilchartEditorContributor_8 );
    showTableInfo.getRetargetAction().setImageDescriptor( KalypsoModelWspmUIImages.ID_ENABLED_OPEN_TABLE );
    showTableInfo.setMenuPath( MENU_ID_PROFIL + IWorkbenchActionConstants.SEP + IWorkbenchActionConstants.MB_ADDITIONS );
    showTableInfo.setToolbarGroup( IWorkbenchActionConstants.MB_ADDITIONS );

    final ProfilChartActionsEnum[] enums = ProfilChartActionsEnum.values();
    for( final ProfilChartActionsEnum chartAction : enums )
    {
      final int style = chartAction.getStyle();
      final RetargetInfo chartActionInfo = m_retargetManager.addRetargetInfo( new RetargetActionManager.RetargetInfo( chartAction.name(), chartAction.toString(), style ) );
      chartActionInfo.setActionHandler( new ChartActionHandler( chartAction ) );
      chartActionInfo.getRetargetAction().setToolTipText( chartAction.getTooltip() );
      chartActionInfo.getRetargetAction().setImageDescriptor( chartAction.getEnabledImage() );
      chartActionInfo.getRetargetAction().setDisabledImageDescriptor( chartAction.getDisabledImage() );
      
          
      chartActionInfo.setMenuPath( chartAction.getMenuPath() );
      final String menuStyleGroup = RetargetActionManager.menuGroupForStyle( style );
      if( chartAction.getMenuPath() != null )
      {
        chartActionInfo.setMenuGroup( MENU_ID_PROFIL + IWorkbenchActionConstants.SEP + chartAction.getMenuPath() );
        chartActionInfo.setToolbarGroup( chartAction.getMenuPath() );
       
      }
      else
      {
        chartActionInfo.setMenuGroup( MENU_ID_PROFIL + IWorkbenchActionConstants.SEP + menuStyleGroup );
        chartActionInfo.setToolbarGroup( menuStyleGroup );
      }
    }
    m_retargetManager.sortRetargetInfos( new RetargetInfoComparator() );
  }

  @Override
  public void init( final IActionBars bars, final IWorkbenchPage page )
  {
    super.init( bars, page );

    m_retargetManager.registerGlobalActionHandlers( bars );

    bars.updateActionBars();

    m_retargetManager.addPartListeners( page );

    final IWorkbenchPart activePart = page.getActivePart();
    if( activePart != null )
      m_retargetManager.partActivated( activePart );

    page.addPartListener( m_profilPartListener );
  }

  @Override
  public void dispose( )
  {
    m_profilPartListener.dispose();

    final IWorkbenchPage page = getPage();
    final IActionBars bars = getActionBars();
    if( page != null )
    {
      page.removePartListener( m_profilPartListener );
      m_retargetManager.disposeActions( bars, page );
    }

    bars.updateActionBars();

    super.dispose();
  }

  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#setActiveEditor(org.eclipse.ui.IEditorPart)
   */
  @Override
  public void setActiveEditor( final IEditorPart targetEditor )
  {
    super.setActiveEditor( targetEditor );

    if( targetEditor instanceof ProfilchartEditor )
    {
//      final ProfilchartEditor profilchartEditor = (ProfilchartEditor) targetEditor;
//      m_statusLineItem.setEditor( profilchartEditor );
    }

    m_retargetManager.setActiveEditor( targetEditor );

    m_profilPartListener.onPartActivated( targetEditor );
  }

  @Override
  public void contributeToToolBar( final IToolBarManager toolBarManager )
  {
    super.contributeToToolBar( toolBarManager );
    toolBarManager.add( new Separator( RetargetActionManager.MENU_GROUP_CHECK ) );
    toolBarManager.add( new Separator( RetargetActionManager.MENU_GROUP_PUSH ) );
    toolBarManager.add( new Separator( RetargetActionManager.MENU_GROUP_RADIO ) );
    toolBarManager.add( new Separator( RetargetActionManager.MENU_GROUP_MENU ) );
    toolBarManager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );

    m_retargetManager.contributeToToolBar( toolBarManager );
  }

  @Override
  public void contributeToMenu( final IMenuManager menuManager )
  {
    super.contributeToMenu( menuManager );
    final MenuManager profilMenu = new MenuManager( "Profil", MENU_ID_PROFIL ); //$NON-NLS-1$
    menuManager.insertAfter( IWorkbenchActionConstants.MB_ADDITIONS, profilMenu );

    profilMenu.add( new Separator( RetargetActionManager.MENU_GROUP_PUSH ) );
    profilMenu.add( new Separator( RetargetActionManager.MENU_GROUP_RADIO ) );
    profilMenu.add( new Separator( RetargetActionManager.MENU_GROUP_CHECK ) );
    final Separator menuSep = new Separator( RetargetActionManager.MENU_GROUP_MENU );
    profilMenu.add( menuSep );
    profilMenu.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );

    
    m_retargetManager.contributeToMenu( menuManager );

    
  }

  /**
   * @see org.eclipse.ui.part.EditorActionBarContributor#contributeToStatusLine(org.eclipse.jface.action.IStatusLineManager)
   */
  @Override
  public void contributeToStatusLine( final IStatusLineManager statusLineManager )
  {
    statusLineManager.add( m_statusLineItem );
  }

  public void fillContextMenu( final IMenuManager manager )
  {
    manager.add( new Separator( RetargetActionManager.MENU_GROUP_CHECK ) );
    manager.add( new Separator( RetargetActionManager.MENU_GROUP_RADIO ) );
    manager.add( new Separator( RetargetActionManager.MENU_GROUP_PUSH ) );
    manager.add( new Separator( RetargetActionManager.MENU_GROUP_MENU ) );
    manager.add( new Separator( IWorkbenchActionConstants.MB_ADDITIONS ) );
    
    
    
    m_retargetManager.fillContextMenu( manager );

  }

  public RetargetActionManager getRetargetManager( )
  {
    return m_retargetManager;
  }
}
