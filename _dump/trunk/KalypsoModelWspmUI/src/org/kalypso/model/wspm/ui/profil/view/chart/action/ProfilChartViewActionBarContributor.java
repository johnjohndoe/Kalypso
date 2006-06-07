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
package org.kalypso.model.wspm.ui.profil.view.chart.action;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.action.ICoolBarManager;
import org.eclipse.jface.action.IMenuManager;
import org.eclipse.jface.action.IStatusLineManager;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.ui.IEditorReference;
import org.eclipse.ui.IPartListener;
import org.eclipse.ui.IViewReference;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.kalypso.contribs.eclipse.ui.PartAdapter;
import org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartActionsEnum;
import org.kalypso.model.wspm.ui.profil.view.chart.ProfilChartView;

/**
 * @author Gernot Belger
 */
public class ProfilChartViewActionBarContributor implements IActionBarContributor
{
  private final StatusPosContributionItem m_statusPosItem = new StatusPosContributionItem( "profilChartViewStatusPosItem" );

  /** The currently active workbench part, the provider for the ProfilChartView. */
  private IWorkbenchPart m_part = null;

  private final IPartListener m_partListener = new PartAdapter()
  {
    /**
     * @see org.kalypso.contribs.eclipse.ui.PartAdapter#partActivated(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partActivated( IWorkbenchPart part )
    {
      ProfilChartViewActionBarContributor.this.partActivated( part );
    }

    /**
     * @see org.kalypso.contribs.eclipse.ui.PartAdapter#partClosed(org.eclipse.ui.IWorkbenchPart)
     */
    @Override
    public void partClosed( IWorkbenchPart part )
    {
      ProfilChartViewActionBarContributor.this.partClosed( part );
    }
  };

  private IWorkbenchPage m_page;

  private final List<ChartAction> m_chartActions = new ArrayList<ChartAction>();

  public ProfilChartViewActionBarContributor( )
  {
    for( final ProfilChartActionsEnum actionDesc : ProfilChartActionsEnum.values() )
      m_chartActions.add( new ChartAction( actionDesc ) );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#contributeTo(org.eclipse.jface.action.IMenuManager)
   */
  public void contributeTo( final IMenuManager menuManager )
  {
    for( final ChartAction action : m_chartActions )
    {
      // final String menuPath = action.getMenuPath();
      // TODO: find sub menu manager with the given relative path
      // implement similiar to object contributions
      // define menues and actions via the same mechanism
      // and make them here
      menuManager.add( action );
    }
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#dispose(org.eclipse.jface.action.IMenuManager)
   */
  public void dispose( final IMenuManager menuManager )
  {
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#contributeTo(org.eclipse.jface.action.ICoolBarManager)
   */
  public void contributeTo( final ICoolBarManager coolbarManager )
  {
    for( final ChartAction action : m_chartActions )
    {
      // TODO: find sub menu manager with the given relative path
      coolbarManager.add( action );
    }
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#dispose(org.eclipse.jface.action.ICoolBarManager)
   */
  public void dispose( final ICoolBarManager coolbarManager )
  {
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#contributeTo(org.eclipse.jface.action.IToolBarManager)
   */
  public void contributeTo( final IToolBarManager toolbarManager )
  {
    for( final ChartAction action : m_chartActions )
    {
      // TODO: find sub menu manager with the given relative path
      toolbarManager.add( action );
    }
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#dispose(org.eclipse.jface.action.IToolBarManager)
   */
  public void dispose( final IToolBarManager toolbarManager )
  {
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#contributeTo(org.eclipse.jface.action.IStatusLineManager)
   */
  public void contributeTo( final IStatusLineManager statuslineManager )
  {
    statuslineManager.add( m_statusPosItem );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#dispose(org.eclipse.jface.action.IStatusLineManager)
   */
  public void dispose( final IStatusLineManager statuslineManager )
  {
    statuslineManager.remove( m_statusPosItem );
  }

  /**
   * @see org.kalypso.contribs.eclipse.ui.actions.IActionBarContributor#dispose()
   */
  public void dispose( )
  {
    unhook();

    m_statusPosItem.dispose();

    m_page.removePartListener( m_partListener );
  }

  /**
   * Hooks with the given page. The status line will show always the position of the last active workbench part which
   * adapts to ProfilChartView.
   */
  public void init( final IWorkbenchPage page )
  {
    m_page = page;
    m_page.addPartListener( m_partListener );

    // try to find the part with the chart
    if( partActivated( page.getActiveEditor() ) )
      return;

    for( final IEditorReference reference : page.getEditorReferences() )
    {
      if( partActivated( reference.getPart( false ) ) )
        return;
    }

    for( final IViewReference reference : page.getViewReferences() )
    {
      if( partActivated( reference.getPart( false ) ) )
        return;
    }

    setChartView( null, null );
  }

  private void unhook( )
  {
    m_part = null;
  }

  protected boolean partActivated( final IWorkbenchPart part )
  {
    if( part == null )
      return false;

    final ProfilChartView chartView = (ProfilChartView) part.getAdapter( ProfilChartView.class );
    setChartView( part, chartView );

    return true;
  }

  protected void partClosed( final IWorkbenchPart part )
  {
    if( part == m_part )
      unhook();
  }

  private void setChartView( final IWorkbenchPart part, final ProfilChartView chartView )
  {
    unhook();

    m_part = part;

    m_statusPosItem.setChartView( chartView );

    for( final ChartAction action : m_chartActions )
      action.setProfilChartView( chartView );
  }

}
