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
package org.kalypso.kalypsomodel1d2d.ui.map.toolbar;

import java.util.Arrays;
import java.util.List;
import java.util.logging.Logger;

import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.IContributionManagerOverrides;
import org.eclipse.jface.action.IToolBarManager;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.ui.IActionBars;
import org.eclipse.ui.IViewPart;
import org.eclipse.ui.IViewSite;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.kalypsosimulationmodel.core.Assert;

/**
 * Provides the mechanism to disable map view contribution items. The mechanism consists of decorating the manager
 * overrides with with the behavior of de-activating the items of specified id
 * 
 * @author Patrice Congo
 * 
 */
public class MapActionDisabler implements IContributionManagerOverrides
{
  private static final Logger logger = Logger.getLogger( MapActionDisabler.class.getName() );

  /** holds the ids of the items to disable */
  private List<String> itemsIDToDisable;

  /** the original manager overrides */
  private IContributionManagerOverrides decorated;

  /**
   * Creates a new Instance of {@link MapActionDisabler} which disables the items with the following links:
   * <ul>
   * <li/>"org.kalypso.ui.editor.mapeditor.action.PanAction"
   * <li/>"org.kalypso.ui.editor.mapeditor.action.ZoomInAction"
   * </ul>
   * 
   */
  public MapActionDisabler( )
  {
    this( new String[] { "org.kalypso.ui.editor.mapeditor.action.PanAction", //$NON-NLS-1$
        "org.kalypso.ui.editor.mapeditor.action.ZoomInAction" } ); //$NON-NLS-1$
  }

  /**
   * Creates {@link MapActionDisabler} instance which disables the items of the given ids
   * 
   * @param itemsIds
   *          the ids of the items to disable
   * @throws IllegalArgumentException
   *           if itemsIds is null or contains a null
   * 
   */
  public MapActionDisabler( String[] itemsIds )
  {
    Assert.throwIAEOnNullParam( itemsIds, "itemsIds" ); //$NON-NLS-1$
    itemsIDToDisable = Arrays.asList( itemsIds );
  }

  /**
   * Disables the items
   */
  public void disableActions( )
  {
    IViewPart findView = UtilMap.getMapView();
    IActionBars actionBars = findView.getViewSite().getActionBars();
    IToolBarManager toolBarManager = actionBars.getToolBarManager();
    IContributionManagerOverrides overrides = toolBarManager.getOverrides();
    this.decorated = overrides;
    if( toolBarManager instanceof ToolBarManager )
    {
      ((ToolBarManager) toolBarManager).setOverrides( this );

    }
    toolBarManager.update( true );
    for( String itemId : itemsIDToDisable )
    {
      IContributionItem find = toolBarManager.find( itemId );
      if( find != null )
      {
        find.update();
        // find.setVisible( false );
      }
      else
      {
        logger.warning( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.toolbar.MapActionDisabler.3" ) + itemId ); //$NON-NLS-1$
      }
    }

  }

  /**
   * Enables the items.
   * 
   * This method resets the tool bar manager overrides and update the items to disable.
   */
  public void enableActions( )
  {

    IViewPart findView = UtilMap.getMapView();
    if( findView == null )
    {
      logger.warning( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.toolbar.MapActionDisabler.4" ) ); //$NON-NLS-1$
      return;
    }

    IViewSite viewSite = findView.getViewSite();
    if( viewSite == null )
    {
      logger.warning( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.toolbar.MapActionDisabler.5" ) ); //$NON-NLS-1$
      return;
    }
    IActionBars actionBars = viewSite.getActionBars();
    IToolBarManager toolBarManager = actionBars.getToolBarManager();
    if( toolBarManager instanceof ToolBarManager )
    {
      ((ToolBarManager) toolBarManager).setOverrides( decorated );
    }
    for( String itemId : itemsIDToDisable )
    {
      IContributionItem find = toolBarManager.find( itemId );
      if( find != null )
      {
        // find.setVisible( true );
        find.update();
      }
      else
      {
        logger.warning( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.toolbar.MapActionDisabler.6" ) + itemId ); //$NON-NLS-1$
      }
    }
    toolBarManager.update( true );
  }

  /**
   * @see org.eclipse.jface.action.IContributionManagerOverrides#getAccelerator(org.eclipse.jface.action.IContributionItem)
   */
  @Override
  public Integer getAccelerator( IContributionItem item )
  {
    return decorated.getAccelerator( item );
  }

  /**
   * @see org.eclipse.jface.action.IContributionManagerOverrides#getAcceleratorText(org.eclipse.jface.action.IContributionItem)
   */
  @Override
  public String getAcceleratorText( IContributionItem item )
  {
    return decorated.getAcceleratorText( item );
  }

  /**
   * @see org.eclipse.jface.action.IContributionManagerOverrides#getEnabled(org.eclipse.jface.action.IContributionItem)
   */
  @Override
  public Boolean getEnabled( IContributionItem item )
  {
    final String itemID = item.getId();
    if( itemsIDToDisable.contains( itemID ) )
    {
      return Boolean.FALSE;
    }
    else
    {
      return decorated.getEnabled( item );
    }
  }

  /**
   * @see org.eclipse.jface.action.IContributionManagerOverrides#getText(org.eclipse.jface.action.IContributionItem)
   */
  @Override
  public String getText( IContributionItem item )
  {
    return decorated.getText( item );
  }

  /**
   * @see org.eclipse.jface.action.IContributionManagerOverrides#getVisible(org.eclipse.jface.action.IContributionItem)
   */
  @Override
  public Boolean getVisible( IContributionItem item )
  {
    return Boolean.TRUE;
  }
}