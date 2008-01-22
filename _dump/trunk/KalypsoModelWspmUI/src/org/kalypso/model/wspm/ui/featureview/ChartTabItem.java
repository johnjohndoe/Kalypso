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
package org.kalypso.model.wspm.ui.featureview;

import java.util.HashMap;

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.menus.CommandContributionItem;
import org.kalypso.chart.framework.model.IChartModel;
import org.kalypso.chart.framework.model.impl.ChartModel;
import org.kalypso.chart.framework.view.ChartComposite;
import org.kalypso.chart.ui.IChartCommand;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.mousehandler.ChartDragHandlerDelegate;
import org.ksp.chart.factory.ChartType;

/**
 * Class for charts inserted as tabs into the chart feature control; this has to be isolated in a separate class as each
 * IChartPart can only return one ChartComposite and one ChartDragHandler
 * 
 * @author burtscher1
 */
public class ChartTabItem implements IChartPart
{
  private final ChartDragHandlerDelegate m_chartDragHandlerDelegate;

  private final ChartComposite m_chartComposite;

  private final Composite m_parent;

  public ChartTabItem( ChartType chartType, Composite folder, int style )
  {
    final TabItem item;
    if( folder instanceof TabFolder )
      item = new TabItem( (TabFolder) folder, SWT.NONE );
    else
      item = null;

    m_parent = new Composite( folder, style );
    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    gridLayout.horizontalSpacing = 0;
    gridLayout.verticalSpacing = 0;
    m_parent.setLayout( gridLayout );

    final ToolBarManager manager = new ToolBarManager( SWT.HORIZONTAL | SWT.FLAT );
    manager.createControl( m_parent );

    if( item != null )
    {
      item.setText( chartType.getTitle() );
      item.setToolTipText( chartType.getDescription() );
    }

    final IChartModel chartModel = new ChartModel();
    final ChartComposite chart = new ChartComposite( m_parent, SWT.BORDER, chartModel, new RGB( 255, 255, 255 ) );
    final GridData gridData = new GridData( SWT.FILL, SWT.FILL, true, true );

    chart.setLayoutData( gridData );

    m_chartDragHandlerDelegate = new ChartDragHandlerDelegate( chart );

    CommandContributionItem zoomIn = new CommandContributionItem( PlatformUI.getWorkbench(), "", IChartCommand.COMMAND_ZOOM_IN, new HashMap(), null, null, null, null, null, null, CommandContributionItem.STYLE_RADIO );
    CommandContributionItem zoomOut = new CommandContributionItem( PlatformUI.getWorkbench(), "", IChartCommand.COMMAND_ZOOM_OUT, new HashMap(), null, null, null, null, null, null, CommandContributionItem.STYLE_RADIO );
    CommandContributionItem pan = new CommandContributionItem( PlatformUI.getWorkbench(), "", IChartCommand.COMMAND_PAN, new HashMap(), null, null, null, null, null, null, CommandContributionItem.STYLE_RADIO );
    CommandContributionItem maximize = new CommandContributionItem( PlatformUI.getWorkbench(), "", IChartCommand.COMMAND_MAXIMIZE, new HashMap(), null, null, null, null, null, null, CommandContributionItem.STYLE_PUSH );
    manager.add( zoomIn );
    manager.add( zoomOut );
    manager.add( pan );
    manager.add( maximize );
    manager.update( true );

    m_chartComposite = chart;

    if( item != null )
      item.setControl( m_parent );
  }

  public Control getControl( )
  {
    return m_parent;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartComposite()
   */
  public ChartComposite getChartComposite( )
  {
    return m_chartComposite;
  }

  /**
   * @see org.kalypso.chart.ui.IChartPart#getChartDragHandler()
   */
  public ChartDragHandlerDelegate getChartDragHandler( )
  {
    return m_chartDragHandlerDelegate;
  }

  public void dispose( )
  {
    m_chartComposite.dispose();
  }
}
