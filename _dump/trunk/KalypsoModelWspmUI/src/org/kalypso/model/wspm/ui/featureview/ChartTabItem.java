/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IExecutionListener;
import org.eclipse.core.commands.NotHandledException;
import org.eclipse.core.expressions.IEvaluationContext;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.swt.widgets.ToolItem;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.commands.ICommandService;
import org.eclipse.ui.handlers.IHandlerService;
import org.eclipse.ui.menus.CommandContributionItem;
import org.kalypso.chart.framework.model.IChartModel;
import org.kalypso.chart.framework.model.impl.ChartModel;
import org.kalypso.chart.framework.view.ChartComposite;
import org.kalypso.chart.ui.IChartPart;
import org.kalypso.chart.ui.editor.commandhandler.ChartHandlerUtilities;
import org.kalypso.chart.ui.editor.mousehandler.ChartDragHandlerDelegate;

/**
 * Class for charts inserted as tabs into the chart feature control; this has to be isolated in a seperate class as each
 * IChartPart can only return one ChartComposite and one ChartDragHandler
 * 
 * @author burtscher1
 */
public class ChartTabItem extends Composite implements IChartPart
{
  private final ChartDragHandlerDelegate m_chartDragHandlerDelegate;

  private final ChartComposite m_chartComposite;

  private final IExecutionListener m_executionListener;

  public ChartTabItem( final Composite parent, final int style, final Map<String, Integer> commands )
  {
    super( parent, style );

    final GridLayout gridLayout = new GridLayout();
    gridLayout.marginHeight = 0;
    gridLayout.marginWidth = 0;
    gridLayout.horizontalSpacing = 0;
    gridLayout.verticalSpacing = 0;
    setLayout( gridLayout );

    final IWorkbench serviceLocator = PlatformUI.getWorkbench();
    final ToolBarManager manager = new ToolBarManager( SWT.HORIZONTAL | SWT.FLAT );
    if( commands.size() > 0 )
    {
      manager.createControl( this );
      for( final Entry<String, Integer> entry : commands.entrySet() )
      {
        final String cmdId = entry.getKey();
        final Integer cmdStyle = entry.getValue();

        final CommandContributionItem contribItem = new CommandContributionItem( serviceLocator, cmdId + "_item_", cmdId, new HashMap<Object, Object>(), null, null, null, null, null, null, cmdStyle );
        manager.add( contribItem );
      }
      manager.update( true );
    }

    final IChartModel chartModel = new ChartModel();
    m_chartComposite = new ChartComposite( this, SWT.BORDER, chartModel, new RGB( 255, 255, 255 ) );
    final GridData gridData = new GridData( SWT.FILL, SWT.FILL, true, true );

    m_chartComposite.setLayoutData( gridData );

    m_chartDragHandlerDelegate = new ChartDragHandlerDelegate( m_chartComposite );

    final ICommandService cmdService = (ICommandService) serviceLocator.getService( ICommandService.class );
    final IHandlerService handlerService = (IHandlerService) serviceLocator.getService( IHandlerService.class );

    m_executionListener = new IExecutionListener()
    {
      public void notHandled( final String commandId, final NotHandledException exception )
      {
      }

      public void preExecute( final String commandId, final ExecutionEvent event )
      {
        if( !commands.keySet().contains( commandId ) )
          return;

        final Event trigger = (Event) event.getTrigger();
        final ToolItem toolItem = (ToolItem) trigger.widget;
        final ToolBar parentToolbar = toolItem.getParent();
        final ToolBar managerToolbar = manager.getControl();

        if( commands.keySet().contains( commandId ) && parentToolbar == managerToolbar )
        {
          final IEvaluationContext context = (IEvaluationContext) event.getApplicationContext();
          context.addVariable( ChartHandlerUtilities.ACTIVE_CHART_PART_NAME, ChartTabItem.this );
        }
      }

      public void postExecuteFailure( final String commandId, final ExecutionException exception )
      {
        if( !commands.keySet().contains( commandId ) )
          return;

        final IEvaluationContext currentState = handlerService.getCurrentState();
        currentState.removeVariable( ChartHandlerUtilities.ACTIVE_CHART_PART_NAME );

        // REMARK: it would be nice to have an error mesage here, but:
        // If we have several tabs, we get several msg-boxes, as we have several listeners.
        // How-to avoid that??
// final IStatus errorStatus = StatusUtilities.createStatus( IStatus.ERROR, "Kommando mit Fehler beendet", exception );
// ErrorDialog.openError( getShell(), "Kommando ausführen", "Fehler bei der Ausführung eines Kommandos", errorStatus );
      }

      public void postExecuteSuccess( final String commandId, final Object returnValue )
      {
        if( !commands.keySet().contains( commandId ) )
          return;

        final IEvaluationContext currentState = handlerService.getCurrentState();
        currentState.removeVariable( ChartHandlerUtilities.ACTIVE_CHART_PART_NAME );
      }
    };

    cmdService.addExecutionListener( m_executionListener );
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

  @Override
  public void dispose( )
  {
    final ICommandService cmdService = (ICommandService) PlatformUI.getWorkbench().getService( ICommandService.class );
    cmdService.removeExecutionListener( m_executionListener );

    m_chartComposite.dispose();
  }
}
