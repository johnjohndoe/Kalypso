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
package org.kalypso.ui.rrm.internal.timeseries.view.edit;

import java.net.URL;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.services.IEvaluationService;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.chart.ui.editor.commandhandler.ChartSourceProvider;
import org.kalypso.chart.ui.editor.mousehandler.ZoomPanMaximizeHandler;
import org.kalypso.chart.ui.editor.mousehandler.ZoomPanMaximizeHandler.DIRECTION;
import org.kalypso.contribs.eclipse.jface.action.ContributionUtils;
import org.kalypso.zml.core.base.IMultipleZmlSourceElement;
import org.kalypso.zml.ui.chart.layer.selection.ZmlChartSelectionChangedHandler;
import org.kalypso.zml.ui.chart.view.DiagramCompositeSelection;
import org.kalypso.zml.ui.chart.view.ZmlDiagramLayerListener;
import org.kalypso.zml.ui.debug.KalypsoZmlUiDebug;

import de.openali.odysseus.chart.framework.model.impl.ChartModel;
import de.openali.odysseus.chart.framework.view.impl.ChartImageComposite;

/**
 * @author Dirk Kuch
 */
public class TimeseriesChartComposite extends Composite
{
  private static final RGB CHART_BACKGROUND = new RGB( 255, 255, 255 );

  private final FormToolkit m_toolkit;

  private final IServiceLocator m_context;

  private final URL m_template;

  private final ChartModel m_model;

  private ChartImageComposite m_chartComposite;

  private ChartSourceProvider m_chartSourceProvider;

  /**
   * The initialize job.
   */
  private Job m_initializeJob;

  /**
   * True, if the chart was initialized.
   */
  private boolean m_initialized;

  /**
   * This source is set, after the chart is initialized.
   */
  private IMultipleZmlSourceElement m_initializeSource;

  private final ZmlDiagramLayerListener m_layerManagerListener;

  public TimeseriesChartComposite( final Composite parent, final FormToolkit toolkit, final IServiceLocator context, final URL template )
  {
    super( parent, SWT.NULL );

    m_toolkit = toolkit;
    m_context = context;
    m_template = template;
    m_model = new ChartModel();
    m_chartComposite = null;
    m_chartSourceProvider = null;
    m_initializeJob = null;
    m_initialized = false;
    m_initializeSource = null;

    m_layerManagerListener = new ZmlDiagramLayerListener( m_model );

    GridLayoutFactory.fillDefaults().spacing( 0, 0 ).applyTo( this );
    draw();
    toolkit.adapt( this );
  }

  public void deactivate( )
  {
    m_model.getLayerManager().getEventHandler().removeListener( m_layerManagerListener );

    m_chartSourceProvider.dispose();
  }

  public synchronized void setSelection( final IMultipleZmlSourceElement source )
  {
    /* In this case, the chart is already initialized. */
    if( m_initialized )
    {
      /* Only set the selection. */
      setSelectionInternal( source );
      return;
    }

    /* Store the selection, so that it can be set later. */
    /* Case 1: Initialize job will be started, selection is set after it has finished. */
    /* Case 2: Initialize job is already running, old selection is discarded and new one stored. */
    m_initializeSource = source;

    /* In this case, the chart is initializing. */
    if( m_initializeJob != null )
      return;

    /* In this case, the chart must be initialized. */
    m_initializeJob = new TimeseriesChartJob( m_template, m_model );
    m_initializeJob.setUser( true );
    m_initializeJob.addJobChangeListener( new JobChangeAdapter()
    {
      @Override
      public void done( final IJobChangeEvent event )
      {
        initialized( event );
      }
    } );

    /* Schedule the initialize job. */
    m_initializeJob.schedule();
  }

  protected synchronized void initialized( final IJobChangeEvent event )
  {
    final Job job = event.getJob();
    if( job != m_initializeJob )
      return;

    final IStatus result = job.getResult();
    if( !result.isOK() )
    {
      System.out.println( "Could not initialize the chart: " + result.getMessage() ); //$NON-NLS-1$
      return;
    }

    setSelectionInternal( m_initializeSource );

    m_initializeJob = null;
    m_initialized = true;
    m_initializeSource = null;
  }

  private void setSelectionInternal( final IMultipleZmlSourceElement source )
  {
    DiagramCompositeSelection.doApply( m_model, source );

    /* initially update noData layer once */
    m_layerManagerListener.reschedule();
  }

  private void draw( )
  {
    createToolbar();

    m_chartComposite = new ChartImageComposite( this, SWT.BORDER, m_model, CHART_BACKGROUND );
    m_chartComposite.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    m_chartSourceProvider = new ChartSourceProvider( m_context, m_chartComposite );

    final ZoomPanMaximizeHandler handler = new ZoomPanMaximizeHandler( m_chartComposite, DIRECTION.eBoth );
    final IEvaluationService service = (IEvaluationService) m_context.getService( IEvaluationService.class );
    handler.addListener( new ZmlChartSelectionChangedHandler( service.getCurrentState() ) );

    m_chartComposite.getPlotHandler().activatePlotHandler( handler );
  }

  private void createToolbar( )
  {
    final ToolBarManager manager = new ToolBarManager( SWT.HORIZONTAL | SWT.FLAT );
    final ToolBar control = manager.createControl( this );
    control.setLayoutData( new GridData( SWT.RIGHT, GridData.FILL, true, false ) );

    ContributionUtils.populateContributionManager( PlatformUI.getWorkbench(), manager, "toolbar:org.kalypso.model.rrm.ui.zml.chart.menu.toolbar" ); //$NON-NLS-1$

    if( KalypsoZmlUiDebug.DEBUG_DIAGRAM.isEnabled() )
    {
      ContributionUtils.populateContributionManager( PlatformUI.getWorkbench(), manager, "toolbar:org.kalypso.model.rrm.ui.chart.debug" ); //$NON-NLS-1$
    }

    manager.update( true );
    m_toolkit.adapt( control );
  }
}