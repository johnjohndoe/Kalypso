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

import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.services.IServiceLocator;
import org.kalypso.chart.ui.editor.commandhandler.ChartSourceProvider;
import org.kalypso.chart.ui.editor.mousehandler.ZoomPanMaximizeHandler;
import org.kalypso.chart.ui.editor.mousehandler.ZoomPanMaximizeHandler.DIRECTION;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.jface.action.ContributionUtils;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.zml.core.base.IMultipleZmlSourceElement;
import org.kalypso.zml.core.diagram.base.ChartTypeHandler;
import org.kalypso.zml.ui.chart.layer.visitor.SingleGridVisibilityVisitor;
import org.kalypso.zml.ui.chart.view.DiagramCompositeSelection;
import org.kalypso.zml.ui.chart.view.HideUnuseLayersVisitor;
import org.kalypso.zml.ui.debug.KalypsoZmlUiDebug;

import de.openali.odysseus.chart.factory.config.ChartExtensionLoader;
import de.openali.odysseus.chart.factory.config.ChartFactory;
import de.openali.odysseus.chart.framework.model.impl.ChartModel;
import de.openali.odysseus.chart.framework.model.layer.ILayerManager;
import de.openali.odysseus.chart.framework.view.IChartHandlerManager;
import de.openali.odysseus.chart.framework.view.impl.ChartImageComposite;

/**
 * @author Dirk Kuch
 */
public class EditTimeseriesChartComposite extends Composite
{
  private static final RGB CHART_BACKGROUND = new RGB( 255, 255, 255 );

  protected ChartModel m_model = new ChartModel();

  private ChartImageComposite m_chartComposite;

  private final IServiceLocator m_context;

  private ChartSourceProvider m_chartSourceProvider;

  EditTimeseriesChartComposite( final Composite parent, final FormToolkit toolkit, final IServiceLocator context )
  {
    super( parent, SWT.NULL );

    m_context = context;

    setLayout( Layouts.createGridLayout() );

    init();
    draw( toolkit );

    toolkit.adapt( this );
  }

  private void init( )
  {
    try
    {
      final ChartTypeHandler handler = new ChartTypeHandler( getClass().getResource( "templates/diagram.kod" ) ); //$NON-NLS-1$
      ChartFactory.doConfiguration( m_model, handler.getReferenceResolver(), handler.getChartType(), ChartExtensionLoader.getInstance(), handler.getContext() );

    }
    catch( final Throwable t )
    {
      t.printStackTrace();
    }
  }

  public void deactivate( )
  {
    m_chartSourceProvider.dispose();
  }

  private void draw( final FormToolkit toolkit )
  {
    createToolbar( toolkit );

    m_chartComposite = new ChartImageComposite( this, SWT.BORDER, m_model, CHART_BACKGROUND );
    m_chartComposite.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    m_chartSourceProvider = new ChartSourceProvider( m_context, m_chartComposite );

    final ZoomPanMaximizeHandler handler = new ZoomPanMaximizeHandler( m_chartComposite, DIRECTION.eBoth );
    final IChartHandlerManager plot = m_chartComposite.getPlotHandler();
    if( Objects.isNotNull( plot ) )
      plot.activatePlotHandler( handler );
  }

  private void createToolbar( final FormToolkit toolkit )
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
    toolkit.adapt( control );
  }

  public void setSelection( final IMultipleZmlSourceElement source )
  {
    DiagramCompositeSelection.doApply( m_model, source );

    final ILayerManager layerManager = m_model.getLayerManager();
    layerManager.accept( new HideUnuseLayersVisitor() );
    layerManager.accept( new SingleGridVisibilityVisitor() );

    m_model.autoscale();

  }

}
