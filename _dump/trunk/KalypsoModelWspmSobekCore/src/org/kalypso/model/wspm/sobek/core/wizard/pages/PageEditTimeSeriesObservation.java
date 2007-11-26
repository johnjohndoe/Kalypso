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
package org.kalypso.model.wspm.sobek.core.wizard.pages;

import java.awt.Insets;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.Map.Entry;

import javax.xml.namespace.QName;

import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.FontData;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.kalypso.chart.ext.base.axis.CalendarAxis;
import org.kalypso.chart.ext.base.axis.NumberAxis;
import org.kalypso.chart.ext.base.axisrenderer.CalendarAxisRenderer;
import org.kalypso.chart.ext.base.axisrenderer.NumberAxisRenderer;
import org.kalypso.chart.ext.base.style.StyledLine;
import org.kalypso.chart.ext.observation.data.TupleResultDomainValueData;
import org.kalypso.chart.ext.observation.layer.TupleResultLineLayer;
import org.kalypso.chart.framework.model.impl.ChartModel;
import org.kalypso.chart.framework.model.layer.IChartLayer;
import org.kalypso.chart.framework.model.layer.ILayerManager;
import org.kalypso.chart.framework.model.mapper.IAxis;
import org.kalypso.chart.framework.model.mapper.IAxisConstants.DIRECTION;
import org.kalypso.chart.framework.model.mapper.IAxisConstants.POSITION;
import org.kalypso.chart.framework.model.mapper.IAxisConstants.PROPERTY;
import org.kalypso.chart.framework.model.mapper.registry.IMapperRegistry;
import org.kalypso.chart.framework.model.styles.IStyledElement;
import org.kalypso.chart.framework.model.styles.impl.LayerStyle;
import org.kalypso.chart.framework.view.ChartComposite;
import org.kalypso.contribs.java.util.DoubleComparator;
import org.kalypso.model.wspm.sobek.core.interfaces.IBoundaryNodeLastfallCondition;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.featureview.control.TupleResultFeatureControl;

/**
 * Wizard page with table of time series observation
 * 
 * @author Dirk Kuch
 */
public class PageEditTimeSeriesObservation extends WizardPage
{

  public static final String OBS_DATE = "urn:ogc:gml:dict:kalypso:wspm:sobek:boundaryConditionObservationDefs#DATE";

  QName DATE_AXIS = new QName( "http://www.w3.org/2001/XMLSchema", "dateTime" );

  private final IBoundaryNodeLastfallCondition m_condition;

  public PageEditTimeSeriesObservation( final IBoundaryNodeLastfallCondition condition )
  {
    super( "editTimeSeriesObservation" );
    m_condition = condition;
    setTitle( "Edit Time Series" );
    setDescription( "Edit Time Series" );
  }

  /**
   * @see org.eclipse.jface.dialogs.IDialogPage#createControl(org.eclipse.swt.widgets.Composite)
   */
  public void createControl( final Composite parent )
  {
    setPageComplete( false );

    final Object layoutData = parent.getLayoutData();
    if( layoutData instanceof GridData )
    {
      final GridData pLayout = (GridData) layoutData;
      pLayout.widthHint = 900;
      pLayout.heightHint = 400;
      parent.layout();
    }

    final Composite container = new Composite( parent, SWT.NULL );
    container.setLayout( new GridLayout( 2, false ) );
    setControl( container );

    final IObservation<TupleResult> myObs = m_condition.getTimeSeriesObservation();
    final TupleResult tupleResult = myObs.getResult();

    // obsTable
    final TupleResultFeatureControl control = new TupleResultFeatureControl( m_condition.getTimeSeriesObservationFeature(), null );
// control.setComponentUiHandlers( new IComponentUiHandler[] { dateHandler } );

    // control.setViewerFilter( new CDViewerFilter( combinations ) );

    final Control tblControl = control.createControl( container, SWT.BORDER );
    tblControl.setLayoutData( new GridData( GridData.FILL, GridData.FILL, false, true ) );

// control.addChangeListener( new IFeatureChangeListener()
// {
// public void featureChanged( final FeatureChange[] changes )
// {
// final UIJob job = new UIJob( "modelUpdate" )
// {
// @Override
// public IStatus runInUIThread( IProgressMonitor monitor )
// {
// GMLWorkspace workspace = m_conflict.getWorkspace();
// final ChangeFeaturesCommand command = new ChangeFeaturesCommand( workspace, changes );
// m_pool.postCommand( command, null );
//
// return Status.OK_STATUS;
// }
// };
// job.schedule();
// }
//
// public void openFeatureRequested( final Feature feature, final IPropertyType pt )
// {
// }
// } );

    /* diagram */
    final Composite cDiagram = new Composite( container, SWT.NULL );
    cDiagram.setLayout( new GridLayout() );
    cDiagram.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );

    final ChartModel model = new ChartModel();
    final IMapperRegistry mapperRegistry = model.getMapperRegistry();
    final ILayerManager layerManager = model.getLayerManager();

    IAxis calendar = null;
    IComponent calendarComp = null;

    final Map<IComponent, IAxis> axes = new HashMap<IComponent, IAxis>();

    final IComponent[] components = tupleResult.getComponents();
    for( final IComponent component : components )
    {
      final QName qname = component.getValueTypeName();

      if( DATE_AXIS.equals( qname ) )
      {
        if( calendar != null )
          throw (new IllegalStateException( "two date axes defined!" ));

        calendar = new CalendarAxis( qname.getLocalPart(), "Date", PROPERTY.CONTINUOUS, POSITION.BOTTOM, DIRECTION.POSITIVE, "yy-MM-dd\nhh:mm" );
        calendarComp = component;
        mapperRegistry.addMapper( calendar );
      }
      else
      {
        final NumberAxis axis = new NumberAxis( component.getId(), component.getName(), PROPERTY.CONTINUOUS, POSITION.LEFT, DIRECTION.POSITIVE, new DoubleComparator( 0.0001 ) );
        axes.put( component, axis );
        mapperRegistry.addMapper( axis );
      }
    }

    final Set<Entry<IComponent, IAxis>> setAxes = axes.entrySet();

    final RGB rgbFG = new RGB( 0, 0, 255 );
    final RGB rgbBG = new RGB( 255, 255, 255 );
    final FontData fontData = new FontData( "Arial", 8, SWT.NONE );
    final Insets inset = new Insets( 1, 1, 1, 1 );

    final CalendarAxisRenderer calRenderer = new CalendarAxisRenderer( calendar.getIdentifier(), rgbFG, rgbBG, 1, 5, inset, inset, 0, fontData, fontData );

    mapperRegistry.setRenderer( calendar.getIdentifier(), calRenderer );

    for( final Entry<IComponent, IAxis> entry : setAxes )
    {
      final TupleResultDomainValueData data = new TupleResultDomainValueData( tupleResult, calendarComp.getId(), entry.getKey().getId() );

      final IAxis axis = entry.getValue();

      final IChartLayer< ? , ? > layer = new TupleResultLineLayer( data, calendar, axis );
      final LayerStyle layerStyle = new LayerStyle();
      final IStyledElement line = new StyledLine( "line", 2, new RGB( 0, 0, 255 ), SWT.LINE_SOLID, 255 );
      layerStyle.add( line );
      layer.setStyle( layerStyle );

      layerManager.addLayer( layer );

      final NumberAxisRenderer axisRenderer = new NumberAxisRenderer( axis.getIdentifier(), rgbFG, rgbBG, 1, 5, inset, inset, 0, fontData, fontData, 0, 0, false, "%s" );
      mapperRegistry.setRenderer( axis.getIdentifier(), axisRenderer );

      model.autoscale( new IAxis[] { calendar, axis } );

    }

    final ChartComposite cc = new ChartComposite( cDiagram, SWT.BORDER, model, new RGB( 255, 255, 255 ) );
  }
}
