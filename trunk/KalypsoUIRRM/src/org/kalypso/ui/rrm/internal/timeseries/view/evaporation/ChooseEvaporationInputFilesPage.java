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
package org.kalypso.ui.rrm.internal.timeseries.view.evaporation;

import java.text.SimpleDateFormat;
import java.util.LinkedHashSet;
import java.util.Set;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.ui.rrm.internal.timeseries.view.actions.CalculateEvaporationData;
import org.kalypso.zml.ui.imports.ParameterTypeLabelProvider;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;

/**
 * @author Dirk Kuch
 */
public class ChooseEvaporationInputFilesPage extends WizardPage
{

  private final CalculateEvaporationData m_data;

  private DatabindingWizardPage m_binding;

  private final IStation m_station;

  private Text m_labelDateRangeFrom;

  private Text m_labelDateRangeTo;

  protected ChooseEvaporationInputFilesPage( final IStation station, final CalculateEvaporationData data )
  {
    super( "ChooseEvaporationInputFilesPage" );
    m_station = station;
    m_data = data;

    setTitle( "Eingangsdaten" );
    setMessage( "Bitte wählen Sie die Eingangsdaten, die als Grundlage für die Berechnung der Verdunstung dienen." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );
    m_binding = new DatabindingWizardPage( this, null );

    final Composite body = new Composite( parent, SWT.NULL );
    body.setLayout( Layouts.createGridLayout() );
    setControl( body );

    final Group groupSources = new Group( body, SWT.NULL );
    groupSources.setLayout( new GridLayout( 2, false ) );
    groupSources.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    groupSources.setText( "Quellen" );

    addControl( groupSources, ITimeseriesConstants.TYPE_MEAN_HUMIDITY, CalculateEvaporationData.PROPERTY_HUMIDITY );
    addControl( groupSources, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE, CalculateEvaporationData.PROPERTY_TEMPERATURE );
    addControl( groupSources, ITimeseriesConstants.TYPE_MEAN_WIND_VELOCITY, CalculateEvaporationData.PROPERTY_WIND_VELOCITY );
    addControl( groupSources, ITimeseriesConstants.TYPE_MEAN_SUNSHINE_HOURS, CalculateEvaporationData.PROPERTY_SUNSHINE_HOURS );

    final Group groupDateRange = new Group( body, SWT.NULL );
    groupDateRange.setLayout( new GridLayout( 2, false ) );
    groupDateRange.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    groupDateRange.setText( "Berechnungszeitraum" );

    new Label( groupDateRange, SWT.NULL ).setText( "Start:" );
    m_labelDateRangeFrom = new Text( groupDateRange, SWT.BORDER | SWT.READ_ONLY | SWT.RIGHT );
    m_labelDateRangeFrom.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    m_labelDateRangeFrom.setText( "" );

    new Label( groupDateRange, SWT.NULL ).setText( "Ende:" );
    m_labelDateRangeTo = new Text( groupDateRange, SWT.BORDER | SWT.READ_ONLY | SWT.RIGHT );
    m_labelDateRangeTo.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    m_labelDateRangeTo.setText( "" );

    doUpdateDateRange();
  }

  private void addControl( final Group group, final String type, final String property )
  {
    final ParameterTypeLabelProvider provider = new ParameterTypeLabelProvider();

    final Label label = new Label( group, SWT.NULL );
    label.setText( provider.getText( type ) );

    final ComboViewer viewer = new ComboViewer( group, SWT.BORDER | SWT.READ_ONLY );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setLabelProvider( new LabelProvider()
    {
      @Override
      public String getText( final Object element )
      {
        if( element instanceof TimeseriesBean )
        {
          return Timeserieses.getTreeLabel( ((TimeseriesBean) element).getFeature() );
        }

        return super.getText( element );
      }
    } );
    viewer.setContentProvider( new ArrayContentProvider() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, property );
    m_binding.bindValue( target, model );

    final TimeseriesBean[] input = findInputFiles( m_station.getTimeseries(), type );
    viewer.setInput( input );

    if( ArrayUtils.isNotEmpty( input ) )
      viewer.setSelection( new StructuredSelection( input[0] ) );

    viewer.addSelectionChangedListener( new ISelectionChangedListener()
    {
      @Override
      public void selectionChanged( final SelectionChangedEvent event )
      {
        doUpdateDateRange();
      }

    } );
  }

  protected void doUpdateDateRange( )
  {
    final DateRange daterange = m_data.getDateRange();
    if( Objects.isNull( daterange ) )
      m_labelDateRangeFrom.setText( "Konnte Berechnungszeitraum nicht bestimmen. Fehlende Eingabedatei?" );
    else
    {
      final SimpleDateFormat sdf = new SimpleDateFormat( "dd.MM.yyyy HH:mm" );
      sdf.setTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );

      m_labelDateRangeFrom.setText( sdf.format( daterange.getFrom() ) );
      m_labelDateRangeTo.setText( sdf.format( daterange.getTo() ) );
    }

    m_labelDateRangeFrom.getParent().layout();
  }

  private TimeseriesBean[] findInputFiles( final IFeatureBindingCollection<ITimeseries> collection, final String type )
  {
    final Set<TimeseriesBean> found = new LinkedHashSet<>();

    for( final ITimeseries timeseries : collection )
    {
      if( StringUtils.equals( timeseries.getParameterType(), type ) )
        found.add( new TimeseriesBean( timeseries ) );
    }

    return found.toArray( new TimeseriesBean[] {} );
  }
}
