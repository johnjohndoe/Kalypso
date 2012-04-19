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
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
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
import org.joda.time.Period;
import org.kalypso.commons.databinding.jface.wizard.DatabindingWizardPage;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.contribs.eclipse.swt.layout.Layouts;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.model.hydrology.timeseries.Timeserieses;
import org.kalypso.model.hydrology.timeseries.binding.IStation;
import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ogc.sensor.DateRange;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ogc.sensor.metadata.ParameterTypeLabelProvider;
import org.kalypso.ui.rrm.internal.timeseries.view.TimeseriesBean;
import org.kalypso.ui.rrm.internal.timeseries.view.evaporation.CalculateEvaporationData.EVAPORATION_TYPE;
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

    setTitle( "Berechnungsdaten" );
    setDescription( "Bitte bestimmen Sie alle Berechnungsdaten, die als Grundlage für die Berechnung der Verdunstung dienen." );
  }

  @Override
  public void createControl( final Composite parent )
  {
    initializeDialogUnits( parent );
    m_binding = new DatabindingWizardPage( this, null );

    final Composite body = new Composite( parent, SWT.NULL );
    body.setLayout( Layouts.createGridLayout() );
    setControl( body );

    doAddEvaporaqtionTypeControl( body );
    doAddSourceControls( body );
    doAddDateRangeControl( body );

    doUpdateDateRange();
  }

  private void doAddEvaporaqtionTypeControl( final Composite body )
  {
    final Group group = new Group( body, SWT.NULL );
    group.setLayout( new GridLayout() );
    group.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    group.setText( "Verdunstung" );

    final ComboViewer viewer = new ComboViewer( group, SWT.BORDER | SWT.READ_ONLY );
    viewer.getCombo().setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    viewer.setLabelProvider( new LabelProvider() );
    viewer.setContentProvider( new ArrayContentProvider() );

    final IViewerObservableValue target = ViewersObservables.observeSinglePostSelection( viewer );
    final IObservableValue model = BeansObservables.observeValue( m_data, CalculateEvaporationData.PROPERTY_EVAPORATION_TYPE );
    m_binding.bindValue( target, model, new IValidator()
    {

      @Override
      public IStatus validate( final Object value )
      {
        if( Objects.isNull( value ) )
        {
          setErrorMessage( "Leeres Element. Bitte weisen Sie alle Quellen zu." );
          return Status.CANCEL_STATUS;
        }

        return Status.OK_STATUS;
      }
    } );

    final EVAPORATION_TYPE[] input = new EVAPORATION_TYPE[] { EVAPORATION_TYPE.eLandBased, EVAPORATION_TYPE.eWaterBase };
    viewer.setInput( input );

    viewer.setSelection( new StructuredSelection( input[0] ) );
  }

  private void doAddDateRangeControl( final Composite body )
  {
    final Group groupDateRange = new Group( body, SWT.NULL );
    groupDateRange.setLayout( new GridLayout( 5, false ) );
    groupDateRange.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    groupDateRange.setText( "Berechnungszeitraum" );

    final Label start = new Label( groupDateRange, SWT.NULL );
    start.setText( "Start" );

    m_labelDateRangeFrom = new Text( groupDateRange, SWT.BORDER | SWT.READ_ONLY | SWT.RIGHT );
    m_labelDateRangeFrom.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    m_labelDateRangeFrom.setText( "" );

    final Label spacer = new Label( groupDateRange, SWT.NULL );
    spacer.setText( "" );
    spacer.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final Label stop = new Label( groupDateRange, SWT.NULL );
    stop.setText( "Ende" );
    m_labelDateRangeTo = new Text( groupDateRange, SWT.BORDER | SWT.READ_ONLY | SWT.RIGHT );
    m_labelDateRangeTo.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    m_labelDateRangeTo.setText( "" );

    final String tooltip = "Berechnungszeitraum wurde automatisch auf Grundlage der gewählten Zeitreihen ermittelt.\nDer ermittelte Berechnungszeitraum ist der gemeinsame Zeitraum aller gewählten Zeitreihen (Schnittmenge).";
    groupDateRange.setToolTipText( tooltip );
    start.setToolTipText( tooltip );
    m_labelDateRangeFrom.setToolTipText( tooltip );
    stop.setToolTipText( tooltip );
    m_labelDateRangeTo.setToolTipText( tooltip );
  }

  private void doAddSourceControls( final Composite body )
  {
    final Group groupSources = new Group( body, SWT.NULL );
    groupSources.setLayout( new GridLayout( 3, false ) );
    groupSources.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, true ) );
    groupSources.setText( "Quellen" );

    addControl( groupSources, ITimeseriesConstants.TYPE_MEAN_HUMIDITY, CalculateEvaporationData.PROPERTY_HUMIDITY );
    addControl( groupSources, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE, CalculateEvaporationData.PROPERTY_TEMPERATURE );
    addControl( groupSources, ITimeseriesConstants.TYPE_MEAN_WIND_VELOCITY, CalculateEvaporationData.PROPERTY_WIND_VELOCITY );
    addControl( groupSources, ITimeseriesConstants.TYPE_SUNSHINE_HOURS, CalculateEvaporationData.PROPERTY_SUNSHINE_HOURS );
  }

  private void addControl( final Group group, final String type, final String property )
  {
    final ParameterTypeLabelProvider provider = new ParameterTypeLabelProvider();

    final Label label = new Label( group, SWT.NULL );
    label.setText( provider.getText( type ) );

    final Label spacer = new Label( group, SWT.NULL );
    spacer.setText( "" );
    final GridData data = new GridData( GridData.FILL, GridData.FILL, false, false );
    data.widthHint = data.minimumWidth = 100;
    spacer.setLayoutData( data );

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
    m_binding.bindValue( target, model, new IValidator()
    {

      @Override
      public IStatus validate( final Object value )
      {
        if( Objects.isNull( value ) )
        {
          setErrorMessage( "Leeres Element. Bitte weisen Sie alle Quellen zu." );
          return Status.CANCEL_STATUS;
        }

        return Status.OK_STATUS;
      }
    } );

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
      {
        // only tageswerte are supported
        final Period timestep = timeseries.getTimestep();
        final int days = timestep.toStandardDays().getDays();
        if( days == 1 )
          found.add( new TimeseriesBean( timeseries ) );
      }

    }

    return found.toArray( new TimeseriesBean[] {} );
  }
}
