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
package org.kalypso.ui.rrm.internal.timeseries.view;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.beans.BeansObservables;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.DateTime;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.core.status.StatusComposite;
import org.kalypso.core.status.StatusCompositeValue;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;

/**
 * @author Gernot Belger
 */
public class TimeseriesComposite extends FeatureBeanComposite<ITimeseries>
{
  public TimeseriesComposite( final Composite parent, final FeatureBean<ITimeseries> featureBean, final IDataBinding binding, final boolean editable )
  {
    super( parent, featureBean, binding, editable );
  }

  @Override
  protected void createContents( )
  {
    createParameterTypeControl();
    createPropertyTextFieldControl( ITimeseries.PROPERTY_QUALITY );
    createPropertyTextFieldControl( ITimeseries.QN_DESCRIPTION );
    createTimestepControl();
    // TODO: show timestamp
    createDateRangeControl();
    createTimeseriesDataValidationControl();
  }

  private void createParameterTypeControl( )
  {
    createPropertyLabel( this, ITimeseries.PROPERTY_PARAMETER_TYPE );

    final Text field = createPropertyTextField( this );

    final String parameterType = (String) getBean().getProperty( ITimeseries.PROPERTY_PARAMETER_TYPE );
    final String parameterLabel = ParameterTypeUtils.formatParameterType( parameterType );

    field.setText( parameterLabel );
  }

  private void createTimestepControl( )
  {
    final FormToolkit toolkit = getToolkit();

    toolkit.createLabel( this, Messages.getString( "TimeseriesComposite_0" ) ); //$NON-NLS-1$

    final Text field = toolkit.createText( this, StringUtils.EMPTY, SWT.BORDER | SWT.SINGLE );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setEnabled( false );

    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = BeansObservables.observeValue( getBean(), TimeseriesBean.PROPERTY_PERIOD_TEXT );

    final DataBinder binder = new DataBinder( target, model );

    getBinding().bindValue( binder );
  }

  // TODO: should we give the ITimeseries a timestamp field?
// /**
// * This function creates the timestamp control.
// */
// private void createPropertyTimestampControl( )
// {
// /* Create the property label. */
// createPropertyLabel( this, ITimeseries.PROPERTY_TIMESTAMP );
//
// /* Create the property text field. */
// final Text field = createPropertyTextField( this );
//
// /* Bind the text field. */
// final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
// final IObservableValue model = new FeatureBeanObservableValue( getBean(), ITimeseries.PROPERTY_TIMESTAMP );
//
// /* Create the data binder. */
// final DataBinder binder = new DataBinder( target, model );
// binder.setModelToTargetConverter( new TimestampModelToTargetConverter() );
// binder.setTargetToModelConverter( new TimestampTargetToModelConverter() );
//
// /* Get the data binding. */
// final IDataBinding binding = getBinding();
//
// /* Bind the value. */
// binding.bindValue( binder );
// }

  private void createDateRangeControl( )
  {

    final Label header = new Label( this, SWT.NULL );
    header.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );
    header.setText( Messages.getString( "TimeseriesComposite.0" ) ); //$NON-NLS-1$

    final Composite body = getToolkit().createComposite( this );
    GridLayoutFactory.swtDefaults().numColumns( 2 ).applyTo( body );
    body.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false ) );

    final DateTime start = createPropertyDateTime( body );
    bindDateTime( start, ITimeseries.PROPERTY_MEASUREMENT_START );

    final DateTime end = createPropertyDateTime( body );
    bindDateTime( end, ITimeseries.PROPERTY_MEASUREMENT_END );
  }

  private void createTimeseriesDataValidationControl( )
  {
    final FormToolkit toolkit = getToolkit();

    toolkit.createLabel( this, Messages.getString( "TimeseriesComposite_1" ) ); //$NON-NLS-1$

    final StatusComposite statusComposite = new StatusComposite( toolkit, this, SWT.NONE );
    statusComposite.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    final IObservableValue target = new StatusCompositeValue( statusComposite );
    final IObservableValue model = BeansObservables.observeValue( getBean(), TimeseriesBean.PROPERTY_DATA_STATUS );

    final DataBinder binder = new DataBinder( target, model );

    getBinding().bindValue( binder );
  }
}