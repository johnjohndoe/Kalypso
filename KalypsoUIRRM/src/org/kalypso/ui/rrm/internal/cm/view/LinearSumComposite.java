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
package org.kalypso.ui.rrm.internal.cm.view;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanObservableValue;
import org.kalypso.ui.rrm.internal.utils.featureBinding.TimestampModelToTargetConverter;
import org.kalypso.ui.rrm.internal.utils.featureBinding.TimestampTargetToModelConverter;

/**
 * This composite shows the contents of the linear sum generator.
 * 
 * @author Gernot Belger
 * @author Holger Albert
 */
public class LinearSumComposite extends FeatureBeanComposite<ILinearSumGenerator>
{
  /**
   * The constructor.
   * 
   * @param parent
   *          The parent composite.
   * @param featureBean
   *          The feature bean.
   * @param binding
   *          The data binding.
   * @param generalEditable
   *          True, if the contents of the composite should be generally editable. False otherwise.
   */
  public LinearSumComposite( final Composite parent, final FeatureBean<ILinearSumGenerator> featureBean, final IDataBinding binding, final boolean generalEditable )
  {
    super( parent, featureBean, binding, generalEditable );
  }

  @Override
  protected void createContents( )
  {
    /* Create the contents. */
    createPropertyTextFieldControl( ILinearSumGenerator.QN_DESCRIPTION );
    createPropertyTextFieldControl( ILinearSumGenerator.PROPERTY_COMMENT );
    createPropertyDateTimeControl( ILinearSumGenerator.PROPERTY_VALID_FROM );
    createPropertyDateTimeControl( ILinearSumGenerator.PROPERTY_VALID_TO );
    createPropertyTextFieldControl( ILinearSumGenerator.PROPERTY_TIMESTEP );
    createPropertyTimestampControl();
    createParameterTypeControl();
  }

  /**
   * This function creates the timestamp control.
   */
  private void createPropertyTimestampControl( )
  {
    /* Create the property label. */
    createPropertyLabel( this, ILinearSumGenerator.PROPERTY_TIMESTAMP );

    /* Create the property text field. */
    final Text field = createPropertyTextField( this );

    /* Bind the text field. */
    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = new FeatureBeanObservableValue( getBean(), ILinearSumGenerator.PROPERTY_TIMESTAMP );

    /* Create the data binder. */
    final DataBinder binder = new DataBinder( target, model );
    binder.setModelToTargetConverter( new TimestampModelToTargetConverter() );
    binder.setTargetToModelConverter( new TimestampTargetToModelConverter() );

    /* Get the data binding. */
    final IDataBinding binding = getBinding();

    /* Bind the value. */
    binding.bindValue( binder );
  }

  /**
   * This function creates the parameter type control.
   */
  private void createParameterTypeControl( )
  {
    /* Create the property label. */
    createPropertyLabel( this, ILinearSumGenerator.PROPERTY_PARAMETER_TYPE );

    /* Get the parameter label. */
    final String parameterType = (String) getBean().getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE );
    final String parameterLabel = ParameterTypeUtils.formatParameterType( parameterType );

    /* Create the property text field. */
    final Text field = createPropertyTextField( this );
    field.setText( parameterLabel );
  }
}