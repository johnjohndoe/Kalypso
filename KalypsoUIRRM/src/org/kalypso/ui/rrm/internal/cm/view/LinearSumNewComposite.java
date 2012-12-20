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

import java.util.LinkedHashMap;

import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.swt.SWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
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
public class LinearSumNewComposite extends FeatureBeanComposite<ILinearSumGenerator>
{
  /**
   * All allowed parameter types.
   */
  private static final String[] ALLOWED_PARAMETER_TYPES = new String[] { ITimeseriesConstants.TYPE_RAINFALL, ITimeseriesConstants.TYPE_MEAN_TEMPERATURE,
      ITimeseriesConstants.TYPE_EVAPORATION_LAND_BASED };

  /**
   * The text field of the comments.
   */
  private Text m_commentText;

  /**
   * The constructor.
   *
   * @param parent
   *          The parent composite.
   * @param featureBean
   *          The feature bean.
   * @param binding
   *          The data binding.
   * @param commentEditable
   *          True, if the comment should be editable. False otherwise.
   */
  public LinearSumNewComposite( final Composite parent, final FeatureBean<ILinearSumGenerator> featureBean, final IDataBinding binding, final boolean commentEditable )
  {
    super( parent, featureBean, binding, true );

    /* The function createContents() was called by the super constructor. */
    /* So all GUI was already created, if the execution reaches here. */
    /* Override the general editable state. */
    m_commentText.setEditable( commentEditable );
    m_commentText.setEnabled( commentEditable );
  }

  /**
   * @see org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite#createContents()
   */
  @Override
  protected void createContents( )
  {
    /* Create the contents. */
    createPropertyTextFieldControl( ILinearSumGenerator.QN_DESCRIPTION );
    m_commentText = createPropertyTextFieldControl( ILinearSumGenerator.PROPERTY_COMMENT );
    createPropertyDateTimeControl( ILinearSumGenerator.PROPERTY_VALID_FROM );
    createPropertyDateTimeControl( ILinearSumGenerator.PROPERTY_VALID_TO );
    createPropertyTimestepControl();
    createPropertyTimestampControl();
    createParameterTypeControl();
  }

  /**
   * This function creates the timestep control.
   */
  private void createPropertyTimestepControl( )
  {
    /* Create the property label. */
    createPropertyLabel( this, ILinearSumGenerator.PROPERTY_TIMESTEP );

    /* Create the property text field. */
    final Text field = createPropertyTextField( this );

    /* Bind the text field. */
    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = new FeatureBeanObservableValue( getBean(), ILinearSumGenerator.PROPERTY_TIMESTEP );

    /* Create the data binder. */
    final DataBinder binder = new DataBinder( target, model );
    binder.addTargetAfterConvertValidator( new TimestepValidator( getBean() ) );

    /* Get the data binding. */
    final IDataBinding binding = getBinding();

    /* Bind the value. */
    binding.bindValue( binder );
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
    binder.addTargetBeforeSetValidator( new TimestampValidator( getBean() ) );

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

    /* Get the parameter labels. */
    final LinkedHashMap<String, String> allowedParameterLabels = new LinkedHashMap<>();
    for( final String allowedParameterType : ALLOWED_PARAMETER_TYPES )
    {
      final String allowedParameterLabel = ParameterTypeUtils.formatParameterType( allowedParameterType );
      allowedParameterLabels.put( allowedParameterType, allowedParameterLabel );
    }

    /* Create the property combo viewer. */
    final ComboViewer comboViewer = createPropertyCombo( this, new LabelProvider()
    {
      /**
       * @see org.eclipse.jface.viewers.LabelProvider#getText(java.lang.Object)
       */
      @Override
      public String getText( final Object element )
      {
        if( element instanceof String )
          return allowedParameterLabels.get( element );

        return super.getText( element );
      }
    }, false );

    /* Set the input. */
    comboViewer.setInput( allowedParameterLabels.keySet().toArray() );

    /* Bind the combo viewer. */
    bindCombo( comboViewer, ILinearSumGenerator.PROPERTY_PARAMETER_TYPE );
  }
}