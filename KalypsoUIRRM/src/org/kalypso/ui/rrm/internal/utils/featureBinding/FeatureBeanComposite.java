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
package org.kalypso.ui.rrm.internal.utils.featureBinding;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.ArrayUtils;
import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.databinding.viewers.IViewerObservableValue;
import org.eclipse.jface.databinding.viewers.ViewersObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Layout;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.commons.databinding.jface.DateControlModelToTargetConverter;
import org.kalypso.commons.databinding.jface.DateControlTargetToModelConverter;
import org.kalypso.commons.databinding.jface.DateTimeControl;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.core.KalypsoCorePlugin;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public abstract class FeatureBeanComposite<F extends Feature> extends Composite
{
  /**
   * The feature bean.
   */
  private final FeatureBean<F> m_featureBean;

  /**
   * The data binding.
   */
  private final IDataBinding m_binding;

  /**
   * True, if the contents of the composite should be generally editable. False otherwise.
   */
  private final boolean m_generalEditable;

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
  public FeatureBeanComposite( final Composite parent, final FeatureBean<F> featureBean, final IDataBinding binding, final boolean generalEditable )
  {
    super( parent, SWT.NONE );

    m_featureBean = featureBean;
    m_binding = binding;
    m_generalEditable = generalEditable;

    final FormToolkit toolkit = m_binding.getToolkit();
    if( toolkit != null )
      toolkit.adapt( this );

    super.setLayout( GridLayoutFactory.fillDefaults().create() );

    createContents();
  }

  @Override
  public final void setLayout( final Layout layout )
  {
  }

  protected final boolean isEditable( )
  {
    return m_generalEditable;
  }

  protected final FeatureBean<F> getBean( )
  {
    return m_featureBean;
  }

  protected final IDataBinding getBinding( )
  {
    return m_binding;
  }

  protected final FormToolkit getToolkit( )
  {
    return m_binding.getToolkit();
  }

  protected abstract void createContents( );

  /**
   * This function creates a control, which allows the editing of the property specified by the qname.
   * 
   * @param property
   *          The qname of the property.
   * @param validators
   *          validation of text box entries
   * @return The text field, which is contained in the property control. Its editable state will be that of the
   *         generalEditable flag, provided int the constructor.
   */
  protected final Text createPropertyControl( final QName property, final IValidator... validators )
  {
    createPropertyLabel( this, property );

    final Text field = createPropertyTextField( this );
    bindTextField( field, property, validators );

    return field;
  }

  protected final void createDateControl( final QName property, final IValidator... validators )
  {
    createPropertyLabel( this, property );

    final DateTimeControl control = new DateTimeControl( this, m_generalEditable );
    control.setDisplayTimeZone( KalypsoCorePlugin.getDefault().getTimeZone() );
    control.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );

    bindDateControl( control, property );
  }

  /**
   * This function creates a control, which allows the editing of the property specified by the qname.
   * 
   * @param property
   *          The qname of the property.
   * @param validators
   *          validation of text box entries
   * @return The text field, which is contained in the property control. Its editable state will be that of the
   *         generalEditable flag, provided int the constructor.
   */
  protected final ComboViewer createPropertyComboControl( final QName property, final String[] choices, final IValidator... validators )
  {
    createPropertyLabel( this, property );

    final ComboViewer viewer = createPropertyCombo( this, new LabelProvider(), true );
    bindCombo( viewer, property );

    viewer.setInput( choices );

    final Object value = m_featureBean.getProperty( property );
    if( Objects.isNotNull( value ) )
      viewer.setSelection( new StructuredSelection( value ) );

    return viewer;
  }

  /**
   * Refresh the control from the state of the underlying feature.
   */
  public final void refresh( )
  {
    m_featureBean.revert();
  }

  protected final void createPropertyLabel( final Composite parent, final QName property )
  {
    final IPropertyType propertyType = m_featureBean.getFeatureType().getProperty( property );
    final String label = AnnotationUtilities.getAnnotation( propertyType.getAnnotation(), null, IAnnotation.ANNO_LABEL );
    final FormToolkit toolkit = m_binding.getToolkit();
    if( toolkit != null )
      toolkit.createLabel( parent, label );
    else
    {
      final Label propertyLabel = new Label( parent, SWT.NONE );
      propertyLabel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
      propertyLabel.setText( label );
    }
  }

  protected final Text createPropertyTextField( final Composite parent )
  {
    int style = SWT.BORDER | SWT.SINGLE;
    if( !m_generalEditable )
      style &= SWT.READ_ONLY;

    final FormToolkit toolkit = m_binding.getToolkit();
    Text field = null;
    if( toolkit != null )
      field = toolkit.createText( parent, StringUtils.EMPTY, style );
    else
      field = new Text( parent, style );

    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setEditable( m_generalEditable );
    field.setEnabled( m_generalEditable );

    return field;
  }

  protected final ComboViewer createPropertyCombo( final Composite parent, final LabelProvider provider, final Boolean writeable )
  {
    int style = SWT.BORDER | SWT.SINGLE;
    if( !m_generalEditable && !writeable )
      style &= SWT.READ_ONLY;

    final ComboViewer viewer = new ComboViewer( parent, style );
    viewer.getCombo().setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    viewer.getCombo().setEnabled( m_generalEditable );

    viewer.setContentProvider( new ArrayContentProvider() );
    viewer.setLabelProvider( provider );

    final FormToolkit toolkit = m_binding.getToolkit();
    if( toolkit != null )
      toolkit.adapt( viewer.getCombo() );

    return viewer;
  }

  protected final void bindTextField( final Text field, final QName property, final IValidator... validators )
  {
    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = new FeatureBeanObservableValue( m_featureBean, property );

    if( ArrayUtils.isNotEmpty( validators ) )
      m_binding.bindValue( target, model, validators );
    else
    {
      final DataBinder binder = new DataBinder( target, model );
      if( Feature.QN_NAME.equals( property ) )
      {
        binder.setModelToTargetConverter( new FeatureNameModelToTargetConverter() );
        binder.setTargetToModelConverter( new FeatureNameTargetToModelConverter() );
      }

      m_binding.bindValue( binder );
    }
  }

  protected final void bindCombo( final ComboViewer viewer, final QName property )
  {
    final IViewerObservableValue target = ViewersObservables.observeSingleSelection( viewer );
    final IObservableValue model = new FeatureBeanObservableValue( m_featureBean, property );

    final DataBinder binder = new DataBinder( target, model );
    m_binding.bindValue( binder );
  }

  protected final void bindDateControl( final DateTimeControl control, final QName property )
  {

    final ISWTObservableValue target = SWTObservables.observeText( control.getText(), SWT.Modify );
    final IObservableValue model = new FeatureBeanObservableValue( m_featureBean, property );

    final DataBinder binder = new DataBinder( target, model );
    binder.setModelToTargetConverter( new DateControlModelToTargetConverter( KalypsoCorePlugin.getDefault().getTimeZone() ) );
    binder.setTargetToModelConverter( new DateControlTargetToModelConverter( control ) );

    m_binding.bindValue( binder );

// final IObservableValue target = ViewersObservables.observeSingleSelection( provider );
// final IObservableValue model = new FeatureBeanObservableValue( m_featureBean, property );
//
// final DataBinder binder = new DataBinder( target, model );
// m_binding.bindValue( binder );
  }
}