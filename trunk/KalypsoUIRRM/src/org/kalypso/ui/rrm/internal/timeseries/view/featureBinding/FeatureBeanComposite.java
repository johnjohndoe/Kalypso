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
package org.kalypso.ui.rrm.internal.timeseries.view.featureBinding;

import javax.xml.namespace.QName;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.databinding.observable.value.IObservableValue;
import org.eclipse.jface.databinding.swt.ISWTObservableValue;
import org.eclipse.jface.databinding.swt.SWTObservables;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.commons.databinding.DataBinder;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.gmlschema.annotation.AnnotationUtilities;
import org.kalypso.gmlschema.annotation.IAnnotation;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Gernot Belger
 */
public abstract class FeatureBeanComposite<F extends Feature> extends Composite
{
  private final FeatureBean<F> m_featureBean;

  private final IDataBinding m_binding;

  private final boolean m_editable;

  public FeatureBeanComposite( final Composite parent, final FeatureBean<F> featureBean, final IDataBinding binding, final boolean editable )
  {
    super( parent, SWT.NONE );

    m_featureBean = featureBean;
    m_binding = binding;
    m_editable = editable;

    m_binding.getToolkit().adapt( this );

    super.setLayout( GridLayoutFactory.fillDefaults().create() );

    createContents();
  }

  protected final boolean isEditable( )
  {
    return m_editable;
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

  protected final void createPropertyControl( final QName property )
  {
    createPropertyLabel( this, property );

    final Text field = createPropertyTextField( this );

    bindTextField( field, property );
  }

  /**
   * Refresh the control from the state of the underlying feature.
   */
  public void refresh( )
  {
    m_featureBean.revert();
  }

  protected final void createPropertyLabel( final Composite parent, final QName property )
  {
    final IPropertyType propertyType = m_featureBean.getFeatureType().getProperty( property );
    final String label = AnnotationUtilities.getAnnotation( propertyType.getAnnotation(), null, IAnnotation.ANNO_LABEL );

    final FormToolkit toolkit = m_binding.getToolkit();

    toolkit.createLabel( parent, label );
  }

  protected final Text createPropertyTextField( final Composite parent )
  {
    int style = SWT.BORDER | SWT.SINGLE;
    if( !m_editable )
      style &= SWT.READ_ONLY;

    final FormToolkit toolkit = m_binding.getToolkit();

    final Text field = toolkit.createText( parent, StringUtils.EMPTY, style );
    field.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false ) );
    field.setEditable( m_editable );
    field.setEnabled( m_editable );

    return field;
  }

  protected final void bindTextField( final Text field, final QName property )
  {
    final ISWTObservableValue target = SWTObservables.observeText( field, SWT.Modify );
    final IObservableValue model = new FeatureBeanObservableValue( m_featureBean, property );

    final DataBinder binder = new DataBinder( target, model );

    if( Feature.QN_NAME.equals( property ) )
    {
      binder.setModelToTargetConverter( new FeatureNameModelToTargetConverter() );
      binder.setTargetToModelConverter( new FeatureNameTargetToModelConverter() );
    }

    // TODO: restrictions!

    m_binding.bindValue( binder );
  }
}