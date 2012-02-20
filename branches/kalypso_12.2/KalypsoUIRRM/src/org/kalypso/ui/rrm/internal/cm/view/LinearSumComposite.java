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

import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Text;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;

/**
 * @author Gernot Belger
 */
public class LinearSumComposite extends FeatureBeanComposite<ILinearSumGenerator>
{
  public LinearSumComposite( final Composite parent, final FeatureBean<ILinearSumGenerator> bean, final IDataBinding binding, final boolean editable )
  {
    super( parent, bean, binding, editable );
  }

  @Override
  protected void createContents( )
  {
    createPropertyControl( ILinearSumGenerator.QN_DESCRIPTION );
    createPropertyControl( ILinearSumGenerator.PROPERTY_COMMENT );
    createPropertyControl( ILinearSumGenerator.PROPERTY_TIMESTEP );
    createPropertyControl( ILinearSumGenerator.PROPERTY_TIMESTAMP );
    createParameterTypeControl();
  }

  private void createParameterTypeControl( )
  {
    createPropertyLabel( this, ILinearSumGenerator.PROPERTY_PARAMETER_TYPE );

    final Text field = createPropertyTextField( this );

    final String parameterType = (String) getBean().getProperty( ILinearSumGenerator.PROPERTY_PARAMETER_TYPE );
    final String parameterLabel = ParameterTypeUtils.formatParameterType( parameterType );

    field.setText( parameterLabel );
  }
}