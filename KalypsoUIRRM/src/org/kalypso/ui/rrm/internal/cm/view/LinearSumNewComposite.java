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

import org.eclipse.jface.viewers.ComboViewer;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.rcm.binding.ILinearSumGenerator;
import org.kalypso.ogc.sensor.metadata.ITimeseriesConstants;
import org.kalypso.ui.rrm.internal.utils.ParameterTypeUtils;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;

/**
 * @author Gernot Belger
 */
public class LinearSumNewComposite extends FeatureBeanComposite<ILinearSumGenerator>
{
  private static final String[] ALLOWED_PARAMETER_TYPES = new String[] { ITimeseriesConstants.TYPE_RAINFALL, ITimeseriesConstants.TYPE_TEMPERATURE, ITimeseriesConstants.TYPE_EVAPORATION };

  public LinearSumNewComposite( final Composite parent, final FeatureBean<ILinearSumGenerator> bean, final IDataBinding binding )
  {
    super( parent, bean, binding, true );
  }

  @Override
  protected void createContents( )
  {
    createPropertyControl( ILinearSumGenerator.QN_DESCRIPTION );
    createPropertyControl( ILinearSumGenerator.PROPERTY_COMMENT );
    createParameterTypeControl();
  }

  private void createParameterTypeControl( )
  {
    final LinkedHashMap<String, String> allowedValues = new LinkedHashMap<String, String>();
    for( String allowedParameterType : ALLOWED_PARAMETER_TYPES )
    {
      final String allowedParameterLabel = ParameterTypeUtils.formatParameterType( allowedParameterType );
      allowedValues.put( allowedParameterType, allowedParameterLabel );
    }

    final ComboViewer comboViewer = createPropertyCombo( this, allowedValues );
    bindCombo( comboViewer, ILinearSumGenerator.PROPERTY_PARAMETER_TYPE );
  }
}