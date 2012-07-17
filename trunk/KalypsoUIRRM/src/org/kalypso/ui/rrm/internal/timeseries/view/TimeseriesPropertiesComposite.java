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

import org.eclipse.core.databinding.validation.IValidator;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.widgets.Composite;
import org.kalypso.commons.databinding.IDataBinding;
import org.kalypso.model.hydrology.binding.timeseries.IStation;
import org.kalypso.model.hydrology.binding.timeseries.ITimeseries;
import org.kalypso.ui.rrm.internal.timeseries.view.edit.EditTimeseriesQualityComposite;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBeanComposite;

/**
 * @author Dirk Kuch
 */
public class TimeseriesPropertiesComposite extends FeatureBeanComposite<ITimeseries>
{
  private final boolean m_readOnly;

  private final IValidator m_qualityValidator;

  public TimeseriesPropertiesComposite( final IStation station, final Composite parent, final FeatureBean<ITimeseries> featureBean, final IDataBinding binding, final boolean readOnly, final IValidator qualityValidator )
  {
    super( parent, featureBean, binding, true );

    m_readOnly = readOnly;

    m_qualityValidator = qualityValidator;

    doCreateContents( station );
  }

  private void doCreateContents( final IStation station )
  {
    final EditTimeseriesQualityComposite quality = new EditTimeseriesQualityComposite( this, station, getBean(), getBinding(), !m_readOnly, m_qualityValidator );
    quality.setLayoutData( new GridData( GridData.FILL, GridData.FILL, true, false, 2, 1 ) );

    createPropertyTextFieldControl( ITimeseries.QN_DESCRIPTION );

    this.layout();
  }

  @Override
  protected void createContents( )
  {
    // nothing to do
  }
}