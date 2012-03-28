/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 *
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestra�e 22
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
package org.kalypso.ui.rrm.internal.timeseries.view.actions;

import org.kalypso.model.hydrology.timeseries.binding.ITimeseries;
import org.kalypso.ui.rrm.internal.UIRrmImages;
import org.kalypso.ui.rrm.internal.UIRrmImages.DESCRIPTORS;
import org.kalypso.ui.rrm.internal.i18n.Messages;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.IMergeTimeseriesOperation;
import org.kalypso.ui.rrm.internal.timeseries.view.imports.ReplaceTimeseriesObservation;
import org.kalypso.ui.rrm.internal.utils.featureBinding.FeatureBean;
import org.kalypso.ui.rrm.internal.utils.featureTree.ITreeNodeModel;

/**
 * @author Dirk Kuch
 */
public class ReplaceTimeseriesAction extends AbstractOverwriteTimeseriesAction
{

  public ReplaceTimeseriesAction( final ITreeNodeModel model, final FeatureBean<ITimeseries> timeseries )
  {
    super( model, timeseries );

    setText( Messages.getString("ReplaceTimeseriesAction_0") ); //$NON-NLS-1$
    setToolTipText( Messages.getString("ReplaceTimeseriesAction_1") ); //$NON-NLS-1$

    setImageDescriptor( UIRrmImages.id( DESCRIPTORS.TIMESERIES_REPLACE ) );
  }

  @Override
  protected IMergeTimeseriesOperation getMergeOperation( )
  {
    return new ReplaceTimeseriesObservation( getTimeseries() );
  }

}
