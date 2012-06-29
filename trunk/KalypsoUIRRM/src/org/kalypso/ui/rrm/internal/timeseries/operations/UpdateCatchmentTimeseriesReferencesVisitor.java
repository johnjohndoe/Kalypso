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
package org.kalypso.ui.rrm.internal.timeseries.operations;

import org.eclipse.core.resources.IFile;
import org.kalypso.model.hydrology.binding.cm.ICatchment;
import org.kalypso.model.hydrology.binding.cm.IFactorizedTimeseries;
import org.kalypso.model.hydrology.binding.cm.ILinearSumGenerator;
import org.kalypso.model.rcm.binding.IRainfallGenerator;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.zml.obslink.TimeseriesLinkType;
import org.kalypsodeegree.model.feature.IFeatureBindingCollection;
import org.kalypsodeegree.model.feature.IFeatureBindingCollectionVisitor;

import com.google.common.base.Objects;

/**
 * updates time series links of catchment model generators
 * 
 * @author Dirk Kuch
 */
public class UpdateCatchmentTimeseriesReferencesVisitor implements IFeatureBindingCollectionVisitor<IRainfallGenerator>
{

  private final IFile m_oldFile;

  private final String m_href;

  private boolean m_changed = false;

  public UpdateCatchmentTimeseriesReferencesVisitor( final IFile oldFile, final String href )
  {
    m_oldFile = oldFile;
    m_href = href;
  }

  @Override
  public void visit( final IRainfallGenerator generator )
  {
    if( !(generator instanceof ILinearSumGenerator) )
      return;

    final ILinearSumGenerator linear = (ILinearSumGenerator) generator;
    final IFeatureBindingCollection<ICatchment> catchments = linear.getCatchments();
    for( final ICatchment catchment : catchments )
    {
      final IFeatureBindingCollection<IFactorizedTimeseries> collection = catchment.getFactorizedTimeseries();
      for( final IFactorizedTimeseries factorized : collection )
      {
        final ZmlLink timeseriesLink = factorized.getTimeseriesLink();
        final IFile lnk = timeseriesLink.getFile();
        if( Objects.equal( m_oldFile, lnk ) )
        {
          final TimeseriesLinkType linkType = timeseriesLink.getTimeseriesLink();
          linkType.setHref( m_href );
          factorized.setTimeseriesLink( m_href );

          m_changed = true;
        }
      }
    }
  }

  public boolean wasChanged( )
  {
    return m_changed;
  }
}