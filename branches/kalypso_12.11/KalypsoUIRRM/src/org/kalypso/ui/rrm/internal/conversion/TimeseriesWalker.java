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
package org.kalypso.ui.rrm.internal.conversion;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.channels.IStorageChannel;
import org.kalypso.model.hydrology.binding.model.channels.StorageChannel;
import org.kalypso.model.hydrology.binding.model.nodes.Node;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Visits each timeserie of the na model and executes an operation.
 *
 * @author Gernot Belger
 */
public class TimeseriesWalker implements FeatureVisitor
{
  private final ITimeseriesVisitor m_operation;

  private final IStatusCollector m_log;

  /**
   * @param log
   *          All non-ok result of the visitor will be added to this log.
   */
  public TimeseriesWalker( final ITimeseriesVisitor visitor, final IStatusCollector log )
  {
    m_operation = visitor;
    m_log = log;
  }

  @Override
  public boolean visit( final Feature f )
  {
    if( f instanceof Node )
      return visitNode( (Node) f );

    if( f instanceof Catchment )
      return visitCatchment( (Catchment) f );

    if( f instanceof StorageChannel )
      return visitStorageChannel( (StorageChannel) f );

    return true;
  }

  private boolean visitNode( final Node node )
  {
    visitTimeseries( node, Node.PROPERTY_PEGEL_ZR );
    visitTimeseries( node, Node.PROPERTY_RESULT_TIMESERIESLINK );
    visitTimeseries( node, Node.PROPERTY_ZUFLUSS_ZR );
    visitTimeseries( node, Node.PROPERTY_RESULT_AS_INFLOW_ZR );

    /* No need to dig further */
    return false;
  }

  private boolean visitCatchment( final Catchment catchment )
  {
    visitTimeseries( catchment, Catchment.PROP_PRECIPITATION_LINK );
    visitTimeseries( catchment, Catchment.PROP_TEMPERATURE_LINK );
    visitTimeseries( catchment, Catchment.PROP_EVAPORATION_LINK );

    /* No need to dig further */
    return false;
  }

  private boolean visitStorageChannel( final StorageChannel channel )
  {
    visitTimeseries( channel, IStorageChannel.PROPERTY_SEA_EVAPORATION_ZMLLINK );
    // nothing to do, for the moment, channels have no timeseries

    /* No need to dig further */
    return false;
  }

  private void visitTimeseries( final Feature feature, final QName linkProperty )
  {
    final IStatus status = m_operation.visit( feature, linkProperty );
    if( !status.isOK() )
      m_log.add( status );
  }
}