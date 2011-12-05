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
package org.kalypso.ui.rrm.wizards.conversion.to10_10;

import java.net.URL;
import java.util.HashSet;
import java.util.Set;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.joda.time.Interval;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.model.hydrology.binding.model.Catchment;
import org.kalypso.model.hydrology.binding.model.Channel;
import org.kalypso.model.hydrology.binding.model.Node;
import org.kalypso.ogc.sensor.util.ZmlLink;
import org.kalypso.ui.rrm.KalypsoUIRRMPlugin;
import org.kalypso.ui.rrm.i18n.Messages;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureVisitor;

/**
 * Checks each timeserie if it is long enough. Extend instantaneous values (same value as start/end)
 * 
 * @author Gernot Belger
 */
public class TimeseriesExtendVisitor implements FeatureVisitor
{
  private final IStatusCollector m_log = new StatusCollector( KalypsoUIRRMPlugin.getID() );

  private final Interval m_simulationRange;

  private final Set<URL> m_extendedTimeseries = new HashSet<URL>();

  public TimeseriesExtendVisitor( final Interval simulationRange )
  {
    m_simulationRange = simulationRange;
  }

  /**
   * @see org.kalypsodeegree.model.feature.FeatureVisitor#visit(org.kalypsodeegree.model.feature.Feature)
   */
  @Override
  public boolean visit( final Feature f )
  {
    if( f instanceof Node )
      return visitNode( (Node) f );

    if( f instanceof Catchment )
      return visitCatchment( (Catchment) f );

    if( f instanceof Channel )
      return visitChannel( (Channel) f );

    return true;
  }

  private boolean visitNode( @SuppressWarnings("unused") final Node node )
  {
    // Possible timeseries, see below. Do we need to tweak anything?
    // pegelZR
    // zuflussZR

    /* No need to dig further */
    return false;
  }

  private boolean visitCatchment( final Catchment catchment )
  {
    extendTimeseries( catchment, Catchment.PROP_PRECIPITATION_LINK );
    extendTimeseries( catchment, Catchment.PROP_TEMPERATURE_LINK );
    extendTimeseries( catchment, Catchment.PROP_EVAPORATION_LINK );

    /* No need to dig further */
    return false;
  }

  private boolean visitChannel( @SuppressWarnings("unused") final Channel channel )
  {
    // nothing to do, for the moment, channels have no timeseries

    /* No need to dig further */
    return false;
  }

  public IStatus getStatus( )
  {
    return m_log.asMultiStatus( Messages.getString("TimeseriesExtendVisitor_0") ); //$NON-NLS-1$
  }

  private void extendTimeseries( final Feature feature, final QName linkProperty )
  {
    /*
     * Performance: extend every timeserie only once. KalypsoHydrology often reuses the same .zml for several
     * catchments...
     */
    final ZmlLink zmlLink = new ZmlLink( feature, linkProperty );
    final URL location = zmlLink.getLocation();
    if( m_extendedTimeseries.contains( location ) )
      return;
    m_extendedTimeseries.add( location );

    /* Check, if this timeseries needs to be extended */
    final TimeseriesExtendOperation operation = new TimeseriesExtendOperation( zmlLink, m_simulationRange );
    final IStatus status = operation.execute( new NullProgressMonitor() );
    if( !status.isOK() )
      m_log.add( status );
  }

}
