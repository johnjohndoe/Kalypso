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
package org.kalypso.model.wspm.pdb.internal.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.hibernate.Session;
import org.hibernatespatial.mgeom.MGeometryFactory;
import org.kalypso.commons.java.lang.Objects;
import org.kalypso.model.wspm.core.IWspmLengthSectionProperties;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.pdb.connect.IPdbConnection;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.WaterBody;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.db.utils.EventUtils;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.internal.waterlevel2d.Waterlevel2dWorker;
import org.kalypso.model.wspm.pdb.wspm.CrossSectionProvider;
import org.kalypso.model.wspm.pdb.wspm.ISectionProvider;
import org.kalypso.model.wspm.pdb.wspm.SaveEventOperation;
import org.kalypso.model.wspm.pdb.wspm.WaterlevelsForStation;
import org.kalypso.observation.IObservation;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.transformation.transformer.JTSTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.opengis.geometry.MismatchedDimensionException;
import org.opengis.referencing.FactoryException;
import org.opengis.referencing.operation.TransformException;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Point;
import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Gernot Belger
 */
public abstract class AbstractCheckinEventOperation implements IPdbOperation
{
  static final String STR_FAILED_TO_CONVERT_GEOMETRY = Messages.getString( "AbstractCheckinEventOperation.0" ); //$NON-NLS-1$

  private final MGeometryFactory m_factory;

  private final Map<String, WaterBody> m_waterBodies;

  private final IProgressMonitor m_monitor;

  private final Event m_event;

  private final WspmWaterBody m_wspmWaterBody;

  private final IPdbConnection m_connection;

  private IStatus m_log;

  public AbstractCheckinEventOperation( final IPdbConnection connection, final Map<String, WaterBody> waterHash, final WspmWaterBody wspmWaterBody, final Event event, final IProgressMonitor monitor )
  {
    m_connection = connection;
    m_wspmWaterBody = wspmWaterBody;
    m_event = event;
    m_waterBodies = waterHash;

    final int dbSRID = connection.getInfo().getSRID();
    m_factory = new MGeometryFactory( new PrecisionModel(), dbSRID );

    m_monitor = monitor;
  }

  public IStatus getLog( )
  {
    return m_log;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "AbstractCheckinEventOperation.1" ); //$NON-NLS-1$
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    m_monitor.beginTask( Messages.getString( "AbstractCheckinEventOperation.2" ), IProgressMonitor.UNKNOWN ); //$NON-NLS-1$

    m_monitor.subTask( Messages.getString( "AbstractCheckinEventOperation.3" ) ); //$NON-NLS-1$

    // FIXME: we should clone the event object, else we get problems if the upload fails with an error

    /* set water body */
    m_event.setWaterBody( findWaterBody() );

    try
    {
      /* convert observation to fixations */
      convertWaterlevels();
    }
    catch( final FactoryException | MismatchedDimensionException | TransformException e )
    {
      e.printStackTrace();
      throw new PdbConnectException( "Failed ot convert waterlevels", e );
    }

    /* load cross sections */
    final ISectionProvider[] sections = loadSections( session );

    /* build 2d waterlevels */
    final WaterlevelsForStation[] waterlevels2d = build2Dwaterlevels( sections, new NullProgressMonitor() );

    /* save event */
    final String username = m_connection.getSettings().getUsername();
    final int dbSRSID = m_connection.getInfo().getSRID();
    final SaveEventOperation operation = new SaveEventOperation( m_event, username, dbSRSID, waterlevels2d );
    m_log = operation.execute( session, new NullProgressMonitor() );

    m_monitor.subTask( Messages.getString( "AbstractCheckinEventOperation.4" ) ); //$NON-NLS-1$
  }

  private ISectionProvider[] loadSections( final Session session )
  {
    final Set<CrossSection> crossSections = EventUtils.loadSectionsForStateName( session, m_event );

    final Collection<ISectionProvider> sections = new ArrayList<>( crossSections.size() );

    for( final CrossSection crossSection : crossSections )
    {
      final CrossSectionProvider provider = new CrossSectionProvider( crossSection, m_factory );
      sections.add( provider );
    }

    return sections.toArray( new ISectionProvider[sections.size()] );
  }

  private WaterlevelsForStation[] build2Dwaterlevels( final ISectionProvider[] sections, final IProgressMonitor monitor )
  {
    final String eventName = m_event.getName();
    final Collection<WaterlevelFixation> fixations = m_event.getWaterlevelFixations();

    final Waterlevel2dWorker worker = new Waterlevel2dWorker( eventName, fixations, sections );
    worker.execute( monitor );

    return worker.getWaterlevels2D();
  }

  private void convertWaterlevels( ) throws FactoryException, MismatchedDimensionException, TransformException
  {
    /* geo transformer */
    // TODO: it is not save using Kalypso srs here; we should rather save the srs in the waterlevel as well.
    final int kalypsoSRID = JTSAdapter.toSrid( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    final JTSTransformer transformer = new JTSTransformer( kalypsoSRID, m_factory.getSRID() );

    /* a waterlevle for each record of the observation */
    final IObservation<TupleResult> observation = getObservation();

    final TupleResult result = observation.getResult();

    final int stationIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_STATION );
    final int waterlevelIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_WATERLEVEL );
    final int runoffIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_RUNOFF );
    final int commentIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_TEXT );
    final int eastingIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_EASTING );
    final int northingIndex = result.indexOfComponent( IWspmLengthSectionProperties.LENGTH_SECTION_PROPERTY_NORTHING );

    for( final IRecord record : result )
    {
      /* retreive values */
      final BigDecimal station = asBigDecimal( stationIndex, record );
      final BigDecimal stationM = station == null ? null : station.movePointRight( 3 );
      final BigDecimal wsp = asBigDecimal( waterlevelIndex, record );
      final BigDecimal runoff = asBigDecimal( runoffIndex, record );
      final String comment = asString( commentIndex, record );
      final BigDecimal easting = asBigDecimal( eastingIndex, record );
      final BigDecimal northing = asBigDecimal( northingIndex, record );

      final Point location = toLocation( transformer, easting, northing );

      /* create new fixation */
      final WaterlevelFixation element = new WaterlevelFixation();

      element.setStation( stationM );
      element.setWaterlevel( wsp );
      element.setDischarge( runoff );
      element.setDescription( comment );

      element.setLocation( location );

      element.setEvent( m_event );
      m_event.getWaterlevelFixations().add( element );
    }
  }

  private Point toLocation( final JTSTransformer transformer, final BigDecimal easting, final BigDecimal northing ) throws MismatchedDimensionException, TransformException
  {
    if( Objects.isNull( easting, northing ) )
      return null;

    final Coordinate coordinate = new Coordinate( easting.doubleValue(), northing.doubleValue() );
    final Coordinate transformed = transformer.transform( coordinate );

    return m_factory.createPoint( transformed );
  }

  protected abstract IObservation<TupleResult> getObservation( );

  private BigDecimal asBigDecimal( final int componentIndex, final IRecord record )
  {
    final Object value = getValue( componentIndex, record );
    if( value instanceof BigDecimal )
      return (BigDecimal)value;

    if( value instanceof Number )
      return new BigDecimal( ((Number)value).doubleValue() );

    return null;
  }

  private String asString( final int componentIndex, final IRecord record )
  {
    final Object value = getValue( componentIndex, record );
    if( value == null )
      return null;

    return (String)value;
  }

  private Object getValue( final int componentIndex, final IRecord record )
  {
    if( componentIndex == -1 )
      return null;

    return record.getValue( componentIndex );
  }

  private WaterBody findWaterBody( )
  {
    final String refNr = m_wspmWaterBody.getRefNr();
    return m_waterBodies.get( refNr );
  }
}