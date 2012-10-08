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
package org.kalypso.model.wspm.pdb.ui.internal.admin.waterbody.imports;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.hibernate.Session;
import org.hibernatespatial.mgeom.MCoordinate;
import org.hibernatespatial.mgeom.MGeometryFactory;
import org.hibernatespatial.mgeom.MLineString;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.pdb.connect.IPdbOperation;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.constants.EventConstants.WL_TYPE;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.State;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.db.utils.CrossSectionPartTypes;
import org.kalypso.model.wspm.pdb.gaf.GafKind;
import org.kalypso.model.wspm.pdb.ui.internal.i18n.Messages;
import org.kalypso.model.wspm.pdb.wspm.CheckinHorizonPartOperation;

/**
 * @author Gernot Belger
 */
public class SaveEventOperation implements IPdbOperation
{
  private final Event m_event;

  private final String m_username;

  private final MGeometryFactory m_geometryFactory;

  private IStatus m_log;

  public SaveEventOperation( final Event event, final String username, final MGeometryFactory geometryFactory )
  {
    m_event = event;
    m_username = username;
    m_geometryFactory = geometryFactory;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "SaveEventOperation.0" ); //$NON-NLS-1$
  }

  public IStatus getLog( )
  {
    return m_log;
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    /* build 2d waterlevels */
    final String eventName = m_event.getName();
    final Set<WaterlevelFixation> waterlevels = m_event.getWaterlevelFixations();
    final Map<BigDecimal, Collection<MLineString>> sectionsByStation = hashSectionsByStation();
    final Waterlevel2dWorker waterlevel2dWorker = new Waterlevel2dWorker( eventName, waterlevels, sectionsByStation );
    m_log = waterlevel2dWorker.execute();
    final Map<IProfileObject, BigDecimal> waterlevels2d = waterlevel2dWorker.getWaterlevels2D();

    /* Prepare event for save */
    final Date now = new Date();
    m_event.setCreationDate( now );
    m_event.setEditingDate( now );
    m_event.setEditingUser( m_username );

    /* update wl_type */
    if( waterlevels2d.size() == 0 )
      m_event.setWlType( WL_TYPE.WL_1D );
    else
      m_event.setWlType( WL_TYPE.WL_2D );

    /* save event */
    session.save( m_event );

    /* save fixations */
    saveFixations( session, waterlevels, now );

    /* save 2d waterlevels */
    saveWaterlevels2D( session, waterlevels2d );
  }

  private void saveFixations( final Session session, final Set<WaterlevelFixation> waterlevels, final Date now )
  {
    for( final WaterlevelFixation waterlevel : waterlevels )
    {
      waterlevel.setCreationDate( now );
      waterlevel.setEditingDate( now );
      waterlevel.setEditingUser( m_username );

      waterlevel.setEvent( m_event );

      if( waterlevel.getMeasurementDate() == null )
        waterlevel.setMeasurementDate( m_event.getMeasurementDate() );

      session.save( waterlevel );
    }
  }

  private void saveWaterlevels2D( final Session session, final Map<IProfileObject, BigDecimal> waterlevels2d ) throws PdbConnectException
  {
    try
    {
      final int targetSRID = m_geometryFactory.getSRID();
      // REMARK: profile objects have been build forfrom db objects, so srs is the same
      final int profileSRID = targetSRID;

      final CrossSectionPartTypes partTypes = new CrossSectionPartTypes( session );

      for( final Entry<IProfileObject, BigDecimal> entry : waterlevels2d.entrySet() )
      {
        final IProfileObject object = entry.getKey();
        final BigDecimal station = entry.getValue();

        final CheckinHorizonPartOperation operation = new CheckinHorizonPartOperation( object, profileSRID, targetSRID, station.doubleValue(), partTypes, m_event );
        operation.execute();
      }
    }
    catch( final Exception e )
    {
      throw new PdbConnectException( "Failed to transform to db coordinate system", e ); //$NON-NLS-1$
    }
  }

  private Map<BigDecimal, Collection<MLineString>> hashSectionsByStation( )
  {
    final Map<BigDecimal, Collection<MLineString>> hash = new TreeMap<>();

    final State state = m_event.getState();
    if( state == null )
      return hash;

    final Set<CrossSection> sections = state.getCrossSections();

    for( final CrossSection section : sections )
    {
      final BigDecimal station = section.getStation();

      if( !hash.containsKey( station ) )
        hash.put( station, new ArrayList<MLineString>() );

      final MLineString profileLine = buildProfileLine( section );
      if( profileLine != null )
        hash.get( station ).add( profileLine );
    }

    return hash;
  }

  private MLineString buildProfileLine( final CrossSection section )
  {
    final Set<CrossSectionPart> parts = section.getCrossSectionParts();
    for( final CrossSectionPart part : parts )
    {
      final String category = part.getCrossSectionPartType().getCategory();
      if( GafKind.P.toString().equals( category ) )
      {
        final MLineString line = buildProfileLine( part );
        if( line != null )
          return line;
      }
    }

    return null;
  }

  private MLineString buildProfileLine( final CrossSectionPart ppart )
  {
    /* sort points by consecutive number */
    final Map<Long, Point> sortedPoints = new TreeMap<>();

    final Set<Point> points = ppart.getPoints();
    for( final Point point : points )
      sortedPoints.put( point.getConsecutiveNum(), point );

    if( sortedPoints.size() < 2 )
      return null;

    /* rebuild line as line M */
    final Collection<MCoordinate> coords = new ArrayList<>( sortedPoints.size() );
    for( final Point point : sortedPoints.values() )
    {
      final com.vividsolutions.jts.geom.Point location = point.getLocation();

      final double xValue = location.getX();
      final double yValue = location.getY();

      final BigDecimal mValue = point.getWidth();
      final BigDecimal zValue = point.getHeight();

      final MCoordinate mCoord = new MCoordinate( xValue, yValue, zValue.doubleValue(), mValue.doubleValue() );
      coords.add( mCoord );
    }

    return m_geometryFactory.createMLineString( coords.toArray( new MCoordinate[coords.size()] ) );
  }
}