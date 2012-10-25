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
package org.kalypso.model.wspm.pdb.wspm;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Date;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.TreeMap;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.Session;
import org.hibernatespatial.mgeom.MGeometryFactory;
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
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.wspm.Waterlevel2dWorker;

import com.vividsolutions.jts.geom.PrecisionModel;

/**
 * @author Gernot Belger
 */
public class SaveEventOperation implements IPdbOperation
{
  private final Event m_event;

  private final String m_username;

  private final MGeometryFactory m_geometryFactory;

  private IStatus m_log;

  public SaveEventOperation( final Event event, final String username, final int dbSRID )
  {
    m_event = event;
    m_username = username;
    m_geometryFactory = new MGeometryFactory( new PrecisionModel(), dbSRID );
  }

  @Override
  public String getLabel( )
  {
    return "Save Event";
  }

  public IStatus getLog( )
  {
    return m_log;
  }

  @Override
  public void execute( final Session session ) throws PdbConnectException
  {
    /* Fetch coefficients, we need to known the unknown classes */
    final ICoefficients coefficients = new Coefficients( session, IGafConstants.POINT_KIND_GAF );

    /* Prepare event for save */
    final Date now = new Date();
    m_event.setCreationDate( now );
    m_event.setEditingDate( now );
    m_event.setEditingUser( m_username );

    /* save event */
    session.save( m_event );

    /* build 2d waterlevels */
    final String eventName = m_event.getName();
    final Set<WaterlevelFixation> waterlevels = m_event.getWaterlevelFixations();
    final Map<BigDecimal, Collection<ISectionProvider>> sectionsByStation = hashSectionsByStation();
    final Waterlevel2dWorker waterlevel2dWorker = new Waterlevel2dWorker( eventName, waterlevels, sectionsByStation );
    m_log = waterlevel2dWorker.execute();
    final Map<IProfileObject, ISectionProvider> waterlevels2d = waterlevel2dWorker.getWaterlevels2D();

    /* update wl_type */
    if( waterlevels2d.size() == 0 )
      m_event.setWlType( WL_TYPE.WL_1D );
    else
      m_event.setWlType( WL_TYPE.WL_2D );

    /* save changed event */
    session.save( m_event );

    /* save fixations */
    saveFixations( session, waterlevels, now );

    /* save 2d waterlevels */
    saveWaterlevels2D( session, waterlevels2d, coefficients );
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

  private void saveWaterlevels2D( final Session session, final Map<IProfileObject, ISectionProvider> waterlevels2d, final ICoefficients coefficients ) throws PdbConnectException
  {
    try
    {
      final int targetSRID = m_geometryFactory.getSRID();
      // REMARK: profile objects have been build from db objects, so srs is the same
      final int profileSRID = targetSRID;

      final CrossSectionPartTypes partTypes = new CrossSectionPartTypes( session );

      for( final Entry<IProfileObject, ISectionProvider> entry : waterlevels2d.entrySet() )
      {
        final IProfileObject object = entry.getKey();
        final ISectionProvider sectionProvider = entry.getValue();

        final BigDecimal station = sectionProvider.getStation();

        final CheckinHorizonPartOperation operation = new CheckinHorizonPartOperation( object, profileSRID, targetSRID, station.doubleValue(), partTypes, m_event, coefficients );
        operation.execute();

        final CrossSectionPart part = operation.getPart();
        part.setCrossSection( sectionProvider.getSection() );
        session.save( part );

        final Set<Point> points = part.getPoints();
        for( final Point point : points )
          session.save( point );
      }
    }
    catch( final Exception e )
    {
      throw new PdbConnectException( "Failed to transform to db coordinate system", e ); //$NON-NLS-1$
    }
  }

  private Map<BigDecimal, Collection<ISectionProvider>> hashSectionsByStation( )
  {
    final Map<BigDecimal, Collection<ISectionProvider>> hash = new TreeMap<>();

    final State state = m_event.getState();
    if( state == null )
      return hash;

    final Set<CrossSection> sections = state.getCrossSections();

    for( final CrossSection section : sections )
    {
      final ISectionProvider provider = new CrossSectionProvider( section, m_geometryFactory );

      final BigDecimal station = provider.getStation();

      if( !hash.containsKey( station ) )
        hash.put( station, new ArrayList<ISectionProvider>() );

      hash.get( station ).add( provider );
    }

    return hash;
  }

  public static boolean askForEmptyState( final Event event, final Shell shell, final String dialogTitle )
  {
    if( event.getState() != null )
      return true;

    final String message = "Das Ereignis ist keinem Zustand zugeordnet, 2D-Wasserspiegel werden nicht erzeugt. Trotzdem fortfahren?";
    return MessageDialog.openConfirm( shell, dialogTitle, message );
  }
}