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
import java.util.Arrays;
import java.util.Date;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.hibernate.Session;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.pdb.connect.AbstractPdbOperationWithMonitor;
import org.kalypso.model.wspm.pdb.connect.PdbConnectException;
import org.kalypso.model.wspm.pdb.db.constants.EventConstants.WL_TYPE;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSection;
import org.kalypso.model.wspm.pdb.db.mapping.CrossSectionPart;
import org.kalypso.model.wspm.pdb.db.mapping.Event;
import org.kalypso.model.wspm.pdb.db.mapping.Point;
import org.kalypso.model.wspm.pdb.db.mapping.WaterlevelFixation;
import org.kalypso.model.wspm.pdb.db.utils.CrossSectionPartTypes;
import org.kalypso.model.wspm.pdb.gaf.ICoefficients;
import org.kalypso.model.wspm.pdb.gaf.IGafConstants;
import org.kalypso.model.wspm.pdb.internal.gaf.Coefficients;
import org.kalypso.model.wspm.pdb.internal.i18n.Messages;

/**
 * @author Gernot Belger
 */
public class SaveEventOperation extends AbstractPdbOperationWithMonitor
{
  private final Event m_event;

  private final String m_username;

  private final WaterlevelsForStation[] m_waterlevels2d;

  private final int m_dbSRID;

  public SaveEventOperation( final Event event, final String username, final int dbSRID, final WaterlevelsForStation[] waterlevels2d )
  {
    m_event = event;
    m_username = username;
    m_dbSRID = dbSRID;
    m_waterlevels2d = waterlevels2d;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "SaveEventOperation_0" ); //$NON-NLS-1$
  }

  @Override
  public IStatus execute( final Session session, final IProgressMonitor monitor ) throws PdbConnectException
  {
    monitor.beginTask( getLabel(), 100 );

    /* Fetch coefficients, we need to known the unknown classes */
    final ICoefficients coefficients = new Coefficients( session, IGafConstants.POINT_KIND_GAF );

    /* Prepare event for save */
    final Date now = new Date();
    m_event.setCreationDate( now );
    m_event.setEditingDate( now );
    m_event.setEditingUser( m_username );

    /* build 2d waterlevels */
    final Set<WaterlevelFixation> waterlevels = m_event.getWaterlevelFixations();

    /* update wl_type */
    final SubProgressMonitor subMonitor = new SubProgressMonitor( monitor, 20 );
    subMonitor.beginTask( Messages.getString( "SaveEventOperation_1" ), 100 ); //$NON-NLS-1$

    addFixations();

    /* save changed event */
    session.save( m_event );

    /* save fixations */
    saveFixations( session, waterlevels, now );

    /* save 2d waterlevels */
    saveWaterlevels2D( session, coefficients );

    subMonitor.done();

    return Status.OK_STATUS;
  }

  private void addFixations( )
  {
    /* no fixations for 2d waterlevels */
    if( m_event.getWlType() == WL_TYPE.WL_2D )
      return;

    /* we already have some fixations, do not change */
    final Set<WaterlevelFixation> waterlevelFixations = m_event.getWaterlevelFixations();
    if( waterlevelFixations.size() > 0 )
      return;

    /* create fixations */
    for( final WaterlevelsForStation waterlevels : m_waterlevels2d )
    {
      final WaterlevelFixation[] fixations = waterlevels.getFixations();
      waterlevelFixations.addAll( Arrays.asList( fixations ) );
    }
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

  private void saveWaterlevels2D( final Session session, final ICoefficients coefficients ) throws PdbConnectException
  {
    try
    {
      if( m_waterlevels2d == null || m_waterlevels2d.length == 0 )
        return;

      final CrossSectionPartTypes partTypes = new CrossSectionPartTypes( session );

      for( final WaterlevelsForStation waterlevel2d : m_waterlevels2d )
      {
        final IProfileObject[] waterlevels = waterlevel2d.getWaterlevelObjects();
        final ISectionProvider sectionProvider = waterlevel2d.getSection();
        if( sectionProvider == null )
          continue;

        final CrossSection section = sectionProvider.getSection();
        final int profileSRID = section.getLine().getSRID();
        final BigDecimal station = sectionProvider.getStation();

        for( final IProfileObject waterlevel : waterlevels )
        {
          final CheckinHorizonPartOperation operation = new CheckinHorizonPartOperation( waterlevel, profileSRID, m_dbSRID, station.doubleValue(), partTypes, m_event, coefficients );
          operation.execute();

          final CrossSectionPart part = operation.getPart();
          part.setCrossSection( section );
          session.save( part );

          final Set<Point> points = part.getPoints();
          for( final Point point : points )
            session.save( point );
        }
      }
    }
    catch( final Exception e )
    {
      throw new PdbConnectException( "Failed to transform to db coordinate system", e ); //$NON-NLS-1$
    }
  }

  public static boolean askForEmptyState( final Event event, final Shell shell, final String dialogTitle )
  {
    if( event.getState() != null )
      return true;

    final String message = Messages.getString( "SaveEventOperation_2" ); //$NON-NLS-1$
    return MessageDialog.openConfirm( shell, dialogTitle, message );
  }
}