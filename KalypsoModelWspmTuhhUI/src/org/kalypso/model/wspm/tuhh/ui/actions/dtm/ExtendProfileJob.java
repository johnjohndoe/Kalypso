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
package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import java.awt.Graphics;
import java.awt.Point;
import java.net.URL;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.util.ExtendProfileTransaction;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.coverage.RichCoverageCollection;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeaturesChangedModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class ExtendProfileJob extends AbstractDemProfileJob
{
  /** Snapping radius in screen-pixels. */
  public static final int SNAPPING_RADIUS = 20;

  private int m_insertSign = 0;

  private final double m_digitalizeDistance;

  private final SLDPainter2 m_profileLinePainter;

  private final SLDPainter2 m_grabPointPainter;

  private GM_Point m_startPoint;

  private ExtendProfileGrabber m_info;

  public ExtendProfileJob( final CreateProfileFromDEMWidget widget, final IMapPanel mapPanel, final ICoverageCollection coverages, final double simplifyDistance, final double digitalizeDistance, final IKalypsoFeatureTheme[] profileThemes )
  {
    super( Messages.getString( "ExtendProfileJob_0" ), widget, mapPanel, coverages, simplifyDistance, profileThemes ); //$NON-NLS-1$

    m_digitalizeDistance = digitalizeDistance;
    m_profileLinePainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/selected.profile.sld" ) } ); //$NON-NLS-1$
    m_grabPointPainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/selected.point.sld" ) } ); //$NON-NLS-1$
  }

  @Override
  protected IStatus runJob( final GM_Curve curve, final RichCoverageCollection richCoverages ) throws Exception
  {
    /* Need the info. */
    if( m_info == null )
      return Status.OK_STATUS;

    /* Fetch points from DTM/TIN. */
    final Coordinate[] newPoints = fetchPoints( curve, richCoverages );
    if( ArrayUtils.isEmpty( newPoints ) )
      return openNoPointsWarning();

    /* Add line into profile. */
    final IProfileFeature profile = m_info.getProfile();
    final IProfile profil = profile.getProfile();

    profil.doTransaction( new ExtendProfileTransaction( newPoints, curve.getCoordinateSystem(), m_insertSign, getSimplifyDistance() ) );

    final Feature profileOwner = m_info.getProfileOwner();
    if( profileOwner != null )
    {
      /* If the reach is not null, we have a reach theme that does not get updated by the event on the profile itself. */
      /* We need to fire an event on the reach itself. */
      final GMLWorkspace workspace = profileOwner.getWorkspace();
      workspace.fireModellEvent( new FeaturesChangedModellEvent( workspace, new Feature[] { profileOwner } ) );
    }

    /* Make the workspace dirty. */
    m_info.getWorkspace().postCommand( new EmptyCommand( "", false ) ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }

  private Coordinate[] fetchPoints( final GM_Curve curve, final RichCoverageCollection richCoverages )
  {
    final Coordinate[] newPoints = richCoverages.extractPoints( curve, m_digitalizeDistance );
    richCoverages.dispose();
    return newPoints;
  }

  @Override
  public String getLabel( )
  {
    return Messages.getString( "ExtendProfileJob_4" ); //$NON-NLS-1$
  }

  private GM_Point grabProfileEnd( final GM_Point pos )
  {
    /* Try to grab a feature. */
    final double snapRadius = MapUtilities.calculateWorldDistance( getMapPanel(), pos, SNAPPING_RADIUS );
    m_insertSign = 0;
    m_info = grabProfile( pos, snapRadius );
    if( m_info == null )
      return null;

    final IProfileFeature profile = m_info.getProfile();
    final GM_Curve line = profile.getLine();
    if( line == null )
      return null;

    final GM_Point startPoint = line.getStartPoint();
    final GM_Point endPoint = line.getEndPoint();
    final double startDistance = pos.distance( startPoint );
    final double endDistance = pos.distance( endPoint );
    if( startDistance < endDistance && startDistance < snapRadius )
    {
      m_insertSign = -1;
      return startPoint;
    }
    else if( endDistance < snapRadius )
    {
      m_insertSign = 1;
      return endPoint;
    }
    else
      return null;
  }

  private ExtendProfileGrabber grabProfile( final GM_Point pos, final double snapRadius )
  {
    final IKalypsoFeatureTheme[] profileThemes = getProfileThemes();
    for( final IKalypsoFeatureTheme theme : profileThemes )
    {
      final ExtendProfileGrabber info = new ExtendProfileGrabber( theme, snapRadius );
      if( info.doGrab( pos ) )
        return info;
    }

    return null;
  }

  @Override
  public void paint( final Graphics g, final IMapPanel mapPanel, final Point currentPoint )
  {
    super.paint( g, mapPanel, currentPoint );

    final GeoTransform projection = mapPanel.getProjection();

    if( m_info != null )
    {
      final IProfileFeature profile = m_info.getProfile();
      final GM_Curve line = profile.getLine();
      m_profileLinePainter.paint( g, projection, line );
    }

    final LineGeometryBuilder geoBuilder = getGeoBuilder();
    if( geoBuilder == null || currentPoint == null )
      return;

    final int pointCount = geoBuilder.getPointCount();
    final GM_Point currentPos = MapUtilities.transform( mapPanel, currentPoint );
    final GM_Point grabPoint;
    if( pointCount == 0 )
      grabPoint = grabProfileEnd( currentPos );
    else
      grabPoint = m_startPoint;

    // Paint grabbed point: the first one we got....
    if( grabPoint != null )
      m_grabPointPainter.paint( g, projection, grabPoint );
  }

  @Override
  public void addPoint( final GM_Point pos ) throws Exception
  {
    final LineGeometryBuilder geoBuilder = getGeoBuilder();
    final int pointCount = geoBuilder.getPointCount();
    if( pointCount != 0 )
    {
      geoBuilder.addPoint( pos );
      return;
    }

    final GM_Point grabProfileEnd = grabProfileEnd( pos );
    if( grabProfileEnd != null )
    {
      m_startPoint = grabProfileEnd;
      geoBuilder.addPoint( grabProfileEnd );
    }
  }

  @Override
  public void removeLastPoint( )
  {
    final LineGeometryBuilder geoBuilder = getGeoBuilder();
    geoBuilder.removeLastPoint();
    if( geoBuilder.getPointCount() == 0 )
    {
      m_info = null;
      m_startPoint = null;
    }
  }
}