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

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.grid.RichCoverageCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.coverages.CoverageProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileUiUtils;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypso.ogc.gml.map.widgets.advanced.utils.SLDPainter2;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.event.FeatureStructureChangeModellEvent;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Coordinate;

/**
 * @author Gernot Belger
 */
public class ExtendProfileJob extends AbstractDemProfileJob
{
  /** Snapping radius in screen-pixels. */
  public static final int SNAPPING_RADIUS = 20;

  private final FeatureList m_profileFeatures;

  private IProfileFeature m_profile;

  private int m_insertSign = 0;

  private final SLDPainter2 m_profileLinePainter;

  private final SLDPainter2 m_grabPointPainter;

  private GM_Point m_startPoint;

  public ExtendProfileJob( final CreateProfileFromDEMWidget widget, final CommandableWorkspace commandableWorkspace, final IMapPanel mapPanel, final ICoverageCollection coverages, final FeatureList profileFeatures, final TuhhReach reach, final double simplifyDistance )
  {
    super( Messages.getString("ExtendProfileJob_0"), widget, commandableWorkspace, mapPanel, reach, coverages, simplifyDistance ); //$NON-NLS-1$

    m_profileFeatures = profileFeatures;

    m_profileLinePainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/selected.profile.sld" ) } ); //$NON-NLS-1$
    m_grabPointPainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/selected.point.sld" ) } ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.AbstractDemProfileJob#runJob(org.kalypsodeegree.model.geometry.GM_Curve,
   *      org.kalypso.grid.RichCoverageCollection)
   */
  @Override
  protected IStatus runJob( final GM_Curve curve, final RichCoverageCollection richCoverages ) throws Exception
  {
    if( m_profile == null )
      return Status.OK_STATUS;

    // fetch points from DTM
    final Coordinate[] newPoints = richCoverages.extractPoints( curve );
    richCoverages.dispose();
    if( newPoints == null )
    {
      // TODO: better message
      return Status.OK_STATUS;
    }

    // add line into profile
    final IProfil profil = m_profile.getProfil();
    CoverageProfile.extendPoints( profil, m_insertSign, newPoints, getSimplifyDistance() );

    ProfileUiUtils.changeProfileAndFireEvent( profil, m_profile );

    final TuhhReach reach = getReach();
    if( reach != null )
    {
      /*
       * If the reach is not null, we have a reach theme that does not get updated by the event on the profile itself.
       * We need to fire an event on the reach itself.
       */
      final GMLWorkspace workspace = reach.getWorkspace();
      workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, reach, (Feature[]) null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
    }

    getWorkspace().postCommand( new EmptyCommand( "", false ) ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return Messages.getString("ExtendProfileJob_4"); //$NON-NLS-1$
  }

  private GM_Point grabProfileEnd( final GM_Point pos )
  {
    /* Try to grab a feature. */
    final double snapRadius = MapUtilities.calculateWorldDistance( getMapPanel(), pos, SNAPPING_RADIUS );
    m_insertSign = 0;
    m_profile = grabProfile( pos, snapRadius );
    if( m_profile == null )
      return null;

    final GM_Curve line = m_profile.getLine();
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

  private IProfileFeature grabProfile( final GM_Point pos, final double snapRadius )
  {
    final IFeatureType targetFeatureType = m_profileFeatures.getParentFeatureTypeProperty().getTargetFeatureType();
    if( GMLSchemaUtilities.substitutes( targetFeatureType, IProfileFeature.QN_PROFILE ) )
      return (IProfileFeature) GeometryUtilities.findNearestFeature( pos, snapRadius, m_profileFeatures, IProfileFeature.QN_PROPERTY_LINE );
    else
    {
      final TuhhReachProfileSegment nearest = (TuhhReachProfileSegment) GeometryUtilities.findNearestFeature( pos, snapRadius, m_profileFeatures, TuhhReachProfileSegment.PROPERTY_PROFILE_LOCATION );
      if( nearest == null )
        return null;

      return nearest.getProfileMember();
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#paint(java.awt.Graphics,
   *      org.kalypso.ogc.gml.map.IMapPanel, java.awt.Point)
   */
  @Override
  public void paint( final Graphics g, final IMapPanel mapPanel, final Point currentPoint )
  {
    super.paint( g, mapPanel, currentPoint );

    final GeoTransform projection = mapPanel.getProjection();

    if( m_profile != null )
    {
      final GM_Curve line = m_profile.getLine();
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

    // paint grabbed point: the first one we got....
    if( grabPoint != null )
      m_grabPointPainter.paint( g, projection, grabPoint );
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#addPoint(org.kalypsodeegree.model.geometry.GM_Point)
   */
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

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#removeLastPoint()
   */
  @Override
  public void removeLastPoint( )
  {
    final LineGeometryBuilder geoBuilder = getGeoBuilder();
    geoBuilder.removeLastPoint();
    if( geoBuilder.getPointCount() == 0 )
    {
      m_profile = null;
      m_startPoint = null;
    }
  }
}
