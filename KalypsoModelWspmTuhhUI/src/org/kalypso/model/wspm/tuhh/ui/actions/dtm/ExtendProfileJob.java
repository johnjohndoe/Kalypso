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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.commons.command.EmptyCommand;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.grid.RichCoverageCollection;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.coverages.CoverageProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileUiUtils;
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
public class ExtendProfileJob extends UIJob implements ICreateProfileStrategy
{
  /** Snapping radius in screen-pixels. */
  public static final int SNAPPING_RADIUS = 20;

  private final CreateProfileFromDEMWidget m_widget;

  private final ICoverageCollection m_coverages;

  private final IMapPanel m_mapPanel;

  private final FeatureList m_profileFeatures;

  private IProfileFeature m_profile;

  private int m_insertSign = 0;

  private final double m_simplifyDistance;

  private final TuhhReach m_reach;

  private final SLDPainter2 m_profileLinePainter;

  private final SLDPainter2 m_grabPointPainter;

  private LineGeometryBuilder m_geoBuilder;

  private GM_Point m_startPoint;

  private final CommandableWorkspace m_commandableWorkspace;

  public ExtendProfileJob( final CreateProfileFromDEMWidget widget, final CommandableWorkspace commandableWorkspace, final IMapPanel mapPanel, final ICoverageCollection coverages, final FeatureList profileFeatures, final TuhhReach reach, final double simplifyDistance )
  {
    super( "Extend Profile" );
    m_widget = widget;
    m_commandableWorkspace = commandableWorkspace;
    m_mapPanel = mapPanel;
    m_coverages = coverages;
    m_profileFeatures = profileFeatures;
    m_reach = reach;
    m_simplifyDistance = simplifyDistance;

    m_profileLinePainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/selected.profile.sld" ) } );
    m_grabPointPainter = new SLDPainter2( new URL[] { getClass().getResource( "resources/selected.point.sld" ) } );
    m_geoBuilder = new LineGeometryBuilder( 0, mapPanel.getMapModell().getCoordinatesSystem() );
  }

  @Override
  public void dispose( )
  {
    if( m_geoBuilder != null )
    {
      m_geoBuilder.reset();
      m_geoBuilder = null;
    }
  }

  /**
   * @see org.eclipse.ui.progress.UIJob#runInUIThread(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus runInUIThread( final IProgressMonitor monitor )
  {
    try
    {
      if( m_profile == null )
        return Status.OK_STATUS;

      // remove last point: as we are using leftPressed, we always get two point on double clicks
      m_geoBuilder.removeLastPoint();
      final GM_Curve curve = (GM_Curve) m_geoBuilder.finish();

      // fetch points from DTM
      final RichCoverageCollection richCoverages = new RichCoverageCollection( m_coverages );
      final Coordinate[] newPoints = richCoverages.extractPoints( curve );
      richCoverages.dispose();
      if( newPoints == null )
      {
        // TODO: better message
        return Status.OK_STATUS;
      }

      // add line into profile
      final IProfil profil = m_profile.getProfil();
      CoverageProfile.extendPoints( profil, m_insertSign, newPoints, m_simplifyDistance );

      ProfileUiUtils.changeProfileAndFireEvent( profil, m_profile );
      if( m_reach != null )
      {
        /*
         * If the reach is not null, we have a reach theme that does not get updated by the event on the profile itself.
         * We need to fire an event on the reach itself.
         */
        final GMLWorkspace workspace = m_reach.getWorkspace();
        workspace.fireModellEvent( new FeatureStructureChangeModellEvent( workspace, m_reach, (Feature[]) null, FeatureStructureChangeModellEvent.STRUCTURE_CHANGE_ADD ) );
      }

      m_commandableWorkspace.postCommand( new EmptyCommand( "", false ) );

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), "Failed to extend profile from terrain model." );
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return "Extend profile";
  }

  private GM_Point grabProfileEnd( final GM_Point pos )
  {
    /* Try to grab a feature. */
    final double snapRadius = MapUtilities.calculateWorldDistance( m_mapPanel, pos, SNAPPING_RADIUS );
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
    if( GMLSchemaUtilities.substitutes( targetFeatureType, IProfileFeature.QNAME_PROFILE ) )
      return (IProfileFeature) GeometryUtilities.findNearestFeature( pos, snapRadius, m_profileFeatures, IProfileFeature.QNAME_LINE );
    else
    {
      final TuhhReachProfileSegment nearest = (TuhhReachProfileSegment) GeometryUtilities.findNearestFeature( pos, snapRadius, m_profileFeatures, TuhhReachProfileSegment.PROPERTY_PROFILE_LOCATION );
      if( nearest == null )
        return null;

      return nearest.getProfileMember();
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#run()
   */
  @Override
  public void run( )
  {
    final CreateProfileFromDEMWidget widget = m_widget;
    final IMapPanel mapPanel = m_mapPanel;

    final JobChangeAdapter listener = new JobChangeAdapter()
    {
      /**
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent event )
      {
        widget.activate( null, mapPanel );
        ExtendProfileJob.this.removeJobChangeListener( this );
      }
    };

    addJobChangeListener( listener );

    schedule();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#paint(java.awt.Graphics,
   *      org.kalypso.ogc.gml.map.IMapPanel, java.awt.Point)
   */
  @Override
  public void paint( final Graphics g, final IMapPanel mapPanel, final Point currentPoint )
  {
    if( currentPoint == null || m_geoBuilder == null )
      return;

    final GM_Point currentPos = MapUtilities.transform( mapPanel, currentPoint );
    final GeoTransform projection = mapPanel.getProjection();

    m_geoBuilder.paint( g, projection, currentPoint );

    if( m_profile != null )
    {
      final GM_Curve line = m_profile.getLine();
      m_profileLinePainter.paint( g, projection, line );
    }

    final int pointCount = m_geoBuilder.getPointCount();
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
    final int pointCount = m_geoBuilder.getPointCount();
    if( pointCount != 0 )
    {
      m_geoBuilder.addPoint( pos );
      return;
    }

    final GM_Point grabProfileEnd = grabProfileEnd( pos );
    if( grabProfileEnd != null )
    {
      m_startPoint = grabProfileEnd;
      m_geoBuilder.addPoint( grabProfileEnd );
    }
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#removeLastPoint()
   */
  @Override
  public void removeLastPoint( )
  {
    m_geoBuilder.removeLastPoint();
    if( m_geoBuilder.getPointCount() == 0 )
    {
      m_profile = null;
      m_startPoint = null;
    }
  }
}
