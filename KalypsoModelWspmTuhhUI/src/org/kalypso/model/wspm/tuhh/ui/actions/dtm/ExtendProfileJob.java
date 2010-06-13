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

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.gml.coverages.CoverageProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReachProfileSegment;
import org.kalypso.model.wspm.tuhh.ui.actions.ProfileUiUtils;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.MapUtilities;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.FeatureList;
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

  public ExtendProfileJob( final CreateProfileFromDEMWidget widget, final IMapPanel mapPanel, final ICoverageCollection coverages, final FeatureList profileFeatures )
  {
    super( "Extend Profile" );
    m_widget = widget;
    m_mapPanel = mapPanel;
    m_coverages = coverages;
    m_profileFeatures = profileFeatures;
  }

  /**
   * @see org.eclipse.ui.progress.UIJob#runInUIThread(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus runInUIThread( final IProgressMonitor monitor )
  {
    if( m_profile == null )
      return Status.OK_STATUS;

    // fetch points from DTM
    final CoverageProfile coverageProfile = new CoverageProfile( m_coverages, null );
    final Coordinate[] newPoints = coverageProfile.extractPoints( m_profile.getLine() );
    if( newPoints == null )
    {
      // TODO: better message
      return Status.OK_STATUS;
    }

    // add line into profile
    final IProfil profil = m_profile.getProfil();
    coverageProfile.insertPoints( profil, m_insertSign, newPoints );

    ProfileUiUtils.changeProfileAndFireEvent( profil, m_profile );

    return Status.OK_STATUS;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return "Extend profile";
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#adjustPoint(org.kalypsodeegree.model.geometry.GM_Point,
   *      int)
   */
  @Override
  public GM_Point adjustPoint( final GM_Point pos, final int pointCount )
  {
    if( pointCount != 0 )
      return pos;

    // search profile (and remember it) and snap to end-point
    return grabProfileEnd( pos );
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
   *      org.kalypsodeegree.graphics.transformation.GeoTransform, org.kalypsodeegree.model.geometry.GM_Point)
   */
  @Override
  public void paint( final Graphics g, final GeoTransform projection, final GM_Point currentPos )
  {
// final GM_Point grabPoint = grabProfileEnd( currentPos );

    // TODO implement

    // paint profile
    if( m_profile != null )
    {
      final GM_Curve line = m_profile.getLine();

    }

    // paint grabbed point: the first one we got....
// if( grabPoint != null )
    {

    }

  }
}
