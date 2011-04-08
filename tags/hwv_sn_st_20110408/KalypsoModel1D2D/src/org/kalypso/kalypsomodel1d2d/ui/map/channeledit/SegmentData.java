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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.kalypso.jts.JTSUtilities;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.PROF;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.SIDE;
import org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.WIDTHORDER;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.KalypsoModelWspmCoreExtensions;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilPointPropertyProvider;
import org.kalypso.model.wspm.core.profil.ProfilFactory;
import org.kalypso.model.wspm.core.profil.util.DouglasPeuckerHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilComparator;
import org.kalypso.model.wspm.core.profil.util.ProfilObsHelper;
import org.kalypso.model.wspm.core.profil.util.ProfilUtil;
import org.kalypso.model.wspm.core.util.WspmProfileHelper;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.transformation.transformer.GeoTransformerFactory;
import org.kalypso.transformation.transformer.IGeoTransformer;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.Geometry;
import com.vividsolutions.jts.geom.GeometryCollection;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * @author Thomas Jung
 */
public class SegmentData
{
  // LineStrings derived from the original data
  private LineString m_profUpOrg;

  private LineString m_profDownOrg;

  private LineString m_bankLeftOrg;

  private LineString m_bankRightOrg;

  /* intersected lines */
  private LineString m_upIntersLinestring;

  private LineString m_downIntersLinestring;

  private LineString m_bankLeftInters;

  private LineString m_bankRightInters;

  private IProfil m_upIntersProfile;

  private IProfil m_downCroppedProfile;

  private IProfil m_downIntersProfile;

  /* intersection data */
  private final List<IntersPointData> m_intersPoints = new ArrayList<IntersPointData>();

  private int m_numBankIntersections;

  // original data
  private final Map<GM_Curve, SIDE> m_bankLines;

  private final IProfileFeature m_upProfile;

  private final IProfileFeature m_DownProfile;

  private LineString m_upProfLineString;

  private LineString m_downProfLineString;

  private final CreateChannelData m_channelData;

  private IProfil m_upCroppedProfile;

  /* areas of the profiles */
  private double m_areaUpCroppedProfile;

  private double m_areaDownCroppedProfile;

  private double m_areaUpIntersProfile;

  private double m_areaDownIntersProfile;

  public SegmentData( final CreateChannelData channelData, final IProfileFeature upProfile, final IProfileFeature downProfile, final Map<GM_Curve, SIDE> banks, final int numBankIntersections )
  {
    m_channelData = channelData;
    m_upProfile = upProfile;
    m_DownProfile = downProfile;
    m_bankLines = banks;

    m_upProfLineString = convertProfilesToLineStrings( upProfile );
    m_downProfLineString = convertProfilesToLineStrings( downProfile );
    m_numBankIntersections = numBankIntersections;

    intersectOrigBanks();
    m_bankLeftInters = intersectLineString( m_bankLeftOrg, m_numBankIntersections );
    m_bankRightInters = intersectLineString( m_bankRightOrg, m_numBankIntersections );

    if( m_bankLeftOrg != null & m_bankRightOrg != null )
      intersectOrigProfiles();

  }

  /**
   * GETTERS
   */
  public LineString getBankLeftOrg( )
  {
    return m_bankLeftOrg;
  }

  public LineString getBankRightOrg( )
  {
    return m_bankRightOrg;
  }

  public LineString getBankLeftInters( )
  {
    return m_bankLeftInters;
  }

  public LineString getBankRightInters( )
  {
    return m_bankRightInters;
  }

  public LineString getProfDownOrg( )
  {
    return m_profDownOrg;
  }

  public LineString getProfUpOrg( )
  {
    return m_profUpOrg;
  }

  public LineString getProfDownIntersLineString( )
  {
    return m_downIntersLinestring;
  }

  public LineString getProfUpIntersLineString( )
  {
    return m_upIntersLinestring;
  }

  public IProfil getProfilUpOrg( )
  {
    return m_upProfile.getProfil();
  }

  public IProfil getProfilDownOrg( )
  {
    return m_DownProfile.getProfil();
  }

  public IProfil getProfUpIntersProfile( )
  {
    return m_upIntersProfile;
  }

  public IProfil getProfDownIntersProfile( )
  {
    return m_downIntersProfile;
  }

  /**
   * Intersects the two WSPM profiles (upstream/downstream) with the allready intersected bank lines (left/right) of the
   * current segment.<br>
   * The two profiles will be cropped at the intersection points and intersected by a specific number of points. <br>
   * Afterwards an area adjustment for the intersected profiles will be done .
   */
  private void intersectOrigProfiles( )
  {
    /* get the cropped and intersected profiles & linestrings */

    // UPSTREAM
    try
    {
      /* crop the profile */
      // IProfil
      m_upCroppedProfile = createCroppedIProfile( m_upProfile, CreateChannelData.PROF.UP );
      // LineString
      m_upProfLineString = createCroppedProfileLineString( m_upProfile, CreateChannelData.PROF.UP );

      /* intersect the cropped profile */
      final IProfil tempUpIntersProfile = createIntersectedIProfile( m_upCroppedProfile );

      /*
       * area adjustment
       */

      /* the cropped profile area is the desired value for the intersected profile area */
      m_areaUpCroppedProfile = ProfilUtil.calcArea( m_upCroppedProfile );
      m_areaUpIntersProfile = ProfilUtil.calcArea( tempUpIntersProfile );

      m_upIntersProfile = adjustProfileArea( tempUpIntersProfile, m_areaUpCroppedProfile, m_areaUpIntersProfile );

      final GeometryFactory factory = new GeometryFactory();
      m_upIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // DOWNSTREAM
    try
    {
      /* crop the profile */
      // LineString
      m_downProfLineString = createCroppedProfileLineString( m_DownProfile, CreateChannelData.PROF.DOWN );
      // IProfil
      m_downCroppedProfile = createCroppedIProfile( m_DownProfile, CreateChannelData.PROF.DOWN );

      /* intersect the cropped profile */
      final IProfil tempDownIntersProfile = createIntersectedIProfile( m_downCroppedProfile );

      /*
       * area adjustment
       */

      /* the cropped profile area is the desired value for the intersected profile area */
      m_areaDownCroppedProfile = ProfilUtil.calcArea( m_downCroppedProfile );
      m_areaDownIntersProfile = ProfilUtil.calcArea( tempDownIntersProfile );

      m_downIntersProfile = adjustProfileArea( tempDownIntersProfile, m_areaDownCroppedProfile, m_areaDownIntersProfile );

      final GeometryFactory factory = new GeometryFactory();
      m_downIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * changes the node elevations of the inner points of an IProfil so that its crosssection area (currentArea) is the
   * same as the area of the cropped original IProfil (targetArea)
   * 
   * @param profile
   *          intersected profile for which the area adjustment shall be done
   * @param targetArea
   *          area of the original cropped profile
   * @param currentArea
   *          current area of the intersected profile
   */
  private IProfil adjustProfileArea( final IProfil profile, final double targetArea, final double currentArea )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );

    final IRecord[] points = profile.getPoints();
    final int numProfPoints = points.length;
    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );

    IProfil tmpProfile2 = null;

    tmpProfil.setStation( profile.getStation() );
    if( m_downCroppedProfile != null & m_upCroppedProfile != null )
    {
      if( profile.getStation() == m_downCroppedProfile.getStation() )
        tmpProfile2 = m_downCroppedProfile;
      else
        tmpProfile2 = m_upCroppedProfile;
    }
    else if( m_downCroppedProfile == null )
      tmpProfile2 = m_upCroppedProfile;
    else
      tmpProfile2 = m_downCroppedProfile;

    final IRecord[] profilPointList = points;

    /* get / create components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent rwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );

    /* add components to new profile */
    tmpProfil.addPointProperty( breiteComponent );
    tmpProfil.addPointProperty( hoeheComponent );
    tmpProfil.addPointProperty( rwComponent );
    tmpProfil.addPointProperty( hwComponent );

    // calculate the area difference
    final double dArea = targetArea - currentArea;

    double dZ = 0;
    double wi = 0;

    // calculate the width of the the first and last segment and devide it by two (because of the triangle area of these
    // parts)

    final TupleResult owner = points[1].getOwner();

    final int indexBreite = owner.indexOfComponent( breiteComponent );
    final int indexHoehe = owner.indexOfComponent( hoeheComponent );
    final int indexRw = owner.indexOfComponent( rwComponent );
    final int indexHw = owner.indexOfComponent( hwComponent );

    final double startSegmentWidth = (Double) points[1].getValue( indexBreite ) - (Double) points[0].getValue( indexBreite );
    final double endSegmentWidth = (Double) points[numProfPoints - 1].getValue( indexBreite ) - (Double) points[numProfPoints - 2].getValue( indexBreite );

    wi = 0.5 * (startSegmentWidth + endSegmentWidth);
    // add the width of the segments inbetween
    for( int i = 1; i < numProfPoints - 2; i++ )
      wi = wi + (Double) points[i + 1].getValue( indexBreite ) - (Double) points[i].getValue( indexBreite );
    dZ = dArea / wi;

    // start point will not be changed
    final IRecord record = profilPointList[0];
    final TupleResult result = record.getOwner();
    final double startBreite = (Double) record.getValue( result.indexOfComponent( breiteComponent ) );
    final double startHoehe = (Double) record.getValue( result.indexOfComponent( hoeheComponent ) );
    final double startRw = (Double) record.getValue( result.indexOfComponent( rwComponent ) );
    final double startHw = (Double) record.getValue( result.indexOfComponent( hwComponent ) );

    final IRecord profilStartPoint = tmpProfil.createProfilPoint();
    
    //due the owner of the Record is null and here is only the index of components needed, this index can be resolved from Record self
    //this change is done for whole file 
//    final TupleResult resultStartPoint = profilStartPoint.getOwner();

    profilStartPoint.setValue( profilStartPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), startBreite );
    profilStartPoint.setValue( profilStartPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), startHoehe );
    profilStartPoint.setValue( profilStartPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), startRw );
    profilStartPoint.setValue( profilStartPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), startHw );

    tmpProfil.addPoint( profilStartPoint );

    // handle the points inbetween
    for( int i = 1; i < numProfPoints - 1; i++ )
    {
      final double width = (Double) points[i].getValue( indexBreite );
      double heigth = 0;
      try
      {
        heigth = WspmProfileHelper.getHeightByWidth( width, tmpProfile2 ) - dZ;
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      final double x = (Double) points[i].getValue( indexRw );
      final double y = (Double) points[i].getValue( indexHw );

      final IRecord point = tmpProfil.createProfilPoint();

      point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), width );
      point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth );
      point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), x );
      point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), y );

      tmpProfil.addPoint( point );
    }

    final IRecord profilEndPoint = tmpProfil.createProfilPoint();

    // end point will be the same
    final double endBreite = (Double) profilPointList[profilPointList.length - 1].getValue( indexBreite );
    final double endHoehe = (Double) profilPointList[profilPointList.length - 1].getValue( indexHoehe );
    final double endRw = (Double) profilPointList[profilPointList.length - 1].getValue( indexRw );
    final double endHw = (Double) profilPointList[profilPointList.length - 1].getValue( indexHw );

    profilEndPoint.setValue( profilEndPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), endBreite );
    profilEndPoint.setValue( profilEndPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), endHoehe );
    profilEndPoint.setValue( profilEndPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), endRw );
    profilEndPoint.setValue( profilEndPoint.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), endHw );

    tmpProfil.addPoint( profilEndPoint );

    // debugging and testing:
    final double areaNew = ProfilUtil.calcArea( tmpProfil );
    final double diffArea = targetArea - areaNew;
    if( diffArea > 0.10 )
    {
      // String s = String.format( "Schlauchgenerator: Fl�chenausgleich nicht hinreichend genau: %f - %f", targetArea,
      // areaNew
      // );

      // System.out.println( s );
    }

    return tmpProfil;
  }

  /**
   * initial intersection of an IProfil. There are two ways of intersection:<br>
   * - if there are more profile points than the wished number of intersection points, the intersection is done by
   * Douglas-Peucker<br>
   * - if there are not enough profile points, the intersection is done with an equidistant approach.
   * 
   * @param profile
   *          input profile to be intersected.
   */
  private IProfil createIntersectedIProfile( final IProfil profile ) throws Exception
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( profile.getType() );

    final int numProfInters = m_channelData.getNumProfileIntersections();
    final IRecord[] profilPointList = profile.getPoints();
    final int numProfPoints = profilPointList.length;

    final IProfil tmpProfil = ProfilFactory.createProfil( profile.getType() );

    /* get / create components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent rwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );

    /* add components to new profile */
    tmpProfil.addPointProperty( breiteComponent );
    tmpProfil.addPointProperty( hoeheComponent );
    tmpProfil.addPointProperty( rwComponent );
    tmpProfil.addPointProperty( hwComponent );

    tmpProfil.setStation( profile.getStation() );

    if( numProfInters > numProfPoints )
    {
      // start point

      /* get values */
      final IRecord pFirst = profilPointList[0];

      double width = (Double) pFirst.getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );
      double heigth = (Double) pFirst.getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      double x = (Double) pFirst.getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      double y = (Double) pFirst.getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );

      /* set values */
      IRecord pointRecord = tmpProfil.createProfilPoint();

      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), width );
      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth );
      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), x );
      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), y );

      tmpProfil.addPoint( pointRecord );

      /* do it by equidistant points */
      // keep in mind, that equidistant widths doesn't get equidistant georeferenced lengths!
      final double startWidth = (Double) profilPointList[0].getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double endWidth = (Double) profilPointList[profilPointList.length - 1].getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );
      final double totalWidth = endWidth - startWidth;
      final double dWidth = totalWidth / (m_channelData.getNumProfileIntersections() - 1); // equidistant widths

      for( int i = 1; i < m_channelData.getNumProfileIntersections() - 1; i++ )
      {
        /* get values */
        width = startWidth + i * dWidth;
        heigth = WspmProfileHelper.getHeightByWidth( width, profile );
        final GM_Point geoPoint = WspmProfileHelper.getGeoPosition( width, profile );

        /* set values */
        pointRecord = tmpProfil.createProfilPoint();

        pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), width );
        pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth );
        pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), geoPoint.getX() );
        pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), geoPoint.getY() );

        tmpProfil.addPoint( pointRecord );
      }

      // end point

      /* get values */
      width = (Double) profilPointList[profilPointList.length - 1].getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );
      heigth = (Double) profilPointList[profilPointList.length - 1].getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ) );
      x = (Double) profilPointList[profilPointList.length - 1].getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
      y = (Double) profilPointList[profilPointList.length - 1].getValue( pFirst.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );

      /* set values */
      pointRecord = tmpProfil.createProfilPoint();

      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), width );
      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth );
      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), x );
      pointRecord.setValue( pointRecord.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), y );

      tmpProfil.addPoint( pointRecord );
    }
    else
    {
      // get the most important points (some kind of Douglas-Peucker)
      final IRecord[] profPoints = profilPointList;
      // do it by Douglas-Peucker
      final IRecord[] DPpoints = DouglasPeuckerHelper.findIProfileVIPPoints( profPoints, numProfInters );

      Arrays.sort( DPpoints, new ProfilComparator( breiteComponent ) );
      for( final IRecord element : DPpoints )
      {
        /* get values */

        final double width = (Double) element.getValue( element.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );
        final double heigth = (Double) element.getValue( element.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ) );
        final double x = (Double) element.getValue( element.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ) );
        final double y = (Double) element.getValue( element.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ) );

        /* set values */
        final IRecord point = tmpProfil.createProfilPoint();

        point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), width );
        point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth );
        point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), x );
        point.setValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), y );

        tmpProfil.addPoint( point );
      } 
    }
    return tmpProfil;
  }

  /**
   * intersects a specific linestring by a given number of intersections.<br>
   * the intersection is done by an equidistant approach. <BR>
   * this method is used for intersecting the banklines (Linestrings)
   * 
   * @param linestring
   *          input linestring to be intersected
   * @param numIntersects
   *          number of intersections of the linestring
   */
  private LineString intersectLineString( final LineString linestring, final int numIntersects )
  {
    if( linestring == null )
      return null;

    // calculate the distance between the intersection points by the distance along the profile linestring.
    final double totaldistance = linestring.getLength();

    // then compute the additional coodinates of the intersected profile linestring
    // by the given spinner data of the composite
    /* for now: equidistant points */
    final double dDist = totaldistance / (numIntersects - 1); // equidistant widths
    final double[] dist = new double[numIntersects];
    final Point[] points = new Point[numIntersects];

    dist[0] = 0;
    points[0] = linestring.getStartPoint();

    for( int i = 1; i < dist.length - 1; i++ )
    {
      dist[i] = dist[0] + dDist * i;
      points[i] = JTSUtilities.pointOnLine( linestring, dist[i] );
    }
    dist[numIntersects - 1] = totaldistance;
    points[numIntersects - 1] = linestring.getEndPoint();

    final GM_Point[] gmpoints = new GM_Point[numIntersects]; // points for the LineString
    final GeometryFactory factory = new GeometryFactory();
    final Coordinate[] coordinates = new Coordinate[points.length];
    for( int i = 0; i < gmpoints.length; i++ )
      try
      {
        gmpoints[i] = (GM_Point) JTSAdapter.wrap( points[i] );

        final double z = points[i].getCoordinate().z;

        if( !Double.isNaN( z ) )
          coordinates[i] = new Coordinate( gmpoints[i].getX(), gmpoints[i].getY(), gmpoints[i].getZ() );
        else
          coordinates[i] = new Coordinate( gmpoints[i].getX(), gmpoints[i].getY() );
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    return factory.createLineString( coordinates );
  }

  /**
   * calculates the intersection of a segment profile. at first, the width coordinates will be calculated and added to
   * the profile. next step is to intersect the profile <br>
   * (a) by Douglas-Peucker <br>
   * (b) equidistant. <br>
   * the geographical data is interpolated from the original profile data.
   * 
   * @param wspmprofile
   *          original profile (WSPMProfile) to be intersected
   * @param prof
   *          additional informations of the corresponding intersection points of that profile (upstream / downstream)
   */
  private IProfil createCroppedIProfile( final IProfileFeature wspmprofile, final CreateChannelData.PROF prof ) throws Exception
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( wspmprofile.getProfil().getType() );

    final double width1 = calcWidthCoord( wspmprofile, prof, CreateChannelData.WIDTHORDER.FIRST );
    final double width2 = calcWidthCoord( wspmprofile, prof, CreateChannelData.WIDTHORDER.LAST );

    final Point firstPoint = getIntersPoint( prof, CreateChannelData.WIDTHORDER.FIRST );
    final Point lastPoint = getIntersPoint( prof, CreateChannelData.WIDTHORDER.LAST );
    final double startWidth;
    final double endWidth;
    final Point geoPoint1;
    final Point geoPoint2;

    if( width1 > width2 )
    {
      startWidth = width2;
      endWidth = width1;

      geoPoint1 = lastPoint;
      geoPoint2 = firstPoint;
    }
    else
    {
      startWidth = width1;
      endWidth = width2;
      geoPoint1 = firstPoint;
      geoPoint2 = lastPoint;
    }

    // convert WSPM-Profil into IProfil an add the additional intersection width points.
    final IProfil orgIProfil = wspmprofile.getProfil();

    // calculate elevations
    final double heigth1 = WspmProfileHelper.getHeightByWidth( startWidth, orgIProfil );
    final double heigth2 = WspmProfileHelper.getHeightByWidth( endWidth, orgIProfil );

    final IRecord[] profilPointList = wspmprofile.getProfil().getPoints();

    /* get / create components */
    final IComponent breiteComponentOrig = ProfilObsHelper.getPropertyFromId( orgIProfil, IWspmConstants.POINT_PROPERTY_BREITE );

    final GM_Curve line = wspmprofile.getLine();

    final IProfil tmpProfil = ProfilFactory.createProfil( wspmprofile.getProfil().getType() );

    /* get / create components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent hwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );
    final IComponent rwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );

    /* add components to new profile */
    tmpProfil.addPointProperty( breiteComponent );
    tmpProfil.addPointProperty( hoeheComponent );
    tmpProfil.addPointProperty( hwComponent );
    tmpProfil.addPointProperty( rwComponent );
    final IRecord point1 = tmpProfil.createProfilPoint();
    final IRecord point2 = tmpProfil.createProfilPoint();

    /* calculate the width of the intersected profile */
    // sort intersection points by width
    point1.setValue( point1.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), startWidth );
    point1.setValue( point1.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth1 );
    point1.setValue( point1.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), geoPoint1.getX() );
    point1.setValue( point1.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), geoPoint1.getY() );

    point2.setValue( point2.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ), endWidth );
    point2.setValue( point2.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE ), heigth2 );
    point2.setValue( point2.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), geoPoint2.getX() );
    point2.setValue( point2.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT  ), geoPoint2.getY() );

    tmpProfil.addPoint( point1 );

    final GM_LineString lineString = line.getAsLineString(); // in the linestring the coordinates are already projected

    for( int i = 0; i < profilPointList.length; i++ )
    {
      final IRecord point = profilPointList[i];

      final double currentWidth = (Double) point.getValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );

      if( currentWidth > startWidth & currentWidth < endWidth )
      {
        final IRecord pt = tmpProfil.createProfilPoint();

        final IComponent[] components = orgIProfil.getPointProperties();
        for( final IComponent component : components )
        {
          if( tmpProfil.hasPointProperty( component ) )
          { 
            final Object object = point.getValue( point.indexOfComponent( component.getId() ) );
            pt.setValue( pt.indexOfComponent( component.getId() ), object );
          }
        }

        // set rw/hw to the projected coordinates
        pt.setValue( pt.indexOfComponent( IWspmConstants.POINT_PROPERTY_RECHTSWERT ), lineString.getPositionAt( i ).getX() );
        pt.setValue( pt.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOCHWERT ), lineString.getPositionAt( i ).getY() );

        tmpProfil.addPoint( pt );
      }
    }

    tmpProfil.addPoint( point2 );

    tmpProfil.setStation( orgIProfil.getStation() );
    return tmpProfil;
  }

  /**
   * gets the profile width coordinate of a specific intersection point corresponding to that profile
   * 
   * @param profile
   *          input profile
   * @param prof
   *          additional infos concerning the intersection point (profile (up/down))
   * @param widthorder
   *          additional infos concerning the intersection point (First/Last)
   */
  private double calcWidthCoord( final IProfileFeature profile, final CreateChannelData.PROF prof, final CreateChannelData.WIDTHORDER widthorder )
  {
    // get the intersection point for the profile
    final Point point = getIntersPoint( prof, widthorder );
    // compute the width coordinates of the intersection points
    try
    {
      final double width = WspmProfileHelper.getWidthPosition( (GM_Point) JTSAdapter.wrap( point ), profile.getProfil(), profile.getSrsName() );
      return width;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    return 0;
  }

  /**
   * crops the WSPM profile at the intersection points and converts it to a LineString
   * 
   * @param profile
   *          input profile to be cropped
   * @param prof
   *          additional infos about the intersection points (wether it is the upstream / downstream profile of the
   *          current segment)
   * @see JTSUtilities.createLineSegment
   */
  private LineString createCroppedProfileLineString( final IProfileFeature profile, final CreateChannelData.PROF prof ) throws GM_Exception
  {
    final LineString lsProfile = (LineString) JTSAdapter.export( profile.getLine() );
    final LineString intersProfile;
    final Point point1 = getIntersPoint( prof, CreateChannelData.SIDE.LEFT );
    final Point point2 = getIntersPoint( prof, CreateChannelData.SIDE.RIGHT );

    /* extract the LineStringSegment of the profile lying between the two corresponding intersection points */
    if( point1 != null & point2 != null )
    {
      intersProfile = JTSUtilities.createLineString( lsProfile, point1, point2 );
      return intersProfile;
    }
    else
      return null;
  }

  /**
   * Intersects the selected bank lines with the two profiles of the segment. This method will be called at the very
   * beginning. Here, the four intersection points will be initialized.
   */
  private void intersectOrigBanks( )
  {
    for( final Entry<GM_Curve, SIDE> bankEntry : m_bankLines.entrySet() )
    {
      final List<Point> intersPointList = new ArrayList<Point>();

      /* convert current bankLine in Curve */
      final GM_Curve bankCurve = bankEntry.getKey();
      final CreateChannelData.SIDE side = bankEntry.getValue();

      /* convert bank curve into LineString */
      try
      {
        final LineString bankLine = (LineString) JTSAdapter.export( bankCurve );

        /* intersect it with the first (previous) profile of this segment */
        final Geometry intersectionUpProfile = bankLine.intersection( m_upProfLineString );
        if( intersectionUpProfile instanceof Point )
        {
          /* there should be only one intersection point for each proflie */
          final double x = intersectionUpProfile.getCoordinate().x;
          final double y = intersectionUpProfile.getCoordinate().y;
          double z = 0;
          double width = 0;
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionUpProfile );
            width = WspmProfileHelper.getWidthPosition( gmpoint, m_upProfile.getProfil(), m_upProfile.getSrsName() );
            z = WspmProfileHelper.getHeightByWidth( width, m_upProfile.getProfil() );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
          }

          final GeometryFactory factory = new GeometryFactory();

          final Coordinate coordinates = new Coordinate( x, y, z );
          final Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.UP, side, width );
          m_intersPoints.add( IntersPoint );

          // store it in the local point list
          intersPointList.add( (Point) intersectionUpProfile );
        }
        else if( intersectionUpProfile instanceof GeometryCollection )
        {
          /*
           * if there are more than one intersection point for one profile, the bank line does not match correctly for
           * this section
           */
        }

        /* intersect it with the second (next) profile of this segment */
        final Geometry intersectionDownProfile = bankLine.intersection( m_downProfLineString );
        if( intersectionDownProfile instanceof Point )
        {
          /* there should be only one intersection point for each proflie */
          final double x = intersectionDownProfile.getCoordinate().x;
          final double y = intersectionDownProfile.getCoordinate().y;
          double z = 0;
          double width = 0;
          // get the z-value from the profile data
          try
          {
            final GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( intersectionDownProfile );
            width = WspmProfileHelper.getWidthPosition( gmpoint, m_DownProfile.getProfil(), m_DownProfile.getSrsName() );
            z = WspmProfileHelper.getHeightByWidth( width, m_DownProfile.getProfil() );
          }
          catch( final Exception e )
          {
            e.printStackTrace();
          }

          final GeometryFactory factory = new GeometryFactory();

          final Coordinate coordinates = new Coordinate( x, y, z );
          final Point point = factory.createPoint( coordinates );

          // store it in the point class including corresponding profile information
          final IntersPointData IntersPoint = new IntersPointData( point, CreateChannelData.PROF.DOWN, side, width );
          m_intersPoints.add( IntersPoint );

          // store it in the local point list
          intersPointList.add( (Point) intersectionDownProfile );
        }
        else if( intersectionDownProfile instanceof GeometryCollection )
        {
          /*
           * there should be only one intersection point for each proflie if there are more or less than two
           * intersection point for the two profiles, the bank lines does not match correctly for this section
           */
        }

        if( intersPointList.size() == 2 )
        {
          /* extract the LineStringSegment of the bankline lying between the two corresponding intersection points */
          final Point point1 = intersPointList.get( 0 );
          final Point point2 = intersPointList.get( 1 );

          switch( side )
          {
            case LEFT:
              if( m_bankLeftOrg == null )
                m_bankLeftOrg = JTSUtilities.createLineString( bankLine, point1, point2 );
              break;

            case RIGHT:
              if( m_bankRightOrg == null )
                m_bankRightOrg = JTSUtilities.createLineString( bankLine, point1, point2 );
              break;

            default:
              break;
          }
        }
      }
      catch( final GM_Exception e )
      {
        e.printStackTrace();
      }
    }
    calcWidthOrder();
  }

  /**
   * sets the widthorder depending on the place of the intersection point in the profile context
   */
  private void calcWidthOrder( )
  {
    final IntersPointData DataUp[] = new IntersPointData[2];
    final IntersPointData DataDown[] = new IntersPointData[2];

    double width1 = 0;
    double width2 = 0;
    int j = 0;
    int k = 0;

    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      if( m_intersPoints.get( i ).getProf() == CreateChannelData.PROF.UP )
      {
        DataUp[j] = m_intersPoints.get( i );
        j = j + 1;
      }
      if( m_intersPoints.get( i ).getProf() == CreateChannelData.PROF.DOWN )
      {
        DataDown[k] = m_intersPoints.get( i );
        k = k + 1;
      }
    }
    if( j == 2 )
    {
      width1 = DataUp[0].getWidth();
      width2 = DataUp[1].getWidth();
      if( width2 > width1 )
      {
        DataUp[0].setWidthOrder( WIDTHORDER.FIRST );
        DataUp[1].setWidthOrder( WIDTHORDER.LAST );
      }
      else
      {
        DataUp[0].setWidthOrder( WIDTHORDER.LAST );
        DataUp[1].setWidthOrder( WIDTHORDER.FIRST );
      }
    }
    if( k == 2 )
    {
      width1 = DataDown[0].getWidth();
      width2 = DataDown[1].getWidth();
      if( width2 > width1 )
      {
        DataDown[0].setWidthOrder( WIDTHORDER.FIRST );
        DataDown[1].setWidthOrder( WIDTHORDER.LAST );
      }
      else
      {
        DataDown[0].setWidthOrder( WIDTHORDER.LAST );
        DataDown[1].setWidthOrder( WIDTHORDER.FIRST );
      }
    }

  }

  /**
   * converts a WSPM profile into an linestring
   * 
   * @param profile
   *          Input profile to be converted.
   */
  private LineString convertProfilesToLineStrings( final IProfileFeature profile )
  {
    // get the profile line
    final GM_Curve profCurve = profile.getLine();
    try
    {
      final LineString profLine = (LineString) JTSAdapter.export( profCurve );
      return profLine;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  public void paintSegment( final Graphics g, final IMapPanel mapPanel ) throws Exception
  {
    // g.dispose();

    // if( m_bankRightOrg != null )
    // paintLineString( m_bankRightOrg, g, mapPanel, new Color( 150, 0, 0 ) );
    // if( m_bankLeftOrg != null )
    // paintLineString( m_bankLeftOrg, g, mapPanel, new Color( 0, 150, 0 ) );
    // if( m_profDownInters != null )
    // paintLinePoints( m_profDownInters, g, mapPanel, new Color( 100, 255, 50 ) );
    // if( m_profUpInters != null )
    // paintLinePoints( m_profUpInters, g, mapPanel, new Color( 100, 255, 50 ) );
    if( m_bankLeftInters != null )
      paintLineString( m_bankLeftInters, g, mapPanel, new Color( 100, 255, 50 ) );
    if( m_bankRightInters != null )
      paintLineString( m_bankRightInters, g, mapPanel, new Color( 100, 255, 50 ) );
  }

  private void paintLineStringPoints( final LineString line, final Graphics g, final IMapPanel mapPanel, final Color color )
  {
    if( line == null )
      return;

    final Color oldColor = g.getColor();
    g.setColor( color );

    for( int i = 0; i < line.getNumPoints(); i++ )
    {
      final int pointRectWidth = 5;
      final int halfRectWidth = pointRectWidth / 2;

      final double x = line.getPointN( i ).getCoordinate().x;
      final double y = line.getPointN( i ).getCoordinate().y;
      final int xs = (int) mapPanel.getProjection().getDestX( x );
      final int ys = (int) mapPanel.getProjection().getDestY( y );
      g.fill3DRect( xs - halfRectWidth, ys - halfRectWidth, pointRectWidth, pointRectWidth, true );
    }
    g.setColor( oldColor );
  }

  private void paintPoint( final Point point, final Graphics g, final IMapPanel mapPanel, final Color color )
  {
    final Color oldColor = g.getColor();
    g.setColor( color );

    final int pointRectWidth = 8;
    final int halfRectWidth = pointRectWidth / 2;

    final double x = point.getCoordinate().x;
    final double y = point.getCoordinate().y;
    final int xs = (int) mapPanel.getProjection().getDestX( x );
    final int ys = (int) mapPanel.getProjection().getDestY( y );
    g.fill3DRect( xs - halfRectWidth, ys - halfRectWidth, pointRectWidth, pointRectWidth, true );

    g.setColor( oldColor );
  }

  public boolean complete( )
  {
    boolean check;
    check = false;

    if( m_downIntersLinestring != null & m_upIntersLinestring != null & m_bankLeftInters != null & m_bankRightInters != null )
      check = true;
    return check;
  }

  public boolean checkBankDataComplete( )
  {
    boolean check;
    check = false;

    if( m_bankLeftInters != null & m_bankRightInters != null )
      check = true;
    return check;
  }

  /**
   * gets the intersection point (Point) of the profile (prof) for the specified bank side (side).
   */
  private Point getIntersPoint( final CreateChannelData.PROF prof, final CreateChannelData.SIDE side )
  {
    Point intersPoint = null;
    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      final IntersPointData data = m_intersPoints.get( i );

      if( side == data.getSide() & prof == data.getProf() )
      {
        intersPoint = data.getPoint();
        return intersPoint;
      }
    }
    return null;
  }

  /**
   * gets the intersection point (Point) of the profile (prof) for the specified side (widthorder).
   */
  private Point getIntersPoint( final CreateChannelData.PROF prof, final CreateChannelData.WIDTHORDER widthorder )
  {
    Point intersPoint = null;
    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      final IntersPointData data = m_intersPoints.get( i );

      if( widthorder == data.getWidthOrder() & prof == data.getProf() )
      {
        intersPoint = data.getPoint();
        return intersPoint;
      }
    }
    return null;
  }

  /**
   * sets the intersection point (Point) of the profile (prof) for the specified bank side (side).
   */
  public void setIntersPoint( final GM_Point gmpoint, final PROF prof, final WIDTHORDER widthorder, final double width ) throws Exception
  {
    final IGeoTransformer transformer = GeoTransformerFactory.getGeoTransformer( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );

    for( int i = 0; i < m_intersPoints.size(); i++ )
    {
      final IntersPointData data = m_intersPoints.get( i );

      if( prof == data.getProf() & data.getWidthOrder() == widthorder )
      {
        // store the old point to get the right bankline points
        final Point oldPoint = data.getPoint();

        // here we have to create a point in the right coordinate system!!

        final GM_Point createGM_Point = (GM_Point) gmpoint.clone();

        final GM_Point transform = (GM_Point) transformer.transform( createGM_Point );
        final Coordinate coordinate2 = new Coordinate( transform.getX(), transform.getY(), transform.getZ() );

        final GeometryFactory factory = new GeometryFactory();
        final Point point = factory.createPoint( coordinate2 );
        data.setPoint( point );
        data.setWidth( width );
        if( point.distance( oldPoint ) > 0.01 )
          updateBanklines( oldPoint, point );
      }
    }

  }

  public int getNumBankIntersections( )
  {
    return m_numBankIntersections;
  }

  public void setNumBankIntersections( final int numIntersections )
  {
    m_numBankIntersections = numIntersections;
  }

  public void updateBankIntersection( )
  {
    if( m_bankLeftInters != null )
      m_bankLeftInters = intersectLineString( m_bankLeftOrg, m_numBankIntersections );
    if( m_bankRightInters != null )
      m_bankRightInters = intersectLineString( m_bankRightOrg, m_numBankIntersections );
  }

  public void updateBankIntersection2( )
  {
    if( m_bankLeftInters != null )
      m_bankLeftInters = intersectLineString( m_bankLeftInters, m_numBankIntersections );
    if( m_bankRightInters != null )
      m_bankRightInters = intersectLineString( m_bankRightInters, m_numBankIntersections );
  }

  /**
   * manages the update of the profile data, after the intersected profiles were changed by the chart view layer in the
   * gui. things to do: -update the intersection points -> will be done by the layer -update the intersected banklines
   * -update the profiles (-> croping, intersecting, elevation adjusting)
   */
  public void updateProfileIntersection( )
  {
    if( complete() == true )
    {
      /* get the cropped and intersected profiles & linestrings */

      // muss jedes mal nach profile edit aufgerufen werden!
      // UPSTREAM
      try
      {
        /* crop the profile */
        m_upCroppedProfile = createCroppedIProfile( m_upProfile, CreateChannelData.PROF.UP );

        /* the cropped profile area is the desired value for the intersected profile area */
        final double areaUpCroppedProfile = ProfilUtil.calcArea( m_upCroppedProfile );

        /* intersect the cropped profile */
        // here not necessary, because the initial intersection was already done. The intersection here will be
        // handled by the user.
        // final IProfil tempPreviousIntersProfile = createIntersectedIProfile( m_previousCroppedProfile );
        // LineString
        // m_upProfLineString = createCroppedProfileLineString( m_upProfile, CreateChannelData.PROF.UP );
        final IProfil tmpupIntersProfile = adaptProfileElevations( m_upIntersProfile, m_upCroppedProfile );
        final double areaUpIntersProfile = ProfilUtil.calcArea( tmpupIntersProfile );

        // Fl�chenausgleich!!
        m_upIntersProfile = adjustProfileArea( m_upIntersProfile, areaUpCroppedProfile, areaUpIntersProfile );

        // m_upIntersProfile = createIntersectedIProfile( m_upCroppedProfile );

        final GeometryFactory factory = new GeometryFactory();
        m_upIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );

      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }

      // DOWNSTREAM
      try
      {
        /* crop the profile */
        // IProfil
        m_downCroppedProfile = createCroppedIProfile( m_DownProfile, CreateChannelData.PROF.DOWN );

        /* the cropped profile area is the desired value for the intersected profile area */
        final double areaDownCroppedProfile = ProfilUtil.calcArea( m_downCroppedProfile );

        /* intersect the cropped profile */
        // here not necessary, because the initial intersection was already done. The intersection here will be
        // handeled by the user.
        // LineString
        // final IProfil tempNextIntersProfile = createIntersectedIProfile( m_nextCroppedProfile );
        final IProfil tmpdownIntersProfile = adaptProfileElevations( m_downIntersProfile, m_downCroppedProfile );
        final double areaDownIntersProfile = ProfilUtil.calcArea( tmpdownIntersProfile );

        // Fl�chenausgleich!!
        m_downIntersProfile = adjustProfileArea( m_downIntersProfile, areaDownCroppedProfile, areaDownIntersProfile );

        final GeometryFactory factory = new GeometryFactory();
        m_downIntersLinestring = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * For the area adjustment it is necessary first to get the original heigths of the cropped profile to the intersected
   * profile
   */
  private IProfil adaptProfileElevations( final IProfil intersProfile, final IProfil croppedProfile )
  {
    final IProfilPointPropertyProvider provider = KalypsoModelWspmCoreExtensions.getPointPropertyProviders( intersProfile.getType() );

    final IRecord[] profilPointList = intersProfile.getPoints();

    final IProfil tmpProfil = ProfilFactory.createProfil( intersProfile.getType() );

    /* get / create components */
    final IComponent breiteComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_BREITE );
    final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( tmpProfil, IWspmConstants.POINT_PROPERTY_HOEHE );
    final IComponent rwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_RECHTSWERT );
    final IComponent hwComponent = provider.getPointProperty( IWspmConstants.POINT_PROPERTY_HOCHWERT );

    /* add components to new profile */
    tmpProfil.addPointProperty( breiteComponent );
    tmpProfil.addPointProperty( hoeheComponent );
    tmpProfil.addPointProperty( rwComponent );
    tmpProfil.addPointProperty( hwComponent );

    for( final IRecord point : profilPointList )
    {
//      final TupleResult result = point.getOwner();
      final double currentWidth = (Double) point.getValue( point.indexOfComponent( IWspmConstants.POINT_PROPERTY_BREITE ) );

      final IRecord pt = tmpProfil.createProfilPoint();
//      final TupleResult owner = pt.getOwner();

      final IComponent[] components = intersProfile.getPointProperties();
      for( final IComponent component : components )
      {
        final int index = pt.indexOfComponent( component.getId() );
        final Object value = point.getValue( index );
        pt.setValue( index, value );
      }
      try
      {
        final int hoeheIndex = pt.indexOfComponent( IWspmConstants.POINT_PROPERTY_HOEHE );
        pt.setValue( hoeheIndex, WspmProfileHelper.getHeightByWidth( currentWidth, croppedProfile ) );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      tmpProfil.addPoint( pt );
    }
    return tmpProfil;
  }

  /**
   * gets the map extend (GM_Envelope) of the current segment
   */
  public GM_Envelope getSegmentMapExtend( )
  {
    final Geometry[] boundary = new Geometry[4];
    int i;

    i = 0;
    if( m_upIntersLinestring != null )
    {
      boundary[0] = m_upIntersLinestring.getBoundary();
      i = i + 1;
    }
    if( m_downIntersLinestring != null )
    {
      boundary[1] = m_downIntersLinestring.getBoundary();
      i = i + 1;
    }
    if( m_bankLeftInters != null )
    {
      boundary[2] = m_bankLeftInters.getBoundary();
      i = i + 1;
    }
    if( m_bankRightInters != null )
    {
      boundary[3] = m_bankRightInters.getBoundary();
      i = i + 1;
    }

    double maxX = 0;
    double maxY = 0;
    double minX = 0;
    double minY = 0;

    for( int j = 0; j < i; j++ )
    {
      Coordinate[] coords = new Coordinate[2];
      coords = boundary[j].getCoordinates();

      for( int k = 0; k < 2; k++ )
      {

        if( j == 0 & k == 0 )
        {
          maxX = coords[k].x;
          maxY = coords[k].y;
          minX = coords[k].x;
          minY = coords[k].y;
        }

        if( coords[k].x > maxX )
          maxX = coords[k].x;
        if( coords[k].y > maxY )
          maxY = coords[k].y;
        if( coords[k].x < minX )
          minX = coords[k].x;
        if( coords[k].y < minY )
          minY = coords[k].y;
      }
    }

    minX = minX - (maxX - minX) / 10;
    minY = minY - (maxY - minY) / 10;
    maxX = maxX + (maxX - minX) / 10;
    maxY = maxY + (maxY - minY) / 10;

    return org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Envelope( minX, minY, maxX, maxY, null );

  }

  /**
   * the editable bank linestring is painted
   */
  public void paintBankLineLineString( final IMapPanel panel, final Graphics g, final Color color )
  {
    // paint the line
    try
    {
      paintLineString( getBankLeftInters(), g, panel, color );
      paintLineString( getBankRightInters(), g, panel, color );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }

    // paint the nodes
    paintLineStringPoints( getBankLeftInters(), g, panel, color );
    paintLineStringPoints( getBankRightInters(), g, panel, color );

  }

  private void paintLineString( final LineString line, final Graphics g, final IMapPanel mapPanel, final Color color ) throws GM_Exception, CoreException
  {
    if( line == null )
      return;

    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

    final GM_Curve Curve = (GM_Curve) JTSAdapter.wrap( line );
    Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );
    defaultstroke = symb.getStroke();
    stroke.setWidth( 2 );
    stroke.setStroke( color );
    symb.setStroke( stroke );

    final DisplayElement de = DisplayElementFactory.buildLineStringDisplayElement( null, Curve, symb );
    de.paint( g, mapPanel.getProjection(), new NullProgressMonitor() );

    // Set the Stroke back to default
    symb.setStroke( defaultstroke );

  }

  public void setBankLeftInters( final LineString bankLeftInters )
  {
    m_bankLeftInters = bankLeftInters;
  }

  public void setBankRightInters( final LineString bankRightInters )
  {
    m_bankRightInters = bankRightInters;
  }

  public void setBankLeftOrg( final LineString bankLeftInters )
  {
    m_bankLeftOrg = bankLeftInters;
  }

  public void setBankRightOrg( final LineString bankRightInters )
  {
    m_bankRightOrg = bankRightInters;
  }

  public void paintProfile( final CreateChannelData.PROF currentProfile, final IMapPanel panel, final Graphics g, final Color color )
  {
    LineString line = null;
    final GeometryFactory factory = new GeometryFactory();

    if( currentProfile == PROF.UP )
      line = factory.createLineString( convertProfileToCoordinates( m_upIntersProfile ) );
    else if( currentProfile == PROF.DOWN )
      line = factory.createLineString( convertProfileToCoordinates( m_downIntersProfile ) );

    try
    {
      paintLineString( line, g, panel, color );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
    paintLineStringPoints( line, g, panel, color );

  }

  public void paintIntersectionPoints( final IMapPanel panel, final Graphics g, final Color color, final PROF prof )
  {
    for( int i = 0; i < m_intersPoints.size(); i++ )
      if( m_intersPoints.get( i ).getProf() == prof )
      {
        final Point point = m_intersPoints.get( i ).getPoint();
        paintPoint( point, g, panel, color );
      }

  }

  private Coordinate[] convertProfileToCoordinates( final IProfil profile )
  {
    final Coordinate[] coords = new Coordinate[profile.getPoints().length];

    for( int i = 0; i < coords.length; i++ )
    {
      final IRecord record = profile.getPoints()[i];
      final TupleResult owner = record.getOwner();

      final IComponent rwComponent = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_RECHTSWERT );
      final IComponent hwComponent = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_HOCHWERT );
      final IComponent hoeheComponent = ProfilObsHelper.getPropertyFromId( profile, IWspmConstants.POINT_PROPERTY_HOEHE );

      final double x = (Double) record.getValue( owner.indexOfComponent( rwComponent ) );
      final double y = (Double) record.getValue( owner.indexOfComponent( hwComponent ) );
      final double z = (Double) record.getValue( owner.indexOfComponent( hoeheComponent ) );

      coords[i] = new Coordinate( x, y, z );
    }
    return coords;
  }

  /**
   * sets the user edited profile as new intersected profile
   */
  public void setNewIntersectedProfile( final IProfil profile, final PROF prof )
  {
    if( prof == PROF.UP )
      m_upIntersProfile = profile;
    else if( prof == PROF.DOWN )
      m_downIntersProfile = profile;

    updateProfileIntersection();
  }

  /**
   * updates the intersected bankline linestring with the new edge point (moved by profile chart) by moving the
   * first/last line point (oldPoint) to the new location (newPoint).
   */
  private void updateBanklines( final Point oldPoint, final Point newPoint )
  {
    // TODO: we have a problem with projected coords. check what has to be transformed (the new coord?). and do it!!

    // find the correct bankline
    Point point = m_bankLeftInters.getPointN( 0 );

    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankLeftInters.getCoordinates();
      coords[0] = newPoint.getCoordinate();

      final GeometryFactory factory = new GeometryFactory();
      m_bankLeftOrg = factory.createLineString( coords );
    }

    point = m_bankLeftInters.getPointN( m_numBankIntersections - 1 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankLeftInters.getCoordinates();
      coords[m_numBankIntersections - 1] = newPoint.getCoordinate();

      final GeometryFactory factory = new GeometryFactory();
      m_bankLeftOrg = factory.createLineString( coords );
    }

    point = m_bankRightInters.getPointN( 0 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankRightInters.getCoordinates();
      coords[0] = newPoint.getCoordinate();

      final GeometryFactory factory = new GeometryFactory();
      m_bankRightOrg = factory.createLineString( coords );

    }

    point = m_bankRightInters.getPointN( m_numBankIntersections - 1 );
    if( point.distance( oldPoint ) < 0.01 )
    {
      Coordinate[] coords = new Coordinate[m_numBankIntersections];
      coords = m_bankRightInters.getCoordinates();
      coords[m_numBankIntersections - 1].setCoordinate( newPoint.getCoordinate() );

      final GeometryFactory factory = new GeometryFactory();
      m_bankRightOrg = factory.createLineString( coords );
    }
  }

}