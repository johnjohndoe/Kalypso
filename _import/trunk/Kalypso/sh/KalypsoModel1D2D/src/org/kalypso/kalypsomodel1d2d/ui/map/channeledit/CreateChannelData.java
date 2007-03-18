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
package org.kalypso.kalypsomodel1d2d.ui.map.channeledit;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.gmlschema.property.IValuePropertyType;
import org.kalypso.jts.LineStringUtilities;
import org.kalypso.jts.QuadMesher.JTSCoordsElevInterpol;
import org.kalypso.jts.QuadMesher.JTSQuadMesher;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.model.wspm.core.gml.WspmProfile;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.model.wspm.core.profil.IProfilEventManager;
import org.kalypso.model.wspm.core.profil.impl.ProfilEventManager;
import org.kalypso.model.wspm.ui.action.FeatureComparator;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.displayelements.IncompatibleGeometryTypeException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.PointSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * State object for create main channel widget and composite.
 * 
 * @author Thomas Jung
 */
public class CreateChannelData
{
  public enum SIDE
  {
    LEFT,
    RIGHT;
  }

  public enum PROF
  {
    UP,
    DOWN;
  }

  public enum WIDTHORDER
  {
    FIRST,
    LAST;
  }

  private static ISchedulingRule THE_SEGMENT_INIT_MUTEX = new MutexRule();
  
  private IKalypsoFeatureTheme m_profileTheme;

  private IKalypsoFeatureTheme m_bankTheme1; // LEFT = 1

  private IKalypsoFeatureTheme m_bankTheme2; // RIGHT = 2

  private Set<Feature> m_selectedProfiles = new HashSet<Feature>();

  private int m_numbProfileIntersections = 6;

  private Map<Feature, SIDE> m_selectedBanks = new HashMap<Feature, SIDE>();

  private final CreateMainChannelWidget m_widget;

  private boolean datacomplete;

  private int m_numBankSelections;

  private int m_globNumbBankIntersections;

  private List<SegmentData> m_segmentList = new LinkedList<SegmentData>();

  private List<Coordinate[][]> m_coordList = new LinkedList<Coordinate[][]>();

  private Coordinate[][] m_meshCoords;

  public int m_selectedSegment;

  private CreateChannelData.PROF m_selectedProfile;

  public CreateChannelData( final CreateMainChannelWidget widget )
  {
    m_widget = widget;
  }

  /* --------------------- Theme handling ---------------------------------- */

  public IKalypsoFeatureTheme getProfileTheme( )
  {
    return m_profileTheme;
  }

  public IKalypsoFeatureTheme getBankTheme1( )
  {
    return m_bankTheme1;
  }

  public IKalypsoFeatureTheme getBankTheme2( )
  {
    return m_bankTheme2;
  }

  public void setProfileTheme( final IKalypsoFeatureTheme profileTheme )
  {
    m_profileTheme = profileTheme;

    // TODO: event handling
  }

  public void setBankTheme1( final IKalypsoFeatureTheme bankTheme )
  {
    m_bankTheme1 = bankTheme;
  }

  public void setBankTheme2( final IKalypsoFeatureTheme bankTheme )
  {
    m_bankTheme2 = bankTheme;
  }

  /**
   * Gets the WSPM profile themes in the Kalypso theme list
   */
  public IKalypsoFeatureTheme[] getProfileThemes( )
  {
    final IMapModell mapModell = m_widget.getPanel().getMapModell();
    if( mapModell == null )
      return new IKalypsoFeatureTheme[0];

    final IKalypsoTheme[] allThemes = mapModell.getAllThemes();

    final List<IKalypsoFeatureTheme> goodThemes = new ArrayList<IKalypsoFeatureTheme>();

    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = fTheme.getFeatureType();

        if( featureType != null && GMLSchemaUtilities.substitutes( featureType, WspmProfile.QNAME_PROFILE ) )
          goodThemes.add( fTheme );
      }
    }

    return goodThemes.toArray( new IKalypsoFeatureTheme[goodThemes.size()] );
  }

  /**
   * Gets the linestring themes in the Kalypso theme list
   */
  public IKalypsoFeatureTheme[] getBankThemes( )
  {
    final IKalypsoTheme[] allThemes = m_widget.getPanel().getMapModell().getAllThemes();

    final List<IKalypsoFeatureTheme> goodThemes = new ArrayList<IKalypsoFeatureTheme>();

    for( final IKalypsoTheme theme : allThemes )
    {
      if( theme instanceof IKalypsoFeatureTheme )
      {
        final IKalypsoFeatureTheme fTheme = (IKalypsoFeatureTheme) theme;
        final IFeatureType featureType = fTheme.getFeatureType();

        if( featureType == null )
          continue;
        
        final IValuePropertyType[] allGeomteryProperties = featureType.getAllGeomteryProperties();
        // choose only the linestrings
        // just take the first found property
        if( allGeomteryProperties.length > 0 && allGeomteryProperties[0].getValueQName().equals( GeometryUtilities.QN_LINE_STRING_PROPERTY ) )
          goodThemes.add( fTheme );
      }
    }
    return goodThemes.toArray( new IKalypsoFeatureTheme[goodThemes.size()] );
  }

  /* --------------------- selection handling ---------------------------------- */

  public void changeSelectedProfiles( final Feature[] profileFeaturesToRemove, final Feature[] profileFeaturesToAdd )
  {
    m_selectedProfiles.removeAll( Arrays.asList( profileFeaturesToRemove ) );
    m_selectedProfiles.addAll( Arrays.asList( profileFeaturesToAdd ) );
    initSegments();
  }

  public void resetSelectedProfiles( )
  {
    m_selectedProfiles.clear();
    initSegments();
  }
  
  public void initSegments( )
  {
    final Job job = new Job( "Init segments" )
    {
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        datacomplete = false;

        // there must be at least two selected profiles and one selected bank.
        if( m_selectedBanks.size() > 1 && m_selectedProfiles.size() > 1 )
        {
          datacomplete = true;
        }
        else if( m_selectedProfiles.size() <= 1 )
        {
          m_segmentList.clear();
          m_coordList.clear();
        }

        // final IProgressMonitor monitor = new NullProgressMonitor();
        if( datacomplete == true )
          /* intersects the banks with the profiles and manages the initial segment creation */
          intersectBanksWithProfs( monitor ); // initial calculation of the segments by global parameters

        m_widget.update();

        return Status.OK_STATUS;
      }
    };
    job.setRule( THE_SEGMENT_INIT_MUTEX );
    job.setUser( true );
    job.schedule();
  }

  public Feature[] getSelectedProfiles( )
  {
    return m_selectedProfiles.toArray( new Feature[m_selectedProfiles.size()] );
  }

  /**
   * @param side
   *          false: left, true: right
   */
  public void addSelectedBanks( final Feature[] bankFeatures, final SIDE side )
  {
    for( final Feature feature : bankFeatures )
      m_selectedBanks.put( feature, side );

    initSegments();
  }

  public Feature[] getSelectedBanks( final SIDE side )
  {
    final List<Feature> result = new ArrayList<Feature>();

    for( final Map.Entry<Feature, SIDE> entry : m_selectedBanks.entrySet() )
    {
      final Feature bankFeature = entry.getKey();
      final SIDE value = entry.getValue();
      if( value == side )
        result.add( bankFeature );
    }

    return result.toArray( new Feature[result.size()] );
  }

  public void removeSelectedBanks( final Feature[] bankFeatures )
  {
    for( final Feature feature : bankFeatures )
      m_selectedBanks.remove( feature );

    m_widget.update();

    m_selectedBanks.keySet().removeAll( Arrays.asList( bankFeatures ) );

    initSegments();
  }

  /* --------------------- profile chart handling ---------------------------------- */

  public IProfilEventManager getProfilEventManager( )
  {
    if( m_selectedProfiles.size() <= 1 )
      return new ProfilEventManager( null, null );
    
    if (getSelectedSegment() == 0)
      return  new ProfilEventManager( null, null );
    
    final SegmentData segment = m_segmentList.get( getSelectedSegment() - 1 );

    final IProfil profil;
    if( m_selectedProfile == PROF.UP )
    {
      profil = segment.getProfilUpOrg();
    }
    else
    {
      profil = segment.getProfilDownOrg();
    }

    final ProfilEventManager pem = new ProfilEventManager( profil, null );
    return pem;
  }

  /* --------------------- workflow handling ---------------------------------- */

  /**
   * checks, if all necessary data is specified (profiles, bank lines...).
   */
  public void completationCheck( )
  {
    m_meshCoords = null;
    datacomplete = false;

    // there must be at least two selected profiles and one selected bank.
    if( m_selectedBanks.size() > 1 && m_selectedProfiles.size() > 1 )
    {
      datacomplete = true;
    }
    else if( m_selectedProfiles.size() <= 1 )
    {
      m_segmentList.clear();
      m_coordList.clear();
    }

    if( datacomplete == false )
      m_segmentList.clear();

    // if( datacomplete == true )
    // /* intersects the banks with the profiles and manages the initial segment creation */
    // intersectBanksWithProfs(); // initial calculation of the segments by global parameters
    // // else if ( datacomplete == true & m_coordList.size() > 0)

    if( m_segmentList.size() > 0 )
    /* if there are segments, start Quadmesher */
    // TODO: error handling implementation!!
    {
      manageQuadMesher();
      mergeMeshList();
    }
  }

  public boolean getMeshStatus( )
  {
    return m_meshCoords != null;
  }

  /**
   * converts the mesh coordinates into the 2d model
   */
  public void convertToModel( )
  {
    GM_Point[][] importingGridPoints = new GM_Point[m_meshCoords.length][m_meshCoords[0].length];
    importingGridPoints = convertToGMPoints( m_meshCoords );

    final MapPanel mapPanel = m_widget.getPanel();
    final IMapModell mapModel = mapPanel.getMapModell();
    final IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme( mapModel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final IKalypsoFeatureTheme theme = UtilMap.findEditableThem( mapModel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final CommandableWorkspace workspace = theme.getWorkspace();

    final TempGrid tempGrid = new TempGrid();
    tempGrid.importMesh( importingGridPoints );
    try
    {
      final ICommand command = tempGrid.getAddToModelCommand( mapPanel, model1d2d, workspace, 0 );
      workspace.postCommand( command );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  /**
   * converts a Coordinate[][] array into a GM_Point[][] array
   */
  private GM_Point[][] convertToGMPoints( Coordinate[][] meshCoords )
  {
    GeometryFactory geometryFactory = new GeometryFactory();

    GM_Point points2D[][] = new GM_Point[meshCoords.length][];
    for( int i = 0; i < meshCoords.length; i++ )
    {
      Coordinate[] line = meshCoords[i];
      GM_Point[] points1D = new GM_Point[line.length];
      points2D[i] = points1D;
      for( int j = 0; j < line.length; j++ )
      {
        Coordinate coord = line[j];
        try
        {
          points1D[j] = (GM_Point) JTSAdapter.wrap( geometryFactory.createPoint( coord ) );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
      }
    }
    return points2D;
  }

  /**
   * this is the manager for starting the JTSQuadMesher for all complete segments
   */
  private void manageQuadMesher( )
  {
    m_coordList.clear();

    for( int i = 0; i < m_segmentList.size(); i++ )
    {
      final boolean complete;
      SegmentData segment = m_segmentList.get( i );
      complete = segment.complete();
      if( complete == true )
      {
        /* arrange the lines for the mesher */
        /*
         * -the lines have to be oriented (clw or cclw), the order ist top (profile) - left (bank) - bottom (profile) -
         * right (bank) or top - right - bottom - left -the end point of a line must be the same as the start point of
         * the next line -the lines itselfes must be also oriented all in the same way (clw/cclw)
         */

        // at the end point of the profile line there should follow the start point of the next line
        LineString[] lines = new LineString[4];

        lines[0] = segment.getProfUpIntersLineString();
        lines[1] = segment.getBankRightInters();
        lines[2] = segment.getProfDownIntersLineString();
        lines[3] = segment.getBankLeftInters();
        lines = checkLineOrientation( lines );

        // now the two lines are right oriented...
        final LineString topLine = lines[0];
        final LineString rightLine = lines[1];
        final LineString bottomLine = lines[2];
        final LineString leftLine = lines[3];

        final JTSQuadMesher mesher = new JTSQuadMesher( topLine, bottomLine, leftLine, rightLine );
        final Coordinate[][] coords = mesher.calculateMesh();
        final JTSCoordsElevInterpol adjuster = new JTSCoordsElevInterpol( coords );
        final Coordinate[][] coords2 = adjuster.calculateElevations();

        m_coordList.add( coords2 );
      }
    }
  }

  public boolean checkMesh( int segmentNumber )
  {
    boolean check = false;
    if( m_coordList.get( segmentNumber ) != null )
      check = true;

    return check;
  }

  /**
   * erases the double coords and merges the coords list to an array of coords
   */
  private void mergeMeshList( )
  {
    m_meshCoords = null;
    final int overallYCoordNum = calculateOverallYCoordNum();
    final int numX = m_numbProfileIntersections;
    final int numY = overallYCoordNum;
    Coordinate[][] newCoords = new Coordinate[numX][numY];

    int coordYPosPointer = 0;
    int coordSegmentPointer = 0;

    // loop over all segments -> coords
    for( int i = 0; i < m_coordList.size(); i++ )
    {
      Coordinate[][] coordinates = m_coordList.get( i );
      if( numX != coordinates.length )
        return;
      // coordSegmentPointer = coordYPosPointer;
      // loop over all profile intersections -> coords [x][]
      for( int j = 0; j < coordinates.length; j++ )
      {
        // loop over all bank intersections -> coords [][x]

        for( int k = 0; k < coordinates[j].length; k++ )
        {
          coordYPosPointer = coordSegmentPointer + k;
          newCoords[j][coordYPosPointer] = coordinates[j][k];
        }
      }
      coordSegmentPointer = coordSegmentPointer + coordinates[0].length - 1;
    }
    if( coordYPosPointer != overallYCoordNum - 1 )
      System.out.println( "consolidateMesh: wrong number of coord array!! " );

    m_meshCoords = newCoords;
  }

  private int calculateOverallYCoordNum( )
  {
    int num = 0;

    // add the coords lengths to num
    for( int i = 0; i < m_coordList.size(); i++ )
    {
      num = num + m_coordList.get( i )[0].length;
    }
    // substract the number of double profile coords
    if( m_coordList.size() > 0 )
      num = num - (m_segmentList.size() - 1);

    return num;
  }

  private LineString[] checkLineOrientation( LineString[] lines )
  {
    final LineString[] lineArray = new LineString[4];

    GeometryFactory factory1 = new GeometryFactory();
    Coordinate[] coords1 = new Coordinate[lines[0].getNumPoints()];

    GeometryFactory factory2 = new GeometryFactory();
    Coordinate[] coords2 = new Coordinate[lines[1].getNumPoints()];

    GeometryFactory factory3 = new GeometryFactory();
    Coordinate[] coords3 = new Coordinate[lines[2].getNumPoints()];

    GeometryFactory factory4 = new GeometryFactory();
    Coordinate[] coords4 = new Coordinate[lines[3].getNumPoints()];

    boolean error = false;

    // At first line[0] and line [1]
    // check if the lines lie allready in a right orientation.

    Point startpoint1 = lines[0].getStartPoint();
    Point endpoint1 = lines[0].getEndPoint();
    Point startpoint2 = lines[1].getStartPoint();
    Point endpoint2 = lines[1].getEndPoint();

    if( endpoint1.distance( startpoint2 ) < 0.001 ) // the distance method checks only in a 2d way!
    {
      for( int i = 0; i < coords1.length; i++ )
      {
        coords1[i] = lines[0].getCoordinateN( i );
      }
      LineString line1 = factory1.createLineString( coords1 );

      for( int i = 0; i < coords2.length; i++ )
      {
        coords2[i] = lines[1].getCoordinateN( i );
      }
      LineString line2 = factory2.createLineString( coords2 );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }
    else if( endpoint1.distance( endpoint2 ) < 0.001 )
    {
      // switch line 2
      for( int i = 0; i < coords1.length; i++ )
      {
        coords1[i] = lines[0].getCoordinateN( i );
      }
      LineString line1 = factory1.createLineString( coords1 );
      LineString line2 = LineStringUtilities.changeOrientation( lines[1] );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }
    else if( startpoint1.distance( startpoint2 ) < 0.001 )
    {
      // switch line 1
      LineString line1 = LineStringUtilities.changeOrientation( lines[0] );

      for( int i = 0; i < coords2.length; i++ )
      {
        coords2[i] = lines[1].getCoordinateN( i );
      }
      LineString line2 = factory1.createLineString( coords2 );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }
    else if( startpoint1.distance( endpoint2 ) < 0.001 )
    {
      // switch both lines
      LineString line1 = LineStringUtilities.changeOrientation( lines[0] );
      LineString line2 = LineStringUtilities.changeOrientation( lines[1] );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }

    // now the first two lines have the same orientation...
    // update the corresponding start and end point informations
    startpoint1 = lineArray[0].getStartPoint();
    endpoint1 = lineArray[0].getEndPoint();
    startpoint2 = lineArray[1].getStartPoint();
    endpoint2 = lineArray[1].getEndPoint();

    Point startpoint3 = lines[2].getStartPoint();
    Point endpoint3 = lines[2].getEndPoint();

    // next: check line[2] but don't change line 1 and line 2

    if( endpoint2.distance( startpoint3 ) < 0.001 ) // the distance method checks only in a 2d way!
    {
      for( int i = 0; i < coords3.length; i++ )
      {
        coords3[i] = lines[2].getCoordinateN( i );
      }
      LineString line3 = factory3.createLineString( coords3 );

      lineArray[2] = line3;
    }
    else if( endpoint2.distance( endpoint3 ) < 0.001 )
    {
      // switch line 3
      LineString line3 = LineStringUtilities.changeOrientation( lines[2] );

      lineArray[2] = line3;
    }
    else
    {
      // this should actually never happen!!
      error = true;
    }

    // now the first three lines have the same orientation...
    // update start / end point informations
    startpoint3 = lineArray[2].getStartPoint();
    endpoint3 = lineArray[2].getEndPoint();

    // next: check line[3] but don't change line 1, line 2 and line 3
    final Point startpoint4 = lines[3].getStartPoint();
    final Point endpoint4 = lines[3].getEndPoint();

    if( endpoint3.distance( startpoint4 ) < 0.001 ) // the distance method checks only in a 2d way!
    {
      for( int i = 0; i < coords4.length; i++ )
      {
        coords4[i] = lines[3].getCoordinateN( i );
      }
      LineString line4 = factory4.createLineString( coords4 );

      lineArray[3] = line4;
    }
    else if( endpoint3.distance( endpoint4 ) < 0.001 )
    {
      // switch line 4
      LineString line4 = LineStringUtilities.changeOrientation( lines[3] );

      lineArray[3] = line4;
    }
    else
    {
      // this should actually never happen!!
      error = true;
    }

    // final check: the last point of the last line should be the same as the first point of the first line
    final Point endpoint = lineArray[3].getEndPoint();
    final Point startpoint = lineArray[0].getStartPoint();

    if( endpoint.distance( startpoint ) > 0.001 || error == true )
    {
      System.out.println( "Flussschlauchgenerator: An error during the orientation of the segment lines occured " );
    }

    return lineArray;
  }

  /**
   * all selected banks will be intersected by the selected profiles Input: all selected banks and profiles Output:
   * intersected banks as linestrings (including the intersection point)
   */
  private void intersectBanksWithProfs( final IProgressMonitor monitor )
  {
    /* at first -> clear the segment list! */
    m_segmentList.clear();

    final Feature[] profileFeatures = m_selectedProfiles.toArray( new Feature[m_selectedProfiles.size()] );

    if( profileFeatures.length == 0 )
      return;

    monitor.beginTask( "Processing profiles", profileFeatures.length );

    final IPropertyType stationProperty = profileFeatures[0].getFeatureType().getProperty( WspmProfile.QNAME_STATION );
    Arrays.sort( profileFeatures, new FeatureComparator( stationProperty ) );

    // TODO: intersect all selected profiles and banks and store it into a linkedList of SegmentData.

    // loop over all profiles
    // take two neighbouring profiles create a segment for them

    WspmProfile lastProfile = null;
    for( final Feature profileFeature : profileFeatures )
    {

      // get the profile line
      final WspmProfile profile = new WspmProfile( profileFeature );

      monitor.subTask( "Station km " + profile.getStation() );

      if( lastProfile != null )
      {
        // working on the segment
        final int numBankIntersections = getGlobNumBankIntersections();
        final SegmentData segment = new SegmentData( this, profile, lastProfile, m_selectedBanks, numBankIntersections );
        System.out.println( "up profile: " + lastProfile.getStation() );
        System.out.println( "down profile: " + profile.getStation() );

        // add to list
        m_segmentList.add( segment );
      }
      else
      {
        // tu nix
      }
      lastProfile = profile;

      if( monitor.isCanceled() )
        throw new OperationCanceledException();

      monitor.worked( 1 );
    }
  }

  public void paintAllSegments( final Graphics g, final MapPanel mapPanel )
  {
    if( m_meshCoords != null )
    {
      try
      {
        paintEdges( m_meshCoords, g, mapPanel );
      }
      catch( GM_Exception e )
      {
        e.printStackTrace();
      }
      catch( IncompatibleGeometryTypeException e )
      {
        e.printStackTrace();
      }
      // paintCoords( m_meshCoords, g, mapPanel );
    }
  }

  public void setNumProfileIntersections( int numProfileIntersections )
  {
    if( m_numbProfileIntersections != numProfileIntersections )
    {
      m_numbProfileIntersections = numProfileIntersections;
      // updateSegments( false );
      initSegments();
    }
  }

  public int getNumProfileIntersections( )
  {
    final int numProfileIntersections = m_numbProfileIntersections;
    return numProfileIntersections;
  }

  public void setGlobNumBankIntersections( int globNumBankIntersections )
  {
    if( globNumBankIntersections != m_globNumbBankIntersections )
    {
      m_globNumbBankIntersections = globNumBankIntersections;
      initSegments();
    }
  }

  public int getGlobNumBankIntersections( )
  {
    final int globNumBankIntersections = m_globNumbBankIntersections;
    return globNumBankIntersections;
  }

  private void paintCoords( final Coordinate[][] coords, final Graphics g, final MapPanel mapPanel )
  {
    final PointSymbolizer symb = new PointSymbolizer_Impl();
    DisplayElement de;

    for( int i = 0; i < coords.length; i++ )
    {
      for( int j = 0; j < coords[i].length; j++ )
      {
        GeometryFactory factory = new GeometryFactory();
        Point point = factory.createPoint( coords[i][j] );
        try
        {
          GM_Point gmpoint = (GM_Point) JTSAdapter.wrap( point );
          de = DisplayElementFactory.buildPointDisplayElement( null, gmpoint, symb );
          de.paint( g, mapPanel.getProjection() );
        }
        catch( GM_Exception e )
        {
          e.printStackTrace();
        }
      }
    }
  }

  private void paintEdges( final Coordinate[][] coords, final Graphics g, final MapPanel mapPanel ) throws GM_Exception, IncompatibleGeometryTypeException
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );
    Stroke defaultstroke = new Stroke_Impl( new HashMap(), null, null );
    defaultstroke = symb.getStroke();
    Color grey = new Color( 100, 100, 100 );

    stroke.setWidth( 1 );
    stroke.setStroke( grey );
    symb.setStroke( stroke );
    DisplayElement de;

    Coordinate[] lineCoords = new Coordinate[2];

    for( int i = 0; i < coords.length; i++ )
    {
      GeometryFactory factory = new GeometryFactory();
      for( int j = 0; j < coords[i].length - 1; j++ )
      {
        lineCoords[0] = coords[i][j];
        lineCoords[1] = coords[i][j + 1];
        LineString line = factory.createLineString( lineCoords );
        final GM_Curve curve = (GM_Curve) JTSAdapter.wrap( line );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, mapPanel.getProjection() );
      }
    }
    for( int j = 0; j < coords[0].length; j++ )
    {
      GeometryFactory factory = new GeometryFactory();
      for( int i = 0; i < coords.length - 1; i++ )
      {
        lineCoords[0] = coords[i][j];
        lineCoords[1] = coords[i + 1][j];
        LineString line = factory.createLineString( lineCoords );
        final GM_Curve curve = (GM_Curve) JTSAdapter.wrap( line );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, mapPanel.getProjection() );
      }
    }
  }

  public int getNumOfSegments( )
  {
    return m_segmentList.size();
  }

  public int getSelectedSegment( )
  {
    return m_selectedSegment;
  }

  public void setSelectedSegment( int selectedSegment )
  {
    m_selectedSegment = selectedSegment;
  }

  /**
   * returns the number of bank intersections for the current segment
   */
  public int getNumBankIntersections( int segment )
  {
    return m_segmentList.get( segment - 1 ).getNumBankIntersections();
  }

  /**
   * sets the number of bank intersections for the current segment
   */
  public void setNumBankIntersections( int segment, int numIntersections )
  {
    m_segmentList.get( segment - 1 ).setNumBankIntersections( numIntersections );
  }

  /**
   * this method possibly updates the segment data and pushes the calculation of the mesh. inputs: boolean edit: false->
   * update of the bankline and profile intersections true: -> no data update
   */
  public void updateSegments( boolean edit )
  {
    // loop over all segments
    final SegmentData[] datas = m_segmentList.toArray( new SegmentData[m_segmentList.size()] );
    for( final SegmentData segment : datas )
    {
      if( segment != null & edit == false )
      {
        // intersect the bankline by the defined number of intersections
        segment.updateBankIntersection();
        segment.updateProfileIntersection();
      }
      else if( segment != null & edit == true )
      {
        // commits the done edits

        // segment.updateBank();
        // segment.updateProfile();
      }
    }
    completationCheck();
  }

  public GM_Envelope getCurrentSegmentExtend( int segment )
  {
    return m_segmentList.get( segment - 1 ).getSegmentMapExtend();
  }

  public void drawBankLine( int segment, int side, final Graphics g )
  {
    MapPanel panel = m_widget.getPanel();
    m_segmentList.get( segment - 1 ).paintBankLineLineString( panel, g, side, new Color( 20, 20, 255 ) );
  }

  public SegmentData getCurrentSegment( int segment )
  {
    if( m_segmentList.size() >= segment )
      return m_segmentList.get( segment - 1 );
    else
      return null;

  }

  public void updateSegment( boolean edit, int segmentNumber )
  {
    final SegmentData segment = m_segmentList.get( segmentNumber - 1 );
    if( segment != null & edit == false )
    {
      // intersect the bankline by the defined number of intersections
      segment.updateBankIntersection();
      segment.updateProfileIntersection();
    }
    else if( segment != null & edit == true )
    {
      // commits the done edits

      // segment.updateBank();
      // segment.updateProfile();
    }
    completationCheck();
  }

  public final CreateChannelData.PROF getCurrentProfile( )
  {
    return m_selectedProfile;
  }

  public void setCurrentProfile( CreateChannelData.PROF profile )
  {
    m_selectedProfile = profile;
  }

  public void switchProfile( )
  {
    if( m_selectedProfile == PROF.UP )
      m_selectedProfile = PROF.DOWN;
    else
      m_selectedProfile = PROF.UP;
  }

  public List getNeighbourSegments( int currentSegmentNum )
  {
    final int numOfSegments = m_segmentList.size();
    final List<SegmentData> neighbours = new LinkedList<SegmentData>();
    /* check the neighbours */
    if( currentSegmentNum > 1 & currentSegmentNum < numOfSegments )
    {
      // the current segment lies inbetween -> two neighbours
      neighbours.add( m_segmentList.get( currentSegmentNum - 2 ) ); // neighbour before
      neighbours.add( m_segmentList.get( currentSegmentNum ) ); // neighbour after
    }
    else if( currentSegmentNum > 1 & currentSegmentNum == numOfSegments )
    {
      // the current segment is the last segment -> one neighbour
      neighbours.add( m_segmentList.get( currentSegmentNum - 2 ) );

    }
    else if( currentSegmentNum == 1 & currentSegmentNum == numOfSegments )
    {
      // there is only one segment, no neighbours
    }
    else if( currentSegmentNum == 1 & currentSegmentNum < numOfSegments )
    {
      // the current segment is the first segment -> one neighbour
      neighbours.add( m_segmentList.get( currentSegmentNum ) );
    }
    return neighbours;
  }

}
