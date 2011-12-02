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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.ISchedulingRule;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.contribs.eclipse.core.runtime.jobs.MutexRule;
import org.kalypso.gmlschema.GMLSchemaUtilities;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.gmlschema.property.IPropertyType;
import org.kalypso.jts.LineStringUtilities;
import org.kalypso.jts.QuadMesher.JTSCoordsElevInterpol;
import org.kalypso.jts.QuadMesher.JTSQuadMesher;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.model.wspm.core.gml.IProfileFeature;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.IKalypsoTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ui.editor.actions.FeatureComparator;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_LineString;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;
import com.vividsolutions.jts.geom.Point;

/**
 * State object for creating main channel widget and composite.
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

  private final Set<IProfileFeature> m_selectedProfiles = new HashSet<IProfileFeature>();

  private int m_numbProfileIntersections = 6;

  private final Map<GM_Curve, SIDE> m_selectedBanks = new HashMap<GM_Curve, SIDE>();

  private final CreateMainChannelWidget m_widget;

  private boolean datacomplete;

  private int m_globNumbBankIntersections;

  private final List<SegmentData> m_segmentList = new LinkedList<SegmentData>();

  private final List<Coordinate[][]> m_coordList = new LinkedList<Coordinate[][]>();

  private Coordinate[][] m_meshCoords;

  public SegmentData m_selectedSegment;

  private CreateChannelData.PROF m_selectedProfile;

  private Shell m_shell;

  private final Map<GM_Position, SegmentData> m_segmentMap = new HashMap<GM_Position, SegmentData>();

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

        if( featureType != null && GMLSchemaUtilities.substitutes( featureType, IProfileFeature.QN_PROFILE ) )
          goodThemes.add( fTheme );
      }
    }

    return goodThemes.toArray( new IKalypsoFeatureTheme[goodThemes.size()] );
  }

  /* --------------------- selection handling ---------------------------------- */

  public void changeSelectedProfiles( final IProfileFeature[] profileFeaturesToRemove, final IProfileFeature[] profileFeaturesToAdd )
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
    final Job job = new Job( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.0" ) ) //$NON-NLS-1$
    {
      @SuppressWarnings("synthetic-access")
      @Override
      protected IStatus run( final IProgressMonitor monitor )
      {
        // there must be at least two selected profiles and one selected bank.
        if( m_selectedBanks.size() > 1 && m_selectedProfiles.size() > 1 )
        {
          /* intersects the banks with the profiles and manages the initial segment creation */
          intersectBanksWithProfs( monitor ); // initial calculation of the segments by global parameters
          if( m_segmentList.size() > 0 )
            m_selectedSegment = m_segmentList.get( 0 );
        }
        else if( m_selectedProfiles.size() <= 1 )
        {
          m_segmentList.clear();
          m_coordList.clear();
          m_selectedSegment = null;
        }

        // final IProgressMonitor monitor = new NullProgressMonitor();
        m_widget.update();

        return Status.OK_STATUS;
      }
    };
    job.setRule( THE_SEGMENT_INIT_MUTEX );
    job.setUser( true );
    job.schedule();
  }

  public IProfileFeature[] getSelectedProfiles( )
  {
    return m_selectedProfiles.toArray( new IProfileFeature[m_selectedProfiles.size()] );
  }

  public GM_Curve getBanklineForSide( final SIDE side )
  {
    for( final Map.Entry<GM_Curve, SIDE> entry : m_selectedBanks.entrySet() )
    {
      final GM_Curve bankCurve = entry.getKey();
      final SIDE value = entry.getValue();
      if( value == side )
        return bankCurve;
    }

    return null;
  }

  public void removeBank( final GM_Curve curve )
  {
    m_selectedBanks.remove( curve );

    m_widget.update();

    m_selectedBanks.keySet().remove( curve );

    initSegments();
  }

  /* --------------------- profile chart handling ---------------------------------- */

  public IProfil getProfil( )
  {
    if( m_selectedProfiles == null )
      return null;

    if( m_selectedProfiles.size() <= 1 )
      return null;

    // TODO: make sure that the initial selection is set.
    if( getSelectedSegment() == null )
      return null;

    final SegmentData segment = m_selectedSegment;

    if( segment == null )
      return null;

    final IProfil profil;
    if( m_selectedProfile == PROF.UP )
      profil = segment.getProfilUpOrg();
    else
      profil = segment.getProfilDownOrg();

    return profil;
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
      datacomplete = true;
    else if( m_selectedProfiles.size() <= 1 )
    {
      m_segmentList.clear();
      m_coordList.clear();
    }

    if( datacomplete == false )
      m_segmentList.clear();

    if( m_segmentList.size() > 0 )
    /* if there are segments, start Quadmesher */
    // TODO: error handling implementation!!
    {
      manageQuadMesher();
      final IStatus status = mergeMeshList();
      if( status != Status.OK_STATUS && m_shell != null )
      {
        MessageDialog.openInformation( m_shell, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.1" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.2" ) ); //$NON-NLS-1$ //$NON-NLS-2$
      }
    }
    m_widget.getPanel().repaintMap();
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

    final IMapPanel mapPanel = m_widget.getPanel();
    final IFEDiscretisationModel1d2d model1d2d = UtilMap.findFEModelTheme( mapPanel );
    final IKalypsoFeatureTheme theme = UtilMap.findEditableTheme( mapPanel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    final CommandableWorkspace workspace = theme.getWorkspace();

    final TempGrid tempGrid = new TempGrid();
    tempGrid.importMesh( importingGridPoints );
    tempGrid.setCoodinateSystem( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
    final double searchDistance = 0.1;
    final IStatus status = tempGrid.getAddToModelCommand( mapPanel, model1d2d, workspace, searchDistance );

    // TODO: handle status!

  }

  /**
   * converts a Coordinate[][] array into a GM_Point[][] array
   */
  private GM_Point[][] convertToGMPoints( final Coordinate[][] meshCoords )
  {
    final GeometryFactory geometryFactory = new GeometryFactory();

    final GM_Point points2D[][] = new GM_Point[meshCoords.length][];
    for( int i = 0; i < meshCoords.length; i++ )
    {
      final Coordinate[] line = meshCoords[i];
      final GM_Point[] points1D = new GM_Point[line.length];
      points2D[i] = points1D;
      for( int j = 0; j < line.length; j++ )
      {
        final Coordinate coord = line[j];
        try
        {
          points1D[j] = (GM_Point) JTSAdapter.wrap( geometryFactory.createPoint( coord ) );
          points1D[j].setCoordinateSystem( KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
        }
        catch( final GM_Exception e )
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
      final SegmentData segment = m_segmentList.get( i );
      complete = segment.complete();
      if( complete == true )
      {
        /* arrange the lines for the mesher */
        /*
         * -the lines have to be oriented (clw or cclw), the order ist top (profile) - left (bank) - bottom (profile) -
         * right (bank) or top - right - bottom - left -the end point of a line must be the same as the start point of
         * the next line -the lines itself must be also oriented all in the same way (clw/cclw)
         */

        // at the end point of the profile line there should follow the start point of the next line
        LineString[] lines = new LineString[4];

        lines[0] = segment.getProfUpIntersLineString();
        lines[1] = segment.getBankRightInters();
        lines[2] = segment.getProfDownIntersLineString();
        lines[3] = segment.getBankLeftInters();
        lines = checkLineOrientation( lines );

        if( lines == null )
        {
          initSegments();
          completationCheck();
          return;
        }
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

  public boolean checkMesh( final int segmentNumber )
  {
    boolean check = false;
    if( m_coordList.get( segmentNumber ) != null )
      check = true;

    return check;
  }

  /**
   * erases the double coords and merges the coords list to an array of coords
   */
  private IStatus mergeMeshList( )
  {
    try
    {
      m_meshCoords = null;
      final int overallYCoordNum = calculateOverallYCoordNum();
      final int numX = m_numbProfileIntersections;
      final int numY = overallYCoordNum;
      final Coordinate[][] newCoords = new Coordinate[numX][numY];

      int coordYPosPointer = 0;
      int coordSegmentPointer = 0;

      // loop over all segments -> coords
      for( int i = 0; i < m_coordList.size(); i++ )
      {
        final Coordinate[][] coordinates = m_coordList.get( i );
        if( numX != coordinates.length )
          return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.3" ) ); //$NON-NLS-1$
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
      {
        System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.4" ) ); //$NON-NLS-1$
        return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.5" ) ); //$NON-NLS-1$
      }

      m_meshCoords = newCoords;

      return Status.OK_STATUS;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.6" ) ); //$NON-NLS-1$
    }
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

  private LineString[] checkLineOrientation( final LineString[] lines )
  {
    final LineString[] lineArray = new LineString[4];

    final GeometryFactory factory1 = new GeometryFactory();
    final Coordinate[] coords1 = new Coordinate[lines[0].getNumPoints()];

    final GeometryFactory factory2 = new GeometryFactory();
    final Coordinate[] coords2 = new Coordinate[lines[1].getNumPoints()];

    final GeometryFactory factory3 = new GeometryFactory();
    final Coordinate[] coords3 = new Coordinate[lines[2].getNumPoints()];

    final GeometryFactory factory4 = new GeometryFactory();
    final Coordinate[] coords4 = new Coordinate[lines[3].getNumPoints()];

    boolean error = false;

    // At first line[0] and line [1]
    // check if the lines lie already in a right orientation.

    Point startpoint1 = lines[0].getStartPoint();
    Point endpoint1 = lines[0].getEndPoint();
    Point startpoint2 = lines[1].getStartPoint();
    Point endpoint2 = lines[1].getEndPoint();

    if( endpoint1.distance( startpoint2 ) < 0.001 ) // the distance method checks only in a 2d way!
    {
      for( int i = 0; i < coords1.length; i++ )
        coords1[i] = lines[0].getCoordinateN( i );
      final LineString line1 = factory1.createLineString( coords1 );

      for( int i = 0; i < coords2.length; i++ )
        coords2[i] = lines[1].getCoordinateN( i );
      final LineString line2 = factory2.createLineString( coords2 );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }
    else if( endpoint1.distance( endpoint2 ) < 0.001 )
    {
      // switch line 2
      for( int i = 0; i < coords1.length; i++ )
        coords1[i] = lines[0].getCoordinateN( i );
      final LineString line1 = factory1.createLineString( coords1 );
      final LineString line2 = LineStringUtilities.changeOrientation( lines[1] );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }
    else if( startpoint1.distance( startpoint2 ) < 0.001 )
    {
      // switch line 1
      final LineString line1 = LineStringUtilities.changeOrientation( lines[0] );

      for( int i = 0; i < coords2.length; i++ )
        coords2[i] = lines[1].getCoordinateN( i );
      final LineString line2 = factory1.createLineString( coords2 );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }
    else if( startpoint1.distance( endpoint2 ) < 0.001 )
    {
      // switch both lines
      final LineString line1 = LineStringUtilities.changeOrientation( lines[0] );
      final LineString line2 = LineStringUtilities.changeOrientation( lines[1] );

      lineArray[0] = line1;
      lineArray[1] = line2;
    }

    if( lineArray[0] == null || lineArray[1] == null )
      return null;
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
        coords3[i] = lines[2].getCoordinateN( i );
      final LineString line3 = factory3.createLineString( coords3 );

      lineArray[2] = line3;
    }
    else if( endpoint2.distance( endpoint3 ) < 0.001 )
    {
      // switch line 3
      final LineString line3 = LineStringUtilities.changeOrientation( lines[2] );

      lineArray[2] = line3;
    }
    else
    {
      // this should actually never happen!!
      error = true;
      return null;
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
        coords4[i] = lines[3].getCoordinateN( i );
      final LineString line4 = factory4.createLineString( coords4 );

      lineArray[3] = line4;
    }
    else if( endpoint3.distance( endpoint4 ) < 0.001 )
    {
      // switch line 4
      final LineString line4 = LineStringUtilities.changeOrientation( lines[3] );

      lineArray[3] = line4;
    }
    else
    {
      // this should actually never happen!!
      error = true;
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.7" ) ); //$NON-NLS-1$
      return null;
    }

    // final check: the last point of the last line should be the same as the first point of the first line
    final Point endpoint = lineArray[3].getEndPoint();
    final Point startpoint = lineArray[0].getStartPoint();

    if( endpoint.distance( startpoint ) > 0.001 || error == true )
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.8" ) ); //$NON-NLS-1$

    return lineArray;
  }

  /**
   * all selected banks will be intersected by the selected profiles <br>
   * Input: all selected banks and profiles Output: intersected banks as linestrings (including the intersection point)
   */
  private void intersectBanksWithProfs( final IProgressMonitor monitor )
  {
    /* at first -> clear the segment list! */
    m_segmentList.clear();

    final Feature[] profileFeatures = m_selectedProfiles.toArray( new Feature[m_selectedProfiles.size()] );

    if( profileFeatures.length == 0 )
      return;

    monitor.beginTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.9" ), profileFeatures.length ); //$NON-NLS-1$

    final Feature firstFeature = profileFeatures[0];
    final IPropertyType stationProperty = firstFeature.getFeatureType().getProperty( IProfileFeature.QN_PROPERTY_STATION );
    Arrays.sort( profileFeatures, new FeatureComparator( firstFeature.getParent(), stationProperty ) );

    // loop over all profiles
    // take two neighbouring profiles create a segment for them

    IProfileFeature lastProfile = null;
    for( final Feature profileFeature : profileFeatures )
    {
      // get the profile line
      final IProfileFeature profile = (IProfileFeature) (profileFeature);

      monitor.subTask( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.10" ) + profile.getStation() ); //$NON-NLS-1$

      if( !checkIntersection( profile ) )
        continue;

      if( lastProfile != null )
      {

        // working on the segment
        final int numBankIntersections = getGlobNumBankIntersections();
        final SegmentData segment = new SegmentData( this, profile, lastProfile, m_selectedBanks, numBankIntersections );
        System.out.println( "up profile: " + lastProfile.getStation() ); //$NON-NLS-1$
        System.out.println( "down profile: " + profile.getStation() ); //$NON-NLS-1$

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

  private boolean checkIntersection( final IProfileFeature profile )
  {
    boolean check = true;

    final Set<GM_Curve> curves = m_selectedBanks.keySet();

    for( final GM_Curve curve : curves ) 
    {
      GM_LineString asLineString = null;
      GM_Curve lCurve = curve;
//      if( !lCurve.intersects( profile.getLine() ) && lCurve.intersection( profile.getLine() ) == null )
//      {
//        try
//        {
//          asLineString = curve.getAsLineString();
//        }
//        catch( GM_Exception e )
//        {
//          e.printStackTrace();
//        }
//        if( asLineString != null && asLineString.getNumberOfPoints() > 2
//            && asLineString.getPositions()[asLineString.getNumberOfPoints() - 1].equals( asLineString.getPositions()[asLineString.getNumberOfPoints() - 2] ) )
//        {
//          try
//          {
//            lCurve = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( Arrays.copyOf( asLineString.getPositions(), asLineString.getNumberOfPoints() - 1 ), curve.getCoordinateSystem() );
//          }
//          catch( GM_Exception e )
//          {
//            e.printStackTrace();
//          }
//        }
        if( !lCurve.intersects( profile.getLine() ) && lCurve.intersection( profile.getLine() ) == null )
        {
          check = false;
          break;
        }
//      }
    }
    return check;
  }

  public void paintAllSegments( final Graphics g, final IMapPanel mapPanel )
  {
    if( m_meshCoords != null )
    {
      try
      {
        paintEdges( m_meshCoords, g, mapPanel );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
      }
      // paintCoords( m_meshCoords, g, mapPanel );
    }
  }

  public void setNumProfileIntersections( final int numProfileIntersections )
  {
    if( m_numbProfileIntersections != numProfileIntersections )
    {
      m_numbProfileIntersections = numProfileIntersections;
      initSegments();
      updateSegments( false );
    }
  }

  public int getNumProfileIntersections( )
  {
    return m_numbProfileIntersections;
  }

  public void setGlobNumBankIntersections( final int globNumBankIntersections )
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

  @SuppressWarnings("unchecked")
  private void paintEdges( final Coordinate[][] coords, final Graphics g, final IMapPanel mapPanel ) throws GM_Exception, CoreException
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

    final Color grey = new Color( 100, 100, 100 );

    stroke.setWidth( 1 );
    stroke.setStroke( grey );
    symb.setStroke( stroke );
    DisplayElement de;

    final Coordinate[] lineCoords = new Coordinate[2];

    for( final Coordinate[] element : coords )
    {
      final GeometryFactory factory = new GeometryFactory();
      for( int j = 0; j < element.length - 1; j++ )
      {
        lineCoords[0] = element[j];
        lineCoords[1] = element[j + 1];
        final LineString line = factory.createLineString( lineCoords );
        final GM_Curve curve;

        if( lineCoords[1] == null || lineCoords[0] == null )
          curve = (GM_Curve) JTSAdapter.wrap( line );
        else
          curve = (GM_Curve) JTSAdapter.wrap( line );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, mapPanel.getProjection(), new NullProgressMonitor() );
      }
    }
    for( int j = 0; j < coords[0].length; j++ )
    {
      final GeometryFactory factory = new GeometryFactory();
      for( int i = 0; i < coords.length - 1; i++ )
      {
        lineCoords[0] = coords[i][j];
        lineCoords[1] = coords[i + 1][j];
        final LineString line = factory.createLineString( lineCoords );
        final GM_Curve curve = (GM_Curve) JTSAdapter.wrap( line );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, mapPanel.getProjection(), new NullProgressMonitor() );
      }
    }
  }

  public int getNumOfSegments( )
  {
    return m_segmentList.size();
  }

  public SegmentData getSelectedSegment( )
  {
    return m_selectedSegment;
  }

  public void setSelectedSegment( final int segmentIndex )
  {
    m_selectedSegment = m_segmentList.get( segmentIndex );
  }

  /**
   * returns the number of bank intersections for the current segment
   */
  public int getNumBankIntersections( final SegmentData segment )
  {
    return segment.getNumBankIntersections();
  }

  /**
   * sets the number of bank intersections for the current segment
   */
  public void setNumBankIntersections( final int segment, final int numIntersections )
  {
    m_segmentList.get( segment - 1 ).setNumBankIntersections( numIntersections );
  }

  /**
   * this method possibly updates the segment data and pushes the calculation of the mesh. inputs: boolean edit: false->
   * update of the bankline and profile intersections true: -> no data update
   */
  public void updateSegments( final boolean edit )
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
        final int i = 1;
        // segment.updateBank();
        // segment.updateProfile();
      }
    }
    completationCheck();
  }

  public void drawBankLines( final Graphics g )
  {
    final IMapPanel panel = m_widget.getPanel();

    for( final SegmentData segment : m_segmentList )
    {
      if( segment == null || m_segmentList.size() == 0 )
        continue;

      segment.paintBankLineLineString( panel, g, new Color( 20, 20, 255 ) );
    }
  }

  public void updateSegment( final boolean edit )
  {
    if( m_selectedSegment != null )
    {
      if( edit == false )
      {
        // intersect the bankline by the defined number of intersections
        m_selectedSegment.updateBankIntersection();
      }
      else
      {
        // commits the done edits
        m_selectedSegment.updateBankIntersection2();
      }

      m_selectedSegment.updateProfileIntersection();
    }
    completationCheck();

  }

  public final CreateChannelData.PROF getCurrentProfile( )
  {
    return m_selectedProfile;
  }

  public void setCurrentProfile( final CreateChannelData.PROF profile )
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

  public CreateChannelData.PROF getCurrentProfilePlace( final double station )
  {
    final double stationUp = m_selectedSegment.getProfilUpOrg().getStation();
    final double stationDown = m_selectedSegment.getProfilDownOrg().getStation();

    if( stationUp == station )
      return CreateChannelData.PROF.UP;
    else if( stationDown == station )
      return CreateChannelData.PROF.DOWN;

    return null;
  }

  public void setShell( final Shell shell )
  {
    m_shell = shell;
  }

  public SegmentData getSegmentAtListPos( final int selection )
  {
    if( m_segmentList.size() == 0 )
      return null;

    if( m_segmentList.size() < selection )
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateChannelData.11" ) ); //$NON-NLS-1$

    return m_segmentList.get( selection );
  }

  public List<SegmentData> getNeighbourSegments( final SegmentData segment )
  {
    final List<SegmentData> neighbours = new LinkedList<SegmentData>();

    /* check the neighbours */
    final int currentIndex = m_segmentList.indexOf( segment );
    final int lastIndex = m_segmentList.size() - 1;

    if( currentIndex > 0 & currentIndex < lastIndex )
    {
      // the current segment lies inbetween -> two neighbours
      neighbours.add( m_segmentList.get( currentIndex - 1 ) ); // neighbour before
      neighbours.add( m_segmentList.get( currentIndex + 1 ) ); // neighbour after
    }
    else if( currentIndex > 0 & currentIndex == lastIndex )
    {
      // the current segment is the last segment -> one neighbour
      neighbours.add( m_segmentList.get( currentIndex - 1 ) );

    }
    else if( currentIndex == 0 & currentIndex == lastIndex )
    {
      // there is only one segment, no neighbours
    }
    else if( currentIndex == 0 & currentIndex < lastIndex )
    {
      // the current segment is the first segment -> one neighbour
      neighbours.add( m_segmentList.get( currentIndex + 1 ) );
    }
    return neighbours;
  }

  /**
   * Returns the list position of the selected {@link SegmentData}.
   */
  public int getSelectedSegmentPos( )
  {
    return m_segmentList.indexOf( m_selectedSegment );
  }

  /**
   * returns the {@link GM_Position}s of already intersected banklines for aall {@link SegmentData}s.<BR>
   * The start and end points of the banklines will be neglected.
   */
  public GM_Position[] getAllSegmentPosses( )
  {
    final List<GM_Position> posList = new ArrayList<GM_Position>();

    for( final SegmentData segment : m_segmentList )
    {
      final List<GM_Position> segmentPosses = getIntersectedBanklinePossesForSegment( segment );

      posList.addAll( segmentPosses );
    }
    return posList.toArray( new GM_Position[posList.size()] );
  }

  /**
   * returns the {@link GM_Position}s of already intersected banklines for a given {@link SegmentData}.<BR>
   * The start and end points of the banklines will be neglected.
   */
  public List<GM_Position> getIntersectedBanklinePossesForSegment( final SegmentData segment )
  {
    final List<GM_Position> positionList = new ArrayList<GM_Position>();

    final Coordinate[] bankLeftInters = segment.getBankLeftInters().getCoordinates();
    final Coordinate[] bankRightInters = segment.getBankRightInters().getCoordinates();
    for( int i = 1; i < bankRightInters.length - 1; i++ )
    {
      final GM_Position pos = JTSAdapter.wrap( bankRightInters[i] );
      m_segmentMap.put( pos, segment );
      positionList.add( pos );
    }

    for( int i = 1; i < bankLeftInters.length - 1; i++ )
    {
      final GM_Position pos = JTSAdapter.wrap( bankLeftInters[i] );
      m_segmentMap.put( pos, segment );
      positionList.add( pos );
    }

    return positionList;
  }

  /**
   * returns the {@link SegmentData} at a given {@link GM_Position}
   */
  public SegmentData getSegmentAtPosition( final GM_Position position )
  {
    return m_segmentMap.get( position );
  }

  /**
   * sets the bankline for a given side.<BR>
   * Before the new line is set the already existing bankline gets deleted as we allow only one line per side.
   * 
   * @param curve
   *          the bankline
   * @param side
   *          the side of the bankline
   */
  public void setBankline( final GM_Curve curve, final SIDE side )
  {
    /*
     * clear all existing banklines of the current side, because we allow only one bankline to be drawn for each side
     */

    final Set<GM_Curve> keySet = m_selectedBanks.keySet();

    final GM_Curve[] curves = keySet.toArray( new GM_Curve[keySet.size()] );
    for( final GM_Curve curve2 : curves )
    {
      if( m_selectedBanks.get( curve2 ).equals( side ) )
        m_selectedBanks.remove( curve2 );
    }

    // set the new bankline
    m_selectedBanks.put( curve, side );

    initSegments();
  }

  /**
   * returns the two original bankline curves as {@link GM_Position}s
   */
  public GM_Position[] getBanklinePoses( final SIDE m_side )
  {
    final GM_Curve bank = getBanklineForSide( m_side );

    final List<GM_Position> posList = new ArrayList<GM_Position>();

    try
    {
      if( bank != null )
      {

        final GM_Position[] leftPositions = bank.getAsLineString().getPositions();
        for( final GM_Position position : leftPositions )
          posList.add( position );
      }
    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return posList.toArray( new GM_Position[posList.size()] );
  }

}
