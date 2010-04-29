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
package org.kalypso.kalypsomodel1d2d.ui.map.util;

import java.awt.Color;
import java.awt.Graphics;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.jts.QuadMesher.JTSQuadMesher;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.DiscretisationModelUtils;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.FE1D2DDiscretisationModel;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.ElementGeometryHelper;
import org.kalypso.kalypsomodel1d2d.ui.map.grid.LinePointCollector;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.KalypsoDeegreePlugin;
import org.kalypsodeegree.graphics.displayelements.DisplayElement;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.FeatureList;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_Ring;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree.model.geometry.GM_SurfacePatch;
import org.kalypsodeegree_impl.graphics.displayelements.DisplayElementFactory;
import org.kalypsodeegree_impl.graphics.sld.LineSymbolizer_Impl;
import org.kalypsodeegree_impl.graphics.sld.Stroke_Impl;
import org.kalypsodeegree_impl.model.geometry.JTSAdapter;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

import com.vividsolutions.jts.geom.Coordinate;
import com.vividsolutions.jts.geom.GeometryFactory;
import com.vividsolutions.jts.geom.LineString;

/**
 * This class provide the mechanism to calculate the grid finit element model node and to display them. The point are
 * supposed to be in the same coordinate reference system so that no no reference system convertion is done
 * 
 * @author Patrice Congo
 * @author Thomas Jung
 */
public class TempGrid
{
  /**
   * The target coordinate reference system for the created grid point
   */
  private String m_crs;

  /**
   * Cache for grid computed grid points
   */
  private GM_Point[][] m_gridPoints;

  private double m_searchRectWidth = 0.1;

  private IKalypsoFeatureTheme m_nodeTheme;

  /**
   * Set the target {@link CS_CoordinateSystem}. All points this grid is coping with are required to reside in that
   * system and no reference system conversion made in by the {@link TempGrid}
   * 
   * @param targetCrs
   *          the target {@link CS_CoordinateSystem}
   * @throws IllegalArgumentException
   *           is targetCrs is null
   */
  public void setCoodinateSystem( final String targetCrs ) throws IllegalArgumentException
  {
    Assert.throwIAEOnNullParam( targetCrs, "targetCrs" ); //$NON-NLS-1$
    m_crs = targetCrs;
  }

  /**
   * Class this to display the {@link TempGrid} on the screen.
   * 
   * @param g
   *          the display graphic context
   * @param projection
   *          the geoprojection for projecting {@link GM_Point} to screen points
   * @param pointRectSize
   *          the side lengtth for square representing the points
   */
  public void paint( final Graphics g, final GeoTransform projection )
  {
    if( m_gridPoints.length != 0 )
    {
      /* Paint a linestring. */
      try
      {
        paintEdges( g, projection );
      }
      catch( final Exception e )
      {
        // TODO Auto-generated catch block
        e.printStackTrace();
      }
    }

  }

  @SuppressWarnings("unchecked")
  private void paintEdges( final Graphics g, final GeoTransform projection ) throws GM_Exception, CoreException
  {
    final LineSymbolizer symb = new LineSymbolizer_Impl();
    final Stroke stroke = new Stroke_Impl( new HashMap(), null, null );

    Color color = new Color( 100, 100, 100 ); // default is it set to green
    if( isValid() != Status.OK_STATUS )
      color = new Color( 255, 100, 100 );

    stroke.setWidth( 1 );
    stroke.setStroke( color );
    symb.setStroke( stroke );
    DisplayElement de;

    final GM_Position[] pos = new GM_Position[2];

    for( final GM_Point[] element : m_gridPoints )
    {
      for( int j = 0; j < element.length - 1; j++ )
      {
        pos[0] = element[j].getPosition();
        pos[1] = element[j + 1].getPosition();
        final GM_Curve curve = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( pos, m_crs );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, projection, new NullProgressMonitor() );
      }
    }
    for( int j = 0; j < m_gridPoints[0].length; j++ )
    {
      for( int i = 0; i < m_gridPoints.length - 1; i++ )
      {
        pos[0] = m_gridPoints[i][j].getPosition();
        pos[1] = m_gridPoints[i + 1][j].getPosition();
        final GM_Curve curve = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Curve( pos, m_crs );
        de = DisplayElementFactory.buildLineStringDisplayElement( null, curve, symb );
        de.paint( g, projection, new NullProgressMonitor() );
      }
    }
  }

  public IStatus isValid( )
  {
    return checkElements();
  }

  /**
   * Reset this {@link TempGrid}. It is empty, i.e. contains no points after reset
   * 
   * @param crs
   *          the target coordinate reference system for the grid
   * @see #setCoodinateSystem(CS_CoordinateSystem)
   * @throws IllegalArgumentException
   *           if crs is null
   */
  public void resetTempGrid( final String crs ) throws IllegalArgumentException
  {
    m_gridPoints = new GM_Point[0][0];
    m_crs = crs;
  }

  /**
   * config the with its side points.
   * 
   * @param topSidePoints
   *          the collector containing the top side points
   * @param bottomSidePoints
   *          the collector containing the bottom side points
   * @param leftSidePoints
   *          the collector containing the to left side points
   * @param rightSidePoints
   *          the collector containing the to right side points
   * @throws IllegalArgumentException
   *           if one the the side point collector is null
   */
  public IStatus setTempGrid( final LinePointCollector topSidePoints, final LinePointCollector bottomSidePoints, final LinePointCollector leftSidePoints, final LinePointCollector rightSidePoints )
  {
    Assert.throwIAEOnNullParam( topSidePoints, "topSidePoints" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( bottomSidePoints, "bottomSidePoints" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( leftSidePoints, "leftSidePoints" ); //$NON-NLS-1$
    Assert.throwIAEOnNullParam( leftSidePoints, "leftSidePoints" ); //$NON-NLS-1$

    try
    {
      m_gridPoints = computeMesh( topSidePoints, bottomSidePoints, leftSidePoints, rightSidePoints );
      return Status.OK_STATUS;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.0" ) ); //$NON-NLS-1$
    }
  }

  /**
   * in case that the data comes already as mesh
   */
  public void importMesh( final GM_Point[][] importedGridPoints )
  {
    m_gridPoints = importedGridPoints;
  }

  /**
   * Computes the grid points from its side points
   */
  private final GM_Point[][] computeMesh( final LinePointCollector topSidePoints, final LinePointCollector bottomSidePoints, final LinePointCollector leftSidePoints, final LinePointCollector rightSidePoints ) throws GM_Exception
  {
    final LineString topLine = pointToLineString( topSidePoints );
    final LineString bottomLine = pointToLineString( bottomSidePoints );
    final LineString leftLine = pointToLineString( leftSidePoints );
    final LineString rightLine = pointToLineString( rightSidePoints );

    // compute mesh points
    final JTSQuadMesher mesher = new JTSQuadMesher( topLine, bottomLine, leftLine, rightLine );
    final Coordinate[][] coordinates = mesher.calculateMesh();
    final GeometryFactory geometryFactory = new GeometryFactory();
    final GM_Point points2D[][] = new GM_Point[coordinates.length][];
    for( int i = 0; i < coordinates.length; i++ )
    {
      final Coordinate[] line = coordinates[i];
      final GM_Point[] points1D = new GM_Point[line.length];
      points2D[i] = points1D;
      for( int j = 0; j < line.length; j++ )
      {
        final Coordinate coord = line[j];
        points1D[j] = (GM_Point) JTSAdapter.wrap( geometryFactory.createPoint( coord ) );
      }
    }
    return points2D;
  }

  /**
   * To get an {@link ICommand} that can be use to hat the temp grid to the model
   */
  public IStatus getAddToModelCommand( final IMapPanel panel, final IFEDiscretisationModel1d2d model, final CommandableWorkspace commandableWorkspace, final double searchRectWidth )
  {
    m_searchRectWidth = searchRectWidth;

    final GM_Point[][] points2D = m_gridPoints;

    if( points2D.length == 0 )
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.1" ) ); //$NON-NLS-1$

    // we must have the node theme. First node theme gets it
    final IKalypsoFeatureTheme nodeTheme = UtilMap.findEditableTheme( panel, Kalypso1D2DSchemaConstants.WB1D2D_F_NODE );
    if( nodeTheme != null && model != null && commandableWorkspace != null )
    {
      try
      {
        addElements( commandableWorkspace, nodeTheme );
      }
      catch( final Exception e )
      {
        e.printStackTrace();
        return StatusUtilities.statusFromThrowable( e, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.2" ) ); //$NON-NLS-1$
      }
    }
    else
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.3" ) ); //$NON-NLS-1$

    return Status.OK_STATUS;
  }

  private void addElements( final CommandableWorkspace workspace, final IKalypsoFeatureTheme nodeTheme ) throws Exception
  {
    final FeatureList featureList = nodeTheme.getFeatureList();
    final Feature parentFeature = featureList.getParentFeature();

    /* Initialize elements needed for edges and elements */
    final IFEDiscretisationModel1d2d discModel = new FE1D2DDiscretisationModel( parentFeature );
    final List<GM_Ring> elements = getRingsFromPoses();

    for( final GM_Ring ring : elements )
      ElementGeometryHelper.createFE1D2DfromRing( workspace, discModel, ring );
  }

  @SuppressWarnings("unchecked")
  private IStatus checkElements( )
  {
    if( m_nodeTheme == null )
      return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.4" ) ); //$NON-NLS-1$
    try
    {
      final IFEDiscretisationModel1d2d discModel = DiscretisationModelUtils.modelForTheme( m_nodeTheme );
      final List<GM_Ring> rings = getRingsFromPoses();
      for( final GM_Ring ring : rings )
      {
        // 4) New Element self-intersects
        if( GeometryUtilities.isSelfIntersecting( ring.getPositions() ) )
          return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.5" ) ); //$NON-NLS-1$

        // New Element intersects other elements
        final GM_Surface<GM_SurfacePatch> newSurface = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Surface( ring.getPositions(), new GM_Position[][] {}, null, KalypsoDeegreePlugin.getDefault().getCoordinateSystem() );
        final List<IFE1D2DElement> elements = discModel.getElements().query( newSurface.getEnvelope() );
        for( final IFE1D2DElement element : elements )
        {
          if( element instanceof IPolyElement )
          {
            final GM_Surface<GM_SurfacePatch> eleGeom = ((IPolyElement) element).getGeometry();
            if( eleGeom.intersects( newSurface ) )
            {
              final GM_Object intersection = eleGeom.intersection( newSurface );
              if( intersection instanceof GM_Surface )
                return StatusUtilities.createErrorStatus( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.util.TempGrid.6" ) ); //$NON-NLS-1$
            }
          }
        }
      }
      return Status.OK_STATUS;
    }
    catch( final GM_Exception e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }

    return Status.OK_STATUS;
  }

  private List<GM_Ring> getRingsFromPoses( ) throws GM_Exception
  {
    // TODO: here we can implement some nice checkies!
    // intersections
    // duplicate nodes
    // ...
    final List<GM_Ring> rings = new ArrayList<GM_Ring>();

    for( int i = 0; i < m_gridPoints.length - 1; i++ )
    {
    //the old version of this line was not so meaningful:
    //for( int j = 0; j < m_gridPoints[0].length - 1; j++ )
    //                                 ^
    //TODO: correct this if it was realy needed in this way
      for( int j = 0; j < m_gridPoints[ i ].length - 1; j++ )
      {
        final GM_Position[] poses = new GM_Position[5];
        poses[0] = m_gridPoints[i][j].getPosition();
        poses[1] = m_gridPoints[i + 1][j].getPosition();
        poses[2] = m_gridPoints[i + 1][j + 1].getPosition();
        poses[3] = m_gridPoints[i][j + 1].getPosition();
        poses[4] = m_gridPoints[i][j].getPosition();

        final GM_Position[] checkedPoses = checkPoses( poses );
        if( checkedPoses.length >= 4 && checkedPoses[0].equals( checkedPoses[checkedPoses.length - 1] ) )
          rings.add( org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Ring( checkedPoses, m_crs ) );
      }
    }
    return rings;
  }

  private GM_Position[] checkPoses( final GM_Position[] poses )
  {
    final List<GM_Position> posToDeleteList = new ArrayList<GM_Position>();
    final List<GM_Position> posList = new ArrayList<GM_Position>();

    for( int i = 0; i < poses.length - 1; i++ )
    {
      posList.add( poses[i] );

      /* check the distance to each other */
      for( int j = 0; j < poses.length - 1; j++ )
      {
        if( i != j )
        {
          final double distance = poses[i].getDistance( poses[j] );
          if( distance < 2 * m_searchRectWidth && !posToDeleteList.contains( poses[j] ) )
          {
            posToDeleteList.add( poses[i] );
          }
        }
      }
    }

    for( final GM_Position position : posToDeleteList )
    {
      posList.remove( position );
    }

    final GM_Position[] positions = posList.toArray( new GM_Position[posList.size() + 1] );
    positions[posList.size()] = positions[0];
    return positions;
  }

  /**
   * get the {@link LinePointCollector} points as {@link LineString}
   */
  private LineString pointToLineString( final LinePointCollector lineGeometryBuilder )
  {
    final int SIZE = lineGeometryBuilder.getCurrentPointCnt();
    final Coordinate coordinates[] = new Coordinate[SIZE];
    for( int i = 0; i < SIZE; i++ )
    {
      coordinates[i] = JTSAdapter.export( lineGeometryBuilder.getPointAt( i ).getPosition() );
    }
    // CoordinateSequence
    // JTSAdapter
    final GeometryFactory geometryFactory = new GeometryFactory();
    final LineString lineString = geometryFactory.createLineString( coordinates );
    return lineString;
  }

  public void setNodeTheme( final IKalypsoFeatureTheme nodeTheme )
  {
    m_nodeTheme = nodeTheme;
  }
}