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
package org.kalypso.kalypsomodel1d2d.conv.results;

import java.io.File;
import java.util.Date;
import java.util.List;

import javax.xml.datatype.XMLGregorianCalendar;
import javax.xml.namespace.QName;

import org.kalypso.contribs.java.util.DateUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.schema.UrlCatalog1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.kalypsodeegree_impl.model.geometry.GM_TriangulatedSurface_Impl;

/**
 * @author Thomas Jung
 */
public class TriangulatedSurfaceTriangleEater implements ITriangleEater
{
  private final GM_TriangulatedSurface m_surface;

  private ResultType.TYPE m_parameter = null;

  private GMLWorkspace m_workspace = null;

  private File m_tinResultFile = null;

  public TriangulatedSurfaceTriangleEater( final File tinResultFile, final GMLWorkspace workspace, final GM_TriangulatedSurface surface, final ResultType.TYPE parameter )
  {
    m_surface = surface;
    m_parameter = parameter;
    m_workspace = workspace;
    m_tinResultFile = tinResultFile;
  }

  public TriangulatedSurfaceTriangleEater( String crs ) throws GM_Exception
  {
    m_surface = new GM_TriangulatedSurface_Impl( crs );
  }

  /**
   * add a triangle to the eater. The triangle is defined by its three nodes ({@link INodeResult} and a information, if
   * the triangle is marked as wet or dry.
   * 
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#add(java.util.List)
   */
  public void add( final List<INodeResult> nodes, final Boolean isWet )
  {
    if( nodes.size() < 3 )
      return;
    GM_Triangle_Impl gmTriangle = null;
    GM_Position pos[] = null;

    final String crs = nodes.get( 0 ).getPoint().getCoordinateSystem();
    if( m_parameter != null )
    {
      if( isWet == true )
      {
        // process the wet triangles in order to get data only inside the inundation area
        pos = processWetNodes( nodes );
      }
      else
      {
        if( m_parameter == ResultType.TYPE.TERRAIN )
        {
          // for fem terrain data add the dry triangles as well
          pos = processNodes( nodes );
        }
        else
        {
          // TODO Case not covered, pos is null!
        }
      }
    }
    else
    {
      // if no parameter is set, add all triangles
      pos = processNodes( nodes );
    }

    try
    {
      if( pos != null )
        gmTriangle = new GM_Triangle_Impl( pos[0], pos[1], pos[2], crs );
    }
    catch( final GM_Exception e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", "TriangulatedSurfaceTriangleEater: error while adding nodes to eater (GM_Exception)." );
      e.printStackTrace();
    }

    if( gmTriangle != null )
      m_surface.add( gmTriangle );

  }

  private GM_Position[] processNodes( final List<INodeResult> nodes )
  {
    final GM_Position pos[] = new GM_Position[3];

    for( int i = 0; i < nodes.size(); i++ )
    {
      final double x = nodes.get( i ).getPoint().getX();
      final double y = nodes.get( i ).getPoint().getY();
      final double z = nodes.get( i ).getPoint().getZ();

      pos[i] = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( x, y, z );
    }

    return pos;
  }

  private GM_Position[] processWetNodes( final List<INodeResult> nodes )
  {
    final GM_Position pos[] = new GM_Position[3];

    for( int i = 0; i < nodes.size(); i++ )
    {
      final double x = nodes.get( i ).getPoint().getX();
      final double y = nodes.get( i ).getPoint().getY();
      double z;

      if( m_parameter != null )
      {
        switch( m_parameter )
        {
          case VELOCITY:
            z = nodes.get( i ).getAbsoluteVelocity();

            break;
          case WATERLEVEL:
            z = nodes.get( i ).getWaterlevel();

            break;
          case DEPTH:
            z = nodes.get( i ).getDepth();
            break;

          case TERRAIN:
            z = nodes.get( i ).getPoint().getZ();

            break;

          case SHEARSTRESS:

            // get the node shear stress depending on nodes velocity and
            // the averaged lambda value (derived by the neighboring elements).
            final double lambda = nodes.get( i ).getAveragedLambda();

            // get velocity
            final double vAbs = nodes.get( i ).getAbsoluteVelocity();
            // calculate shearstress
            z = lambda * 0.125 * 1000 * vAbs * vAbs;

            break;

          default:
            z = nodes.get( i ).getPoint().getZ();
            break;

        }
      }
      else
        z = nodes.get( i ).getPoint().getZ();

      pos[i] = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( x, y, z );
    }
    return pos;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#finished()
   */
  public void finished( )
  {
    if( m_surface.size() == 0 || m_tinResultFile == null || m_workspace == null )
      return;

    final String name = m_tinResultFile.getPath();

    final int extensionIndex = name.lastIndexOf( "." );

    final String substring = name.substring( 0, extensionIndex );
    final String extension = name.substring( extensionIndex, name.length() );

    /* create filename */
    String param;
    if( m_parameter != null )
      param = m_parameter.name();
    else
      param = "";

    final String fileName = substring + "_" + param + extension;

    try
    {
      // TODO: zip Url + stream
      final File paramFile = new File( fileName );
      if( extension.equals( ".zip" ) )
      {

        GmlSerializer.serializeWorkspaceToZipFile( paramFile, m_workspace, "tin_" + param + ".gml" );
      }
      else
      {
        GmlSerializer.serializeWorkspace( paramFile, m_workspace, "CP1252" );
      }
    }
    catch( final Exception e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", "TriangulatedSurfaceTriangleEater: error while finishing eater ." );
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#setTime(java.util.Date)
   */
  public void setTime( final Date date )
  {
    if( m_workspace == null )
      return;

    final Feature triangleFeature = m_workspace.getRootFeature();
    if( triangleFeature != null )
    {
      final XMLGregorianCalendar gregorianCalendar = DateUtilities.toXMLGregorianCalendar( date );
      triangleFeature.setProperty( new QName( UrlCatalog1D2D.MODEL_1D2DResults_NS, "date" ), gregorianCalendar );
    }
  }

  public void add( final List<GM_Point> nodeList )
  {
    final String crs = nodeList.get( 0 ).getCoordinateSystem();

    GM_Triangle_Impl gmTriangle = null;

    final GM_Position pos[] = new GM_Position[3];

    for( int i = 0; i < nodeList.size(); i++ )
    {
      final GM_Point point = nodeList.get( i );
      pos[i] = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( point.getX(), point.getY(), point.getZ() );
    }

    try
    {
      // for shape export we need a clockwise orientation, the algorithm that delivers the nodes is the nodes to the
      // eater is thinking counter-clockwise.
      // for that reason we switch the positions order to get that clockwise orientation.
      gmTriangle = new GM_Triangle_Impl( pos[2], pos[1], pos[0], crs );
    }
    catch( final GM_Exception e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", "TriangulatedSurfaceTriangleEater: error while adding nodes to eater (GM_Exception)." );
      e.printStackTrace();
    }

    if( gmTriangle != null )
      m_surface.add( gmTriangle );
  }

  public GM_TriangulatedSurface getSurface( )
  {
    return m_surface;
  }
}
