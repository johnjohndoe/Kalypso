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
import java.io.IOException;
import java.util.Date;
import java.util.List;

import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;
import org.kalypso.ogc.gml.serialize.GmlSerializeException;
import org.kalypso.ogc.gml.serialize.GmlSerializer;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GM_Triangle_Impl;
import org.opengis.cs.CS_CoordinateSystem;

/**
 * @author Thomas Jung
 * 
 */
public class TriangulatedSurfaceTriangleEater implements ITriangleEater
{
  private final GM_TriangulatedSurface m_surface;

  private final ResultType.TYPE m_parameter;

  private final GMLWorkspace m_workspace;

  private final File m_tinResultFile;

  public TriangulatedSurfaceTriangleEater( final File tinResultFile, final GMLWorkspace workspace, final GM_TriangulatedSurface surface, final ResultType.TYPE parameter )
  {
    m_surface = surface;
    m_parameter = parameter;
    m_workspace = workspace;
    m_tinResultFile = tinResultFile;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#add(java.util.List)
   */
  public void add( final List<INodeResult> nodes )
  {
    final GM_Position pos[] = new GM_Position[3];

    for( int i = 0; i < nodes.size(); i++ )
    {
      final double x = nodes.get( i ).getPoint().getX();
      final double y = nodes.get( i ).getPoint().getY();
      double z;

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

        default:
          z = nodes.get( i ).getPoint().getZ();
          break;

      }

      pos[i] = org.kalypsodeegree_impl.model.geometry.GeometryFactory.createGM_Position( x, y, z );
    }

    final CS_CoordinateSystem crs = nodes.get( 0 ).getPoint().getCoordinateSystem();

    GM_Triangle_Impl gmTriangle = null;

    try
    {
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

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#finished()
   */
  public void finished( )
  {
    final String name = m_tinResultFile.getPath();

    final int extensionIndex = name.lastIndexOf( "." );

    final String substring = name.substring( 0, extensionIndex );
    final String extension = name.substring( extensionIndex, name.length() );

    /* create filename */
    final String param = m_parameter.name();
    final String paramName = substring + "_" + param + extension;
    final File paramFile = new File( paramName );

    try
    {
      GmlSerializer.serializeWorkspace( paramFile, m_workspace, "UTF-8" );
    }
    catch( final IOException e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", "TriangulatedSurfaceTriangleEater: error while finishing eater (IOException)." );
      e.printStackTrace();
    }
    catch( final GmlSerializeException e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", "TriangulatedSurfaceTriangleEater: error while finishing eater (GmlSerializeException)." );
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#setTime(java.util.Date)
   */
  public void setTime( final Date time )
  {
  }
}
