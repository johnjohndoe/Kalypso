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

import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.apache.commons.io.IOUtils;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DDebug;
import org.kalypso.kalypsomodel1d2d.schema.binding.results.INodeResult;

/**
 * @author Thomas Jung
 * 
 */
public class HMOTriangleEater implements ITriangleEater
{
  private final Map<INodeResult, Integer> m_nodes = new LinkedHashMap<INodeResult, Integer>();

  private final List<Integer[]> m_triangles = new LinkedList<Integer[]>();

  private int m_nodeIndex = 0;

  private final File m_output;

  private final ResultType.TYPE m_parameter;

  public HMOTriangleEater( final File filename, final ResultType.TYPE parameter )
  {
    m_output = filename;
    m_parameter = parameter;
  }

  @Override
  public void add( final INodeResult... nodes )
  {
    final Integer[] triangle = new Integer[3];

    /* add nodes to node list */
    for( int i = 0; i < nodes.length; i++ )
    {
      final INodeResult nodeResult = nodes[i];

      if( m_nodes.containsKey( nodeResult ) == false )
      {
        m_nodeIndex = m_nodeIndex + 1;
        m_nodes.put( nodeResult, m_nodeIndex );
      }

      triangle[i] = m_nodes.get( nodeResult );
    }
    /* add the triangle to the list */
    m_triangles.add( triangle );
  }

  private void writeHMOFile( final File paramFile ) throws IOException
  {
    /* node list */
    BufferedWriter buf = null;
    try
    {
      buf = new BufferedWriter( new FileWriter( paramFile ) );
      for( final INodeResult node : m_nodes.keySet() )
      {
        final double x = node.getPoint().getX();
        final double y = node.getPoint().getY();
        double z;

        switch( m_parameter )
        {
          case VELOCITY:
            z = node.getAbsoluteVelocity();

            break;
          case WATERLEVEL:
            z = node.getWaterlevel();

            break;
          case DEPTH:
            z = node.getWaterlevel();

            break;

          default:
            z = node.getPoint().getZ();
            break;
        }

        final int nodeID = m_nodes.get( node );

        buf.write( "P:  " + nodeID + "  " + x + "  " + y + "  " + z + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

        // KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s %d %f20.8 %f20.8 %f20.8%n", "HMOTriangleEater: P: ", nodeID,
        // x, y, z );
      }
      /* triangle list */
      for( int i = 0; i < m_triangles.size(); i++ )
      {
        Integer[] triangle = new Integer[3];

        triangle = m_triangles.get( i );

        final int triangleID = i + 1;
        final int n1 = triangle[0];
        final int n2 = triangle[1];
        final int n3 = triangle[2];
        buf.write( "D:  " + triangleID + "  " + n1 + "  " + n2 + "  " + n3 + "\n" ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$ //$NON-NLS-5$

        // KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s %d %d %d %d%n", "HMOTriangleEater: D: ", triangleID, n1, n2,
        // n3 );
      }
      buf.close();
    }
    finally
    {
      IOUtils.closeQuietly( buf );
    }
  }

  @Override
  public void finished( )
  {
    final String name = m_output.getPath();

    final int extensionIndex = name.lastIndexOf( "." ); //$NON-NLS-1$

    final String substring = name.substring( 0, extensionIndex );
    final String extension = name.substring( extensionIndex, name.length() );

    /* create filename */
    final String param = m_parameter.name();
    // FIXME: change to file name based on time, we have no timestep any more
    final int timestep = 0;
    final String paramName = substring + "_" + param + "_" + timestep + extension; //$NON-NLS-1$ //$NON-NLS-2$
    final File paramFile = new File( paramName );

    try
    {
      writeHMOFile( paramFile );
    }
    catch( final IOException e )
    {
      KalypsoModel1D2DDebug.TRIANGLEEATER.printf( "%s", "HMOTriangleEater: error while finishing eater." ); //$NON-NLS-1$ //$NON-NLS-2$

      e.printStackTrace();
    }

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.results.ITriangleEater#setTime(java.util.Date)
   */
  @Override
  public void setTime( final Date time )
  {
  }
}
