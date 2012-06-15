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
package org.kalypso.kalypsomodel1d2d.conv;

import java.io.File;
import java.io.IOException;
import java.nio.charset.Charset;
import java.util.Date;
import java.util.Formatter;
import java.util.HashMap;
import java.util.List;
import java.util.Locale;
import java.util.Map;

import org.eclipse.core.runtime.Assert;
import org.kalypso.contribs.java.util.FormatterUtils;
import org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES;
import org.kalypso.kalypsomodel1d2d.conv.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType.TYPE;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Position;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 */
public class DifferenceResultModel1d2dHandler implements IRMA10SModelElementHandler
{

  private Formatter m_formatter = null;

  private int m_lastnodeID;

  private final GM_TriangulatedSurface[] m_minuendSurfaces;

  private final GM_TriangulatedSurface[] m_subtrahentSurfaces;

  private final TYPE[] m_resultTypes;

  private final org.kalypso.kalypsomodel1d2d.conv.results.differences.ResultCalculatorType.TYPE m_differenceType;

  private GM_Position m_nodePos;

  public DifferenceResultModel1d2dHandler( final File outputFile, final GM_TriangulatedSurface[] minuendSurfaces, final GM_TriangulatedSurface[] subtrahentSurfaces, final TYPE[] types, final org.kalypso.kalypsomodel1d2d.conv.results.differences.ResultCalculatorType.TYPE differenceType )
  {
    m_minuendSurfaces = minuendSurfaces;
    m_subtrahentSurfaces = subtrahentSurfaces;
    m_resultTypes = types;
    m_differenceType = differenceType;

    try
    {
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      m_formatter = new Formatter( outputFile, Charset.defaultCharset().name(), Locale.US );
    }
    catch( final IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  @Override
  public void end( )
  {
    if( m_formatter != null )
    {
      try
      {
        FormatterUtils.checkIoException( m_formatter );
      }
      catch( final IOException e )
      {
        e.printStackTrace();
      }
      finally
      {
        m_formatter.close();
      }
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#getCreatedFeatures()
   */
  public List<IFeatureWrapper2> getCreatedFeatures( )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handle1dJunctionInformation(java.lang.String,
   *      int, java.util.List)
   */
  @Override
  public void handle1dJunctionInformation( final String line, final int junctionId, final List<Integer> junctionNodeIDList )
  {
    m_formatter.format( "%s%n", line ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  @Override
  public void handleArc( final String lineString, final int id, final int node1ID, final int node2ID, final int elementLeftID, final int elementRightID, final int middleNodeID )
  {
    m_formatter.format( "%s%n", lineString ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleElement( final String lineString, final int id, final int currentRougthnessClassID, final int previousRoughnessClassID, final int eleminationNumber )
  {
    m_formatter.format( "%s%n", lineString ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  @Override
  public void handleError( final String lineString, final EReadError errorHints )
  {
    m_formatter.format( "%s%n", lineString ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleFlowResitance(java.lang.String, int,
   *      double, double, double)
   */
  @Override
  public void handleFlowResitance( final String line, final int id, final double combinedLambda, final double soilLambda, final double vegetationLambda )
  {
    m_formatter.format( "%s%n", line ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleJunction(java.lang.String, int, int, int,
   *      int)
   */
  @Override
  public void handleJunction( final String line, final int junctionID, final int element1dID, final int boundaryLine2dID, final int node1dID )
  {
    m_formatter.format( "%s%n", line ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  @Override
  public void handleNode( final String lineString, final int id, final double easting, final double northing, final double elevation )
  {
    m_formatter.format( "%s%n", lineString ); //$NON-NLS-1$

    m_lastnodeID = id;

    m_nodePos = GeometryFactory.createGM_Position( easting, northing, elevation );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNodeInformation(java.lang.String, int, int,
   *      double, double, double, double)
   */
  @Override
  public void handleNodeInformation( final String line, final int id, final int dry, final double value1, final double value2, final double value3, final double value4 )
  {
    m_formatter.format( "%s%n", line ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  @Override
  public void handleResult( final String lineString, final int id, double vx, double vy, double depth, double waterlevel )
  {
    Assert.isLegal( id == m_lastnodeID );

    final Map<TYPE, Double> valueMap = calculateDifferences( m_minuendSurfaces, m_subtrahentSurfaces, m_nodePos, m_resultTypes );

    // get the vector values of the difference vector (minuend - subtrahent)
    final Double diffVx = valueMap.get( ResultType.TYPE.VELOCITY_X );
    final Double diffVy = valueMap.get( ResultType.TYPE.VELOCITY_Y );

    // get the vector values of all vectors (we assume, that surface 0 is X-direction and surface 1 is Y-direction)
    final double secondaryX = m_subtrahentSurfaces[0].getValue( m_nodePos );
    final double secondaryY = m_subtrahentSurfaces[1].getValue( m_nodePos );

    // calculate the vector values for the projected vector
    final double faktorParallel = ((diffVx) * secondaryX + (diffVy) * secondaryY) / (secondaryX * secondaryX + secondaryY * secondaryY);

    // calculate the vector values for the orthogonal part
    final Double diffMainParallelX = faktorParallel * secondaryX;
    final Double diffMainParallelY = faktorParallel * secondaryY;

    switch( m_differenceType )
    {
      case VECTOR_DIFFERENCE:

        vx = diffVx == null ? vx : diffVx;
        vy = diffVy == null ? vy : diffVy;

        break;

      case VECTOR_DIFFERENCE_ORTHOGONAL:

        /* calculate the orthogonal projection on the main vector */

        if( !Double.isNaN( faktorParallel ) )
        {
          final Double diffMainOrthoX = diffVx - diffMainParallelX;
          final Double diffMainOrthoY = diffVy - diffMainParallelY;

          vx = diffMainOrthoX == null ? vx : diffMainOrthoX;
          vy = diffMainOrthoY == null ? vx : diffMainOrthoY;
        }

        break;

      case VECTOR_DIFFERENCE_PARALLEL:

        vx = diffMainParallelX == null ? vx : diffMainParallelX;
        vy = diffMainParallelY == null ? vy : diffMainParallelY;

        break;

      default:

        System.out.println( Messages.getString("org.kalypso.kalypsomodel1d2d.conv.DifferenceResultModel1d2dHandler.8") ); //$NON-NLS-1$
        break;
    }

    /* check the values */
    if( Double.isNaN( vx ) )
      vx = 0.0;

    if( Double.isNaN( vy ) )
      vy = 0.0;

    /* other values */
    final Double diffDepth = valueMap.get( ResultType.TYPE.DEPTH );
    depth = diffDepth == null ? depth : diffDepth;

    final Double diffWaterlevel = valueMap.get( ResultType.TYPE.WATERLEVEL );
    waterlevel = diffWaterlevel == null ? waterlevel : diffWaterlevel;

    m_formatter.format( "VA%10d%20.7f%20.7f%20.7f%20.7f%n", id, vx, vy, depth, waterlevel ); //$NON-NLS-1$
  }

  /**
   * Calculates the difference values of all given result types for the current position and returns them as a map. The
   * differences is computed as follows: minuend - subtrahent
   * 
   * @param minuendSurfaces
   *          the tins from which it will be substracted
   * @param subtrahentSurfaces
   *          the tins that give the values with which it will be substracted
   * @param pos
   *          the position at which the values are being read from the result tins.
   * @param resultTypes
   *          the given result tpyes
   */
  private static Map<TYPE, Double> calculateDifferences( final GM_TriangulatedSurface[] minuendSurfaces, final GM_TriangulatedSurface[] subtrahentSurfaces, final GM_Position pos, final TYPE[] resultTypes )
  {
    final Map<TYPE, Double> valueMap = new HashMap<TYPE, Double>();

    for( int i = 0; i < resultTypes.length; i++ )
    {
      final double minuendValue = minuendSurfaces[i].getValue( pos );
      final double subtrahentValue = subtrahentSurfaces[i].getValue( pos );
      double diffValue = 0.0;

      if( !Double.isNaN( minuendValue ) && !Double.isNaN( subtrahentValue ) )
        diffValue = minuendValue - subtrahentValue;
      else
        diffValue = 0.0;

      valueMap.put( resultTypes[i], diffValue );
    }
    return valueMap;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTime(java.lang.String, java.util.Date)
   */
  @Override
  public void handleTime( final String line, final Date time )
  {
    m_formatter.format( "%s%n", line ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  @Override
  public void handlerUnIdentifyable( final String lineString )
  {
    m_formatter.format( "%s%n", lineString ); //$NON-NLS-1$
  }
  
  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  @Override
  public void start( )
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTimeDependentAdditionalResult(java.lang.String, int, double, double, double, org.kalypso.kalypsomodel1d2d.conv.RMA10S2GmlConv.RESULTLINES)
   */
  @Override
  public void handleTimeDependentAdditionalResult( String lineString, int id, double vx, double vy, double depth, RESULTLINES resultlines )
  {
    // TODO Auto-generated method stub
    
  }

}
