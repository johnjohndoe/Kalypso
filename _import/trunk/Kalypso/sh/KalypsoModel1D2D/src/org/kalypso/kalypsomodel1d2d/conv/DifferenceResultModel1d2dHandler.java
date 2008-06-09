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
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType;
import org.kalypso.kalypsomodel1d2d.conv.results.ResultType.TYPE;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_TriangulatedSurface;
import org.kalypsodeegree_impl.model.geometry.GeometryFactory;

/**
 * @author Thomas Jung
 * 
 */
public class DifferenceResultModel1d2dHandler implements IRMA10SModelElementHandler
{

  private Formatter m_formatter = null;

  private int m_lastnodeID;

  private double m_elevation;

  private double m_easting;

  private double m_northing;

  private final GM_TriangulatedSurface[] m_minuendSurfaces;

  private final GM_TriangulatedSurface[] m_subtrahentSurfaces;

  private final TYPE[] m_types;

  public DifferenceResultModel1d2dHandler( final File outputFile, GM_TriangulatedSurface[] minuendSurfaces, GM_TriangulatedSurface[] subtrahentSurfaces, TYPE[] types )
  {
    m_minuendSurfaces = minuendSurfaces;
    m_subtrahentSurfaces = subtrahentSurfaces;
    m_types = types;

    try
    {
      // REMARK: Made a central formatter with US locale (causing decimal point to be '.'),
      // so no locale parameter for each format is needed any more .
      m_formatter = new Formatter( outputFile, Charset.defaultCharset().name(), Locale.US );

    }
    catch( IOException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#end()
   */
  public void end( )
  {
    if( m_formatter != null )
    {
      try
      {
        FormatterUtils.checkIoException( m_formatter );
      }
      catch( IOException e )
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
  public void handle1dJunctionInformation( String line, int junctionId, List<Integer> junctionNodeIDList )
  {
    m_formatter.format( "%s%n", line );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleArc(java.lang.String, int, int, int, int,
   *      int, int)
   */
  public void handleArc( String lineString, int id, int node1ID, int node2ID, int elementLeftID, int elementRightID, int middleNodeID )
  {
    m_formatter.format( "%s%n", lineString );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleElement(java.lang.String, int, int, int,
   *      int)
   */
  public void handleElement( String lineString, int id, int currentRougthnessClassID, int previousRoughnessClassID, int eleminationNumber )
  {
    m_formatter.format( "%s%n", lineString );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleError(java.lang.String,
   *      org.kalypso.kalypsomodel1d2d.conv.EReadError)
   */
  public void handleError( String lineString, EReadError errorHints )
  {
    m_formatter.format( "%s%n", lineString );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleFlowResitance(java.lang.String, int,
   *      double, double, double)
   */
  public void handleFlowResitance( String line, int id, double combinedLambda, double soilLambda, double vegetationLambda )
  {
    m_formatter.format( "%s%n", line );

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleJunction(java.lang.String, int, int, int,
   *      int)
   */
  public void handleJunction( String line, int junctionID, int element1dID, int boundaryLine2dID, int node1dID )
  {
    m_formatter.format( "%s%n", line );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNode(java.lang.String, int, double, double,
   *      double)
   */
  public void handleNode( String lineString, int id, double easting, double northing, double elevation )
  {
    m_formatter.format( "%s%n", lineString );

    m_lastnodeID = id;
    m_easting = easting;
    m_northing = northing;
    m_elevation = elevation;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleNodeInformation(java.lang.String, int, int,
   *      double, double, double, double)
   */
  public void handleNodeInformation( String line, int id, int dry, double value1, double value2, double value3, double value4 )
  {
    m_formatter.format( "%s%n", line );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleResult(java.lang.String, int, double,
   *      double, double, double)
   */
  public void handleResult( String lineString, int id, double vx, double vy, double depth, double waterlevel )
  {

    Assert.isLegal( id == m_lastnodeID );

    final Map<TYPE, Double> valueMap = new HashMap<TYPE, Double>();

    // get new result from tins
    for( int i = 0; i < m_types.length; i++ )
    {
      final double minuendValue = m_minuendSurfaces[i].getValue( GeometryFactory.createGM_Position( m_easting, m_northing, m_elevation ) );
      final double subtrahentValue = m_subtrahentSurfaces[i].getValue( GeometryFactory.createGM_Position( m_easting, m_northing, m_elevation ) );
      double diffValue = 0.0;

      if( !Double.isNaN( minuendValue ) && !Double.isNaN( subtrahentValue ) )
        diffValue = minuendValue - subtrahentValue;
      else
        diffValue = 0.0;

      valueMap.put( m_types[i], diffValue );
    }

    final Double diffVx = valueMap.get( ResultType.TYPE.VELOCITY_X );
    vx = diffVx == null ? vx : diffVx;

    final Double diffVy = valueMap.get( ResultType.TYPE.VELOCITY_Y );
    vy = diffVy == null ? vy : diffVy;

    final Double diffDepth = valueMap.get( ResultType.TYPE.DEPTH );
    depth = diffDepth == null ? depth : diffDepth;

    final Double diffWaterlevel = valueMap.get( ResultType.TYPE.WATERLEVEL );
    waterlevel = diffWaterlevel == null ? waterlevel : diffWaterlevel;

    m_formatter.format( "VA%10d%20.7f%20.7f%20.7f%20.7f%n", id, vx, vy, depth, waterlevel ); //$NON-NLS-1$
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handleTime(java.lang.String, java.util.Date)
   */
  public void handleTime( String line, Date time )
  {
    m_formatter.format( "%s%n", line );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#handlerUnIdentifyable(java.lang.String)
   */
  public void handlerUnIdentifyable( String lineString )
  {
    m_formatter.format( "%s%n", lineString );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#setIRoughnessIDProvider(org.kalypso.kalypsomodel1d2d.conv.IRoughnessIDProvider)
   */
  public void setIRoughnessIDProvider( IRoughnessIDProvider roughnessIDProvider ) throws IllegalArgumentException
  {
    // TODO Auto-generated method stub

  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.conv.IRMA10SModelElementHandler#start()
   */
  public void start( )
  {
    // TODO Auto-generated method stub

  }

}
