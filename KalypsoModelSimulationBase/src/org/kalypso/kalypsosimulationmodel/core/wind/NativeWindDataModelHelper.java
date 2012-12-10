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
package org.kalypso.kalypsosimulationmodel.core.wind;

import java.io.File;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.deegree.framework.util.Pair;
import org.kalypso.contribs.java.util.CollectionsHelper;
import org.kalypso.kalypsosimulationmodel.core.Assert;
import org.kalypso.kalypsosimulationmodel.i18n.Messages;
import org.kalypsodeegree.model.coverage.GridRange;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree.model.geometry.GM_Triangle;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain;
import org.kalypsodeegree_impl.gml.binding.commons.RectifiedGridDomain.OffsetVector;
import org.kalypsodeegree_impl.model.cv.GridRange_Impl;
import org.kalypsodeegree_impl.tools.GeometryUtilities;

public class NativeWindDataModelHelper
{

  private NativeWindDataModelHelper( )
  {
  }

  public static final IWindDataProvider getWindDataModel( final File pNativeModelFile, final RectifiedGridDomain pGridDescriptor ) throws Exception
  {
    Assert.throwIAEOnNullParam( pNativeModelFile, "nativeWindDataModelFile_" ); //$NON-NLS-1$
    // Decoding the White Spaces present in the File Paths. Sometimes requires to decode twice.
    // One particular case is having %2520 instead of a single white space.
    final File nativeModelFileEncoded = new File( URLDecoder.decode( pNativeModelFile.toString(), "UTF-8" ) ); //$NON-NLS-1$
    final File nativeModelFile = new File( URLDecoder.decode( nativeModelFileEncoded.toString(), "UTF-8" ) ); //$NON-NLS-1$
    if( nativeModelFile.isDirectory() )
    {
      return null;
    }
    if( !nativeModelFile.exists() )
    {
      return null;
    }
    return resolveWindDataModel( nativeModelFile, pGridDescriptor );
  }

  private static final IWindDataProvider resolveWindDataModel( final File ascFile, final RectifiedGridDomain gridDescriptor ) throws Exception
  {
    final String filePath = ascFile.getAbsolutePath();
    //    if( filePath.endsWith( ".asc" ) ) //$NON-NLS-1$
    // {
    // final ASCWindDataModel lWindDataModel = new ASCWindDataModel( ascFile.toURI().toURL(), gridDescriptor );
    // return lWindDataModel;
    // }
    // else
    if( filePath.endsWith( ".bin" ) ) //$NON-NLS-1$
    {
      final BinaryWindDataModel lWindDataModel = new BinaryWindDataModel( ascFile.toURI().toURL(), gridDescriptor );
      return lWindDataModel;
    }
    else
    {
      throw new RuntimeException( Messages.getString( "org.kalypso.kalypsosimulationmodel.core.terrainmodel.NativeTerrainElevationModelFactory.10" ) + filePath ); //$NON-NLS-1$
    }
  }

  public static RectifiedGridDomain createGridDescriptor( final GM_Point pGMPointOrigin, final int pIntCol, final int pIntRow, final double pDoubleCellXLen, final double pDoubleCellYLen )
  {
    final OffsetVector offsetX = new OffsetVector( pDoubleCellXLen, 0 );
    final OffsetVector offsetY = new OffsetVector( 0, -pDoubleCellYLen );

    final double[] low = { 0.0, 0.0 };
    final double[] high = { pIntCol, pIntRow };
    final GridRange gridRange = new GridRange_Impl( low, high );

    try
    {
      return new RectifiedGridDomain( pGMPointOrigin, offsetX, offsetY, gridRange );
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  /**
   * @return {@link Pair} with first value as modulo of speed value and second as angle in degrees. if windAsVector
   *         parameter is null result value will be also null.
   */
  public static Pair<Double, Double> convertVectorWindToSpeedAndDirection( final Pair<Double, Double> windAsVector )
  {
    if( windAsVector == null )
    {
      return null;
    }
    double lDoubleSpeedValue = Math.sqrt( windAsVector.first * windAsVector.first + windAsVector.second * windAsVector.second );
    // double lDoubleSpeedAngle = GeometryUtilities.directionFromVector( windAsVector.first, windAsVector.second );
    double lDoubleSpeedAngle = (180 / Math.PI) * Math.atan2( windAsVector.second, windAsVector.first );
    return new Pair<Double, Double>( lDoubleSpeedValue, lDoubleSpeedAngle );
  }

  public static Pair<Double, Double> getInterpolatedPair( final GM_Point pPointToInterpolateAt, final Map<GM_Point, Pair<Double, Double>> pMapPointsValues )
  {
    GM_Triangle lTriFirst = null;
    GM_Triangle lTriSecond = null;
    try
    {
      List<GM_Point> lListAllPoints = new ArrayList<GM_Point>();
      List<Double> lListAllFirsts = new ArrayList<Double>();
      List<Double> lListAllSeconds = new ArrayList<Double>();

      Set<GM_Point> lSetPoints = pMapPointsValues.keySet();
      for( Iterator<GM_Point> iterator = lSetPoints.iterator(); iterator.hasNext(); )
      {
        GM_Point gmPoint = iterator.next();
        lListAllPoints.add( gmPoint );
        lListAllFirsts.add( pMapPointsValues.get( gmPoint ).first );
        lListAllSeconds.add( pMapPointsValues.get( gmPoint ).second );
      }
      Map<GM_Point, Double> lMapFirst = CollectionsHelper.joinListsToMap( lListAllPoints.subList( 0, 3 ), lListAllFirsts.subList( 0, 3 ) );
      lTriFirst = GeometryUtilities.createTriangleForBilinearInterpolation( lMapFirst );
      if( lTriFirst.contains( pPointToInterpolateAt ) )
      {
        Map<GM_Point, Double> lMapSecond = CollectionsHelper.joinListsToMap( lListAllPoints.subList( 0, 3 ), lListAllSeconds.subList( 0, 3 ) );
        lTriSecond = GeometryUtilities.createTriangleForBilinearInterpolation( lMapSecond );
        return new Pair<Double, Double>( lTriFirst.getValue( pPointToInterpolateAt.getPosition() ), lTriSecond.getValue( pPointToInterpolateAt.getPosition() ) );
      }
      else
      {
        lListAllPoints.remove( 1 );
        lListAllFirsts.remove( 1 );
        lListAllSeconds.remove( 1 );
        lMapFirst = CollectionsHelper.joinListsToMap( lListAllPoints, lListAllFirsts );
        lTriFirst = GeometryUtilities.createTriangleForBilinearInterpolation( lMapFirst );
        Map<GM_Point, Double> lMapSecond = CollectionsHelper.joinListsToMap( lListAllPoints, lListAllSeconds );
        lTriSecond = GeometryUtilities.createTriangleForBilinearInterpolation( lMapSecond );
        return new Pair<Double, Double>( lTriFirst.getValue( pPointToInterpolateAt.getPosition() ), lTriSecond.getValue( pPointToInterpolateAt.getPosition() ) );
      }
    }
    catch( Exception e )
    {
      return null;
    }
  }

//  private static Map<GM_Point, Double> convertListsToMap( final List<GM_Point> listPoints, final List<Double> listValues )
//  {
//    if( listPoints == null || listValues == null || listPoints.size() != listValues.size() )
//    {
//      return null;
//    }
//    Map<GM_Point, Double> lMapRes = new HashMap<GM_Point, Double>();
//    for( int i = 0; i < listPoints.size(); i++ )
//    {
//      lMapRes.put( listPoints.get( i ), listValues.get( i ) );
//    }
//    return lMapRes;
//  }
}