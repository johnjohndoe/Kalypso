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
package org.kalypso.model.wspm.tuhh.core.wspwin.prf;

import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.kalypso.commons.math.LinearEquation;
import org.kalypso.commons.math.LinearEquation.SameXValuesException;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.core.profil.IProfil;
import org.kalypso.observation.phenomenon.IPhenomenon;
import org.kalypso.observation.result.IComponent;
import org.kalypso.observation.result.IRecord;
import org.kalypso.wspwin.core.prf.datablock.CoordDataBlock;
import org.kalypso.wspwin.core.prf.datablock.DataBlockHeader;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;

/**
 * Experimental (for Steiermark): converts a watrerlevel component into a plotter readable datablock.
 * 
 * @author Gernot Belger
 */
public class PlotterWaterlevelWriter
{
  private final IProfil m_profile;

  public PlotterWaterlevelWriter( final IProfil profile )
  {
    m_profile = profile;
  }

  public IDataBlock[] createDataBlocks( )
  {
    final IComponent[] pointProperties = m_profile.getPointProperties();

    final Collection<IDataBlock> dbs = new ArrayList<IDataBlock>();

    for( final IComponent property : pointProperties )
    {
      final IDataBlock db = createDataBlock( property );
      if( db != null )
        dbs.add( db );
    }

    return dbs.toArray( new IDataBlock[dbs.size()] );
  }

  private IDataBlock createDataBlock( final IComponent property )
  {
    final IPhenomenon phenomenon = property.getPhenomenon();
    final String phenomenonId = phenomenon.getID();
    if( !IWspmConstants.PHENOMENON_WATERLEVEL_2D.equals( phenomenonId ) )
      return null;

    final DataBlockHeader dbh = new DataBlockHeader();
    dbh.setFirstLine( "WSP-HOEHE NN+m" ); //$NON-NLS-1$

    final String componentName = property.getName();
    final String strandName = "unknown";
    final String secondLine = String.format( "%100s%s@%s", " ", componentName, strandName );
    dbh.setSecondLine( secondLine );

    return writeCoords( m_profile, property, dbh );
  }

  private IDataBlock writeCoords( final IProfil profil, final IComponent waterlevelProperty, final DataBlockHeader dbh )
  {
    final IRecord[] points = profil.getPoints();
    if( points.length < 2 )
      return null;

    final int iBreite = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_BREITE );
    final int iHoehe = profil.indexOfProperty( IWspmConstants.POINT_PROPERTY_HOEHE );
    final int iWaterlevel = profil.indexOfProperty( waterlevelProperty );

    try
    {
      return writeWaterlevel( dbh, points, iBreite, iHoehe, iWaterlevel );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }

  }

  private IDataBlock writeWaterlevel( final DataBlockHeader dbh, final IRecord[] points, final int iBreite, final int iHoehe, final int iWaterlevel )
  {
    final CoordDataBlock db = new CoordDataBlock( dbh );

    final List<Double> xs = new ArrayList<Double>( points.length );
    final List<Double> ys = new ArrayList<Double>( points.length );

    final Map<IRecord, Double> extrapolatedWaterlevel = extrapolateWaterlevel( points, iWaterlevel, iHoehe );
    for( int i = 1; i < points.length; i++ )
    {
      final IRecord pointFirst = points[i - 1];
      final IRecord pointSecond = points[i];

      try
      {
        writeWaterlevelSegment( iBreite, iHoehe, xs, ys, pointFirst, pointSecond, extrapolatedWaterlevel );
      }
      catch( final SameXValuesException e )
      {
        e.printStackTrace();
      }
    }

    final Double[] xArray = xs.toArray( new Double[xs.size()] );
    final Double[] yArray = ys.toArray( new Double[ys.size()] );
    db.setCoords( xArray, yArray );
    return db;
  }

  private Map<IRecord, Double> extrapolateWaterlevel( final IRecord[] points, final int iWaterlevel, final int iHoehe )
  {
    final Map<IRecord, Double> result = new HashMap<IRecord, Double>();

    for( int i = 0; i < points.length; i++ )
    {
      final IRecord point = points[i];

      final Double waterlevel = getExtrapolatedWaterlevel( points, i, iWaterlevel, iHoehe );
      if( waterlevel != null )
        result.put( point, waterlevel );
    }

    return result;
  }

  private Double getExtrapolatedWaterlevel( final IRecord[] points, final int i, final int iWaterlevel, final int iHoehe )
  {
    final IRecord point = points[i];

    final Object value = point.getValue( iWaterlevel );

    if( value instanceof Number )
      return ((Number) value).doubleValue();

    final Double prevWaterlevel = findNextWaterlevel( points, i, iWaterlevel, iHoehe );
    if( prevWaterlevel != null )
      return prevWaterlevel;

    return findPrevWaterlevel( points, i, iWaterlevel, iHoehe );
  }

  private Double findPrevWaterlevel( final IRecord[] points, final int i, final int iWaterlevel, final int iHoehe )
  {
    double maxHeight = -Double.MAX_VALUE;

    for( int j = i - 1; j >= 0; j-- )
    {
      final IRecord point = points[j];

      final Object valueWaterlevel = point.getValue( iWaterlevel );
      final Object valueHoehe = point.getValue( iHoehe );

      if( valueHoehe instanceof Number )
      {
        final double h = ((Number) valueHoehe).doubleValue();
        maxHeight = Math.max( maxHeight, h );
      }

      if( valueWaterlevel instanceof Number )
      {
        final double w = ((Number) valueWaterlevel).doubleValue();
        if( w > maxHeight )
          return w;

        /* Stop on first encountered waterlevel */
        return null;
      }
    }

    return null;
  }

  private Double findNextWaterlevel( final IRecord[] points, final int i, final int iWaterlevel, final int iHoehe )
  {
    double maxHeight = -Double.MAX_VALUE;

    for( int j = i + 1; j < points.length; j++ )
    {
      final IRecord point = points[j];

      final Object valueWaterlevel = point.getValue( iWaterlevel );
      final Object valueHoehe = point.getValue( iHoehe );

      if( valueHoehe instanceof Number )
      {
        final double h = ((Number) valueHoehe).doubleValue();
        maxHeight = Math.max( maxHeight, h );
      }

      if( valueWaterlevel instanceof Number )
      {
        final double w = ((Number) valueWaterlevel).doubleValue();
        if( w > maxHeight )
          return w;

        /* Stop on first encountered waterlevel */
        return null;
      }
    }

    return null;
  }

  private void writeWaterlevelSegment( final int iBreite, final int iHoehe, final List<Double> xs, final List<Double> ys, final IRecord pointFirst, final IRecord pointSecond, final Map<IRecord, Double> extrapolatedWaterlevel ) throws SameXValuesException
  {
    final Object breiteFirst = pointFirst.getValue( iBreite );
    final Object breiteSecond = pointSecond.getValue( iBreite );

    final Object hoeheFirst = pointFirst.getValue( iHoehe );
    final Object hoeheSecond = pointSecond.getValue( iHoehe );

    final Double waterlevelFirst = extrapolatedWaterlevel.get( pointFirst );
    final Double waterlevelSecond = extrapolatedWaterlevel.get( pointSecond );

    if( breiteFirst instanceof Number && breiteSecond instanceof Number && hoeheFirst instanceof Number && hoeheSecond instanceof Number )
    {
      final double b1 = ((Number) breiteFirst).doubleValue();
      final double b2 = ((Number) breiteSecond).doubleValue();
      final double h1 = ((Number) hoeheFirst).doubleValue();
      final double h2 = ((Number) hoeheSecond).doubleValue();

// if( waterlevelFirst == null && waterlevelSecond != null )
// {
// /* open beginning: extrapolate to the left side */
// final double w2 = ((Number) waterlevelSecond).doubleValue();
// addWaterlevelSegment( xs, ys, b1, w2, b2, w2, h1, h2 );
// }
// else if( waterlevelFirst != null && waterlevelSecond == null )
// {
// /* open beginning: extrapolate to the left side */
// final double w1 = ((Number) waterlevelFirst).doubleValue();
// addWaterlevelSegment( xs, ys, b1, w1, b2, w1, h1, h2 );
// }
// else
      if( waterlevelFirst != null && waterlevelSecond != null )
      {
        final double w1 = ((Number) waterlevelFirst).doubleValue();
        final double w2 = ((Number) waterlevelSecond).doubleValue();
        addWaterlevelSegment( xs, ys, b1, w1, b2, w2, h1, h2 );
      }
    }
  }

  // Plotter needs always pairs of two values as a waterlevel segment
  private void addWaterlevelSegment( final List<Double> xs, final List<Double> ys, final double b1, final double w1, final double b2, final double w2, final double h1, final double h2 ) throws SameXValuesException
  {
    if( w1 < h1 && w2 > h2 )
    {
      /* Find b, where w == h and use it as left hand side */
      final LinearEquation le0 = new LinearEquation( b1, w1 - h1, b2, w2 - h2 );
      final LinearEquation leW = new LinearEquation( b1, w1, b2, w2 );

      final double b0 = le0.computeX( 0.0 );
      final double w0 = leW.computeY( b0 );

      xs.add( b0 );
      ys.add( w0 );

      xs.add( b2 );
      ys.add( w2 );
    }
    else if( w1 > h1 && w2 < h2 )
    {
      /* Find b, where w == h and use it as right hand side */
      final LinearEquation le0 = new LinearEquation( b1, w1 - h1, b2, w2 - h2 );
      final LinearEquation leW = new LinearEquation( b1, w1, b2, w2 );

      final double b0 = le0.computeX( 0.0 );
      final double w0 = leW.computeY( b0 );

      xs.add( b1 );
      ys.add( w1 );

      xs.add( b0 );
      ys.add( w0 );
    }
    else if( w1 < h1 && w2 < h2 )
    {
      /* Waterlevel completely under soil: ignore */
    }
    else
    {
      /* Waterlevel completely above soil: just add */
      xs.add( b1 );
      ys.add( w1 );

      xs.add( b2 );
      ys.add( w2 );
    }
  }
}
