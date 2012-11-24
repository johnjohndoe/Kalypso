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

import org.apache.commons.lang3.StringUtils;
import org.kalypso.commons.java.lang.Doubles;
import org.kalypso.model.wspm.core.profil.IProfile;
import org.kalypso.model.wspm.core.profil.IProfileObject;
import org.kalypso.model.wspm.core.profil.IProfileObjectRecord;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.model.wspm.tuhh.core.i18n.Messages;
import org.kalypso.wspwin.core.prf.datablock.IDataBlock;
import org.kalypso.wspwin.core.prf.datablock.IDataBlockNames;

/**
 * Special case for profiles containing (2d-)waterlevels: converts a waterlevel component into a plotter readable
 * datablock.
 * 
 * @author Gernot Belger
 */
public class ProfileWaterlevelWriter
{
  private final IProfile m_profile;

  public ProfileWaterlevelWriter( final IProfile profile )
  {
    m_profile = profile;
  }

  public IDataBlock[] createDataBlocks( )
  {
    final Collection<IDataBlock> dbs = new ArrayList<>();

    final IProfileObject[] profileObjects = m_profile.getProfileObjects();
    for( final IProfileObject profileObject : profileObjects )
    {
      final IDataBlock db = createDataBlock( profileObject );

      if( db != null )
        dbs.add( db );
    }

    return dbs.toArray( new IDataBlock[dbs.size()] );
  }

  private IDataBlock createDataBlock( final IProfileObject object )
  {
    final String type = object.getType();
    if( !IWspmTuhhConstants.OBJECT_TYPE_WATERLEVEL_SEGMENT.equals( type ) )
      return null;

    final String eventName = object.getValue( IWspmTuhhConstants.PROFIL_PROPERTY_EVENT_NAME, null );
    // final String eventType = object.getValue( IWspmTuhhConstants.PROFIL_PROPERTY_EVENT_TYPE, null );
    if( StringUtils.isEmpty( eventName ) )
      return null;

    final String strandName = Messages.getString( "ProfileWaterlevelWriter_0" ); //$NON-NLS-1$
    final String secondLine = String.format( "%100s%s@%s", " ", eventName, strandName ); //$NON-NLS-1$ //$NON-NLS-2$

    final CoordDataBlockCreator creator = new CoordDataBlockCreator( IDataBlockNames.WSP_HOEHE, secondLine );

    return writeCoords( object, creator );
  }

  private IDataBlock writeCoords( final IProfileObject object, final CoordDataBlockCreator creator )
  {
    final IProfileObjectRecord[] records = object.getRecords().getAll();
    if( records.length < 2 )
      return null;

    try
    {
      writeWaterlevel( creator, records );
      return creator.createDataBlock();
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return null;
    }
  }

  private void writeWaterlevel( final CoordDataBlockCreator creator, final IProfileObjectRecord[] records )
  {
    // final Double[] extrapolatedWaterlevel = extrapolateWaterlevel( points, iWaterlevel, iBreite, iHoehe );

    /* write 2d waterlevel as pairs of points, so plotter will understand it */
    for( int i = 1; i < records.length; i++ )
    {
      final IProfileObjectRecord pointFirst = records[i - 1];
      final IProfileObjectRecord pointSecond = records[i];

//      try
//      {
      final Double breiteFirst = pointFirst.getBreite();
      final Double breiteSecond = pointSecond.getBreite();

      // final Object hoeheFirst = pointFirst.getValue( iHoehe );
      // final Object hoeheSecond = pointSecond.getValue( iHoehe );

      // final Double waterlevelFirst = extrapolatedWaterlevel[i - 1];
      // final Double waterlevelSecond = extrapolatedWaterlevel[i];

      final Double waterlevelFirst = pointFirst.getHoehe();
      final Double waterlevelSecond = pointSecond.getHoehe();

      if( !Doubles.isNullOrInfinite( breiteFirst, breiteSecond, waterlevelFirst, waterlevelSecond ) )
      {
        // writeWaterlevelSegment( creator, breiteFirst, breiteSecond, hoeheFirst, hoeheSecond, waterlevelFirst, waterlevelSecond );

        creator.add( breiteFirst, waterlevelFirst );
        creator.add( breiteSecond, waterlevelSecond );
      }
//      }
//      catch( final SameXValuesException e )
//      {
//        e.printStackTrace();
//      }
    }
  }

//  private Double[] extrapolateWaterlevel( final IRecord[] points, final int iWaterlevel, final int iBreite, final int iHoehe )
//  {
//    final Double[] result = new Double[points.length];
//
//    for( int i = 0; i < points.length; i++ )
//    {
//      final Double waterlevel = getExtrapolatedWaterlevel( points, i, iWaterlevel, iBreite, iHoehe );
//      result[i] = waterlevel;
//    }
//
//    return result;
//  }

//  private Double getExtrapolatedWaterlevel( final IRecord[] points, final int i, final int iWaterlevel, final int iBreite, final int iHoehe )
//  {
//    final IRecord point = points[i];
//
//    final Object value = point.getValue( iWaterlevel );
//
//    if( value instanceof Number )
//      return ((Number)value).doubleValue();
//
//    final IRecord prevWaterlevel = findPrevWaterlevel( points, i, iWaterlevel, iHoehe );
//    final IRecord nextWaterlevel = findNextWaterlevel( points, i, iWaterlevel, iHoehe );
//
//    if( nextWaterlevel != null && prevWaterlevel != null )
//    {
//      final double prevW = ((Number)prevWaterlevel.getValue( iWaterlevel )).doubleValue();
//      final double nextW = ((Number)nextWaterlevel.getValue( iWaterlevel )).doubleValue();
//
//      final Object breite = point.getValue( iBreite );
//      final Object prevBreite = prevWaterlevel.getValue( iBreite );
//      final Object nextBreite = nextWaterlevel.getValue( iBreite );
//
//      if( breite instanceof Number && prevBreite instanceof Number && nextBreite instanceof Number )
//      {
//        final double prevB = ((Number)prevBreite).doubleValue();
//        final double nextB = ((Number)nextBreite).doubleValue();
//
//        try
//        {
//          final LinearEquation linearEquation = new LinearEquation( prevB, prevW, nextB, nextW );
//          return linearEquation.computeY( ((Number)breite).doubleValue() );
//        }
//        catch( final SameXValuesException e )
//        {
//          e.printStackTrace();
//        }
//      }
//    }
//
//    if( nextWaterlevel != null )
//      return ((Number)nextWaterlevel.getValue( iWaterlevel )).doubleValue();
//
//    if( prevWaterlevel != null )
//      return ((Number)prevWaterlevel.getValue( iWaterlevel )).doubleValue();
//
//    return null;
//  }

//  private IRecord findPrevWaterlevel( final IRecord[] points, final int i, final int iWaterlevel, final int iHoehe )
//  {
//    double maxHeight = -Double.MAX_VALUE;
//
//    for( int j = i - 1; j >= 0; j-- )
//    {
//      final IRecord point = points[j];
//
//      final Object valueWaterlevel = point.getValue( iWaterlevel );
//      final Object valueHoehe = point.getValue( iHoehe );
//
//      if( valueHoehe instanceof Number )
//      {
//        final double h = ((Number)valueHoehe).doubleValue();
//        maxHeight = Math.max( maxHeight, h );
//      }
//
//      if( valueWaterlevel instanceof Number )
//      {
//        final double w = ((Number)valueWaterlevel).doubleValue();
//        if( w > maxHeight )
//          return point;
//
//        /* Stop on first encountered waterlevel */
//        return null;
//      }
//    }
//
//    return null;
//  }
//
//  private IRecord findNextWaterlevel( final IRecord[] points, final int i, final int iWaterlevel, final int iHoehe )
//  {
//    double maxHeight = -Double.MAX_VALUE;
//
//    for( int j = i + 1; j < points.length; j++ )
//    {
//      final IRecord point = points[j];
//
//      final Object valueWaterlevel = point.getValue( iWaterlevel );
//      final Object valueHoehe = point.getValue( iHoehe );
//
//      if( valueHoehe instanceof Number )
//      {
//        final double h = ((Number)valueHoehe).doubleValue();
//        maxHeight = Math.max( maxHeight, h );
//      }
//
//      if( valueWaterlevel instanceof Number )
//      {
//        final double w = ((Number)valueWaterlevel).doubleValue();
//        if( w > maxHeight )
//          return point;
//
//        /* Stop on first encountered waterlevel */
//        return null;
//      }
//    }
//
//    return null;
//  }

//  private void writeWaterlevelSegment( final CoordDataBlockCreator creator, final Object breiteFirst, final Object breiteSecond, final Object hoeheFirst, final Object hoeheSecond, final Double waterlevelFirst, final Double waterlevelSecond ) throws SameXValuesException
//  {
//    if( breiteFirst instanceof Number && breiteSecond instanceof Number && hoeheFirst instanceof Number && hoeheSecond instanceof Number )
//    {
//      final double b1 = ((Number)breiteFirst).doubleValue();
//      final double b2 = ((Number)breiteSecond).doubleValue();
//      final double h1 = ((Number)hoeheFirst).doubleValue();
//      final double h2 = ((Number)hoeheSecond).doubleValue();
//
//      if( waterlevelFirst != null && waterlevelSecond != null )
//      {
//        final double w1 = ((Number)waterlevelFirst).doubleValue();
//        final double w2 = ((Number)waterlevelSecond).doubleValue();
//        addWaterlevelSegment( creator, b1, w1, b2, w2, h1, h2 );
//      }
//    }
//  }

//  // Plotter needs always pairs of two values as a waterlevel segment
//  private void addWaterlevelSegment( final CoordDataBlockCreator creator, final double b1, final double w1, final double b2, final double w2, final double h1, final double h2 ) throws SameXValuesException
//  {
//    if( w1 < h1 && w2 > h2 )
//    {
//      /* Find b, where w == h and use it as left hand side */
//      final LinearEquation le0 = new LinearEquation( b1, w1 - h1, b2, w2 - h2 );
//      final LinearEquation leW = new LinearEquation( b1, w1, b2, w2 );
//
//      final double b0 = le0.computeX( 0.0 );
//      final double w0 = leW.computeY( b0 );
//
//      creator.add( b0, w0 );
//      creator.add( b2, w2 );
//    }
//    else if( w1 > h1 && w2 < h2 )
//    {
//      /* Find b, where w == h and use it as right hand side */
//      final LinearEquation le0 = new LinearEquation( b1, w1 - h1, b2, w2 - h2 );
//      final LinearEquation leW = new LinearEquation( b1, w1, b2, w2 );
//
//      final double b0 = le0.computeX( 0.0 );
//      final double w0 = leW.computeY( b0 );
//
//      creator.add( b1, w1 );
//      creator.add( b0, w0 );
//    }
//    else if( w1 < h1 && w2 < h2 )
//    {
//      /* Waterlevel completely under soil: ignore */
//    }
//    else
//    {
//      /* Waterlevel completely above soil: just add */
//      creator.add( b1, w1 );
//      creator.add( b2, w2 );
//    }
//  }
}
