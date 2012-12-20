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
package org.kalypso.model.wspm.tuhh.core.profile;

import java.math.BigDecimal;

import org.apache.commons.lang3.ArrayUtils;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.IWspmTuhhConstants;
import org.kalypso.observation.result.IInterpolationHandler;
import org.kalypso.observation.result.IRecord;
import org.kalypso.observation.result.TupleResult;

/**
 * @author kimwerner
 */
public class TUHHInterpolationHandler implements IInterpolationHandler
{
  /* Get interpolated value during interpolation */
  private static final String[] INTERPOLATION_IDS = new String[] { IWspmConstants.POINT_PROPERTY_BREITE, IWspmConstants.POINT_PROPERTY_HOEHE, IWspmConstants.POINT_PROPERTY_HOCHWERT,
      IWspmConstants.POINT_PROPERTY_RECHTSWERT, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE,
      IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR };

  /* Gets value of first point during interpolation */
  private static final String[] CONSTANT_IDS = new String[] { IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP,
      IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KST };

  @Override
  public String getInterpolationComponent( )
  {
    return IWspmConstants.POINT_PROPERTY_BREITE;
  }

  @Override
  public boolean doInterpolation( final TupleResult result, final IRecord record, final int index, final double distance )
  {
    if( result == null || record == null || index < 0 || index > result.size() - 2 )
      return false;

    try
    {
      final IRecord previous = result.get( index );
      final IRecord next = result.get( index + 1 );
      for( final String id : INTERPOLATION_IDS )
      {
        final int i = result.indexOfComponent( id );
        if( i < 0 )
        {
          continue;
        }

        final Object v1 = previous.getValue( i );
        final Object v2 = next.getValue( i );
        final Object interp = interpolateValues( v1, v2, distance );
        record.setValue( i, interp );
      }

      for( final String id : CONSTANT_IDS )
      {
        final int i = result.indexOfComponent( id );
        if( i < 0 )
        {
          continue;
        }
        record.setValue( i, previous.getValue( i ) );
      }
      return true;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      throw new ArithmeticException( e.getLocalizedMessage() );
    }
  }

  private Object interpolateValues( final Object v1, final Object v2, final double distance )
  {
    if( !(v1 instanceof Number) || !(v2 instanceof Number) )
      return null;

    final Number n1 = (Number)v1;
    final Number n2 = (Number)v2;

    final double d1 = n1.doubleValue();
    final double d2 = n2.doubleValue();

    final double result = d1 + (d2 - d1) * distance;
    return createValue( v1, result );
  }

  private Object createValue( final Object template, final double value )
  {
    if( template.getClass() == Double.class )
      return Double.valueOf( value );
    if( template.getClass() == BigDecimal.class )
      return BigDecimal.valueOf( value ).setScale( ((BigDecimal)template).scale(), BigDecimal.ROUND_HALF_UP );
    if( template.getClass() == Integer.class )
      return Integer.valueOf( (int)Math.round( value ) );
    return template;
  }

  @Override
  public String[] getExtrapolationsIDs( )
  {
    return ArrayUtils.addAll( INTERPOLATION_IDS, CONSTANT_IDS );
  }
}