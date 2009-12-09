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
  final String[] m_interpolIDs = new String[] { IWspmConstants.POINT_PROPERTY_BREITE, IWspmConstants.POINT_PROPERTY_HOEHE, IWspmConstants.POINT_PROPERTY_HOCHWERT,
      IWspmConstants.POINT_PROPERTY_RECHTSWERT, IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEBRUECKE, IWspmTuhhConstants.POINT_PROPERTY_UNTERKANTEBRUECKE,
      IWspmTuhhConstants.POINT_PROPERTY_OBERKANTEWEHR };

  final String[] m_defaultIDs = new String[] { IWspmConstants.POINT_PROPERTY_BEWUCHS_AX, IWspmConstants.POINT_PROPERTY_BEWUCHS_AY, IWspmConstants.POINT_PROPERTY_BEWUCHS_DP,
      IWspmTuhhConstants.POINTMARKER_PROPERTY_RAUHEIT, IWspmTuhhConstants.POINT_PROPERTY_RAUHEIT_KS };

  /**
   * @see org.kalypso.observation.result.IInterpolationHandler#doInterpolation(java.lang.String, java.lang.Double,
   *      java.lang.Double)
   */
  public boolean doInterpolation( final TupleResult result, final IRecord record, final int index, final Double distance )
  {
    if( result == null || record == null || index < 0 || index > result.size() - 2 )
      return false;
    try
    {
      final IRecord previous = result.get( index );
      final IRecord next = result.get( index + 1 );
      for( final String id : m_interpolIDs )
      {
        final int i = result.indexOfComponent( id );
        if( i < 0 )
          continue;
        final Double v1 = (Double) previous.getValue( i );
        final Double v2 = (Double) next.getValue( i );
        final Double value = v1 + (v2 - v1) * distance;
        record.setValue( i, value );
      }
      for( final String id : m_defaultIDs )
      {
        final int i = result.indexOfComponent( id );
        if( i < 0 )
          continue;
        record.setValue( i, previous.getValue( i ) );
      }
      return true;
    }
    catch( Exception e )
    {
      throw new ArithmeticException( e.getLocalizedMessage() );
    }
  }
}
