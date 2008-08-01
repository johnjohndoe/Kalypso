/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
 
 ---------------------------------------------------------------------------------------------------*/
package org.kalypso.ogc.sensor.filter.filters;

import java.util.Date;

import org.kalypso.core.i18n.Messages;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.AbstractTuppleModel;
import org.kalypso.ogc.sensor.status.KalypsoStatusUtils;

/**
 * @author doemming
 */
public class NOperationTupplemodel extends AbstractTuppleModel
{
  private final int m_operation;

  private final ITuppleModel[] m_baseModels;

  public NOperationTupplemodel( ITuppleModel[] models, int operation )
  {
    super( models[0].getAxisList() );

    m_baseModels = models;
    m_operation = operation;
  }

  public int getCount() throws SensorException
  {
    return m_baseModels[0].getCount();
  }

  @Override
  public int hashCode( )
  {
    return m_baseModels[0].hashCode();
  }

  public Object getElement( int index, IAxis axis ) throws SensorException
  {
    //    final String axisName = axis.getName();
    final String axisType = axis.getType();

    final Class dataClass = axis.getDataClass();
    if( dataClass.equals( Date.class ) )
    {
      final IAxis a = ObservationUtilities.findAxisByType( m_baseModels[0].getAxisList(), axisType );
      //      final IAxis a = ObservationUtilities.findAxisByName(
      // m_baseModels[0].getAxisList(), axisName );
      return m_baseModels[0].getElement( index, a );
    }

    if( dataClass.equals( Double.class ) )
    {
      //      final IAxis a = ObservationUtilities.findAxisByName(
      // m_baseModels[0].getAxisList(), axisName );
      final IAxis a = ObservationUtilities.findAxisByType( m_baseModels[0].getAxisList(), axisType );
      if( index >= m_baseModels[0].getCount() )
        return null;
      double value = ( (Number)m_baseModels[0].getElement( index, a ) ).doubleValue();
      for( int i = 1; i < m_baseModels.length; i++ )
      {
        ITuppleModel model = m_baseModels[i];
        if( index >= model.getCount() )
          continue;

        //        final IAxis a2 = ObservationUtilities.findAxisByName( m_baseModels[i].getAxisList(), axisName );
        final IAxis a2 = ObservationUtilities.findAxisByType( m_baseModels[i].getAxisList(), axisType );

        double nextValue = ( (Number)model.getElement( index, a2 ) ).doubleValue();
        switch( m_operation )
        {
        case OperationFilter.OPERATION_PLUS:
          value += nextValue;
          break;
        case OperationFilter.OPERATION_MINUS:
          value -= nextValue;
          break;
        case OperationFilter.OPERATION_MAL:
          value *= nextValue;
          break;
        case OperationFilter.OPERATION_DURCH: // macht das sinn, bei mehr als
          // zwei ?
          value /= nextValue;
          break;
        }
      }
      return new Double( value );
    }

    // status-axis
    if(KalypsoStatusUtils.isStatusAxis(axis) )
    {
      final IAxis a = ObservationUtilities.findAxisByType( m_baseModels[0].getAxisList(), axisType );
      if( index >= m_baseModels[0].getCount() )
        return null;

      int value = ( (Number)m_baseModels[0].getElement( index, a ) ).intValue();
      for( int i = 1; i < m_baseModels.length; i++ )
      {
        ITuppleModel model = m_baseModels[i];
        if( index >= model.getCount() )
          continue;

        final IAxis a2 = ObservationUtilities.findAxisByType( m_baseModels[i].getAxisList(), axisType );

        int nextValue = ( (Number)model.getElement( index, a2 ) ).intValue();
        value |= nextValue;
      }
      return new Integer( value );
    }
    throw new UnsupportedOperationException( getClass().getName() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationTupplemodel.0") //$NON-NLS-1$
        + axis.getDataClass().getName() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationTupplemodel.1") ); //$NON-NLS-1$
  }

  public void setElement( int index, Object element, IAxis axis )
  {
    throw new UnsupportedOperationException( getClass().getName() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationTupplemodel.2") ); //$NON-NLS-1$
  }

  public int indexOf( Object element, IAxis axis ) throws SensorException
  {
    // TODO: better than this test: should test if axis.isKey() is true
    if( element instanceof Date )
      return m_baseModels[0].indexOf( element, axis );
    throw new UnsupportedOperationException( getClass().getName() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationTupplemodel.3") //$NON-NLS-1$
        + axis.getName() + Messages.getString("org.kalypso.ogc.sensor.filter.filters.NOperationTupplemodel.4") ); //$NON-NLS-1$
  }
}