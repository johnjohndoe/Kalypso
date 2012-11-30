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
package org.kalypso.ui.rrm.internal.gml;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Map;

import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.ObservationUtilities;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.visitor.ITupleModelValueContainer;
import org.kalypso.ogc.sensor.visitor.ITupleModelVisitor;

/**
 * @author Gernot Belger
 *
 */
public class CopyObservationVisitor implements ITupleModelVisitor
{
  private final Collection<Object[]> m_values = new ArrayList<>();

  private final IAxis[] m_targetAxis;

  private Map<String, String> m_nameMapping;

  public CopyObservationVisitor( final IAxis[] targetAxis )
  {
    m_targetAxis = targetAxis;
  }

  @Override
  public void visit( final ITupleModelValueContainer container ) throws SensorException
  {
    final Object[] values = new Object[m_targetAxis.length];
    for( int i = 0; i < m_targetAxis.length; i++ )
    {
      final IAxis targetAxis = m_targetAxis[i];
      values[i] = getValue( targetAxis, container );
    }

    m_values.add( values );
  }

  private Object getValue( final IAxis targetAxis, final ITupleModelValueContainer container ) throws SensorException
  {
    final IAxis sourceAxis = findSourceAxis( targetAxis, container );
    // a new axis is added, fill with 0.0
    if( sourceAxis == null )
      return 0.0;

    /* Keep old value */
    return container.get( sourceAxis );
  }

  private IAxis findSourceAxis( final IAxis targetAxis, final ITupleModelValueContainer container )
  {
    final IAxis[] axes = container.getAxes();
    final String targetName = targetAxis.getName();
    final IAxis sourceAxis = ObservationUtilities.findAxisByNameNoEx( axes, targetName );
    if( sourceAxis != null )
      return sourceAxis;

    /* Maybe we have a mapping for his axis? */
    for( final IAxis oldAxis : axes )
    {
      final String oldName = oldAxis.getName();
      if( m_nameMapping.containsKey( oldName ) )
      {
        final String newName = m_nameMapping.get( oldName );
        if( targetName.equals( newName ) )
          return oldAxis;
      }
    }

    return null;
  }

  public Object[][] getValues( )
  {
    return m_values.toArray( new Object[m_values.size()][] );
  }

  public void setNameMapping( final Map<String, String> nameMapping )
  {
    m_nameMapping = nameMapping;
  }
}