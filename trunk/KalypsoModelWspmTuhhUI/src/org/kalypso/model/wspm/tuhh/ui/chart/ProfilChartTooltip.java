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
package org.kalypso.model.wspm.tuhh.ui.chart;

import java.util.ArrayList;
import java.util.Formatter;
import java.util.List;

import org.apache.commons.collections.KeyValue;
import org.apache.commons.collections.keyvalue.DefaultKeyValue;
import org.apache.commons.lang.StringUtils;

/**
 * Helper for chart layers that formats tooltips in a general way.<br/>
 * TODO: support components with different scales (calc max-scale and use to format)
 * 
 * @author Gernot Belger
 */
public class ProfilChartTooltip
{
  private final List<KeyValue> m_labels = new ArrayList<KeyValue>();

  public void add( final String label, final Object value )
  {
    m_labels.add( new DefaultKeyValue( label, value ) );
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final Formatter formatter = new Formatter();

    final int maxLabelLength = getMaxLabelLength();

    for( final KeyValue keyValue : m_labels )
    {
      final Object key = keyValue.getKey();
      final Object value = keyValue.getValue();
      if( key instanceof String && value instanceof Number )
      {
        final String format = getFormat( maxLabelLength );
        formatter.format( format, key, value );
      }
      else
      {
        if( key != null )
          formatter.format( "%s", key ); //$NON-NLS-1$
      }
    }

    final String result = formatter.toString();
    return StringUtils.chop( result );
  }

  private String getFormat( final int maxLabelLength )
  {
    return String.format( "%%-%ds %%9.4f\n", maxLabelLength ); //$NON-NLS-1$
  }

  private int getMaxLabelLength( )
  {
    int maxLength = 0;
    for( final KeyValue keyValue : m_labels )
    {
      final Object key = keyValue.getKey();
      if( key instanceof String )
        maxLength = Math.max( ((String) key).length(), maxLength );
    }

    return maxLength;
  }

}
