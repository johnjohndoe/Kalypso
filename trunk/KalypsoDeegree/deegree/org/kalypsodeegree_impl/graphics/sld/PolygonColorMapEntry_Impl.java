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
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.Fill;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.PolygonColorMapEntry;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Thomas Jung
 */
public class PolygonColorMapEntry_Impl implements PolygonColorMapEntry
{
  private ParameterValueType m_label;

  private ParameterValueType m_to;

  private ParameterValueType m_from;

  private Fill m_fill;

  public PolygonColorMapEntry_Impl( Fill fill, ParameterValueType label, ParameterValueType from, ParameterValueType to )
  {
    m_fill = fill;
    m_label = label;
    m_from = from;
    m_to = to;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.PolygonColorMapEntry#exportAsXML()
   */
  public String exportAsXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<PolygonColorMapEntry>" );

    // TODO

    sb.append( "</PolygonColorMapEntry>" );

    return sb.toString();
  }


  /**
   * @see org.kalypsodeegree.graphics.sld.PolygonColorMapEntry#getFill()
   */
  public Fill getFill( )
  {
    return m_fill;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.PolygonColorMapEntry#getFrom()
   */
  public double getFrom( Feature feature )
  {
    String val = null;
    try
    {
      val = m_from.evaluate( feature );
    }
    catch( FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    if( val != null )
      return Double.parseDouble( val );
    else
      return 0;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.PolygonColorMapEntry#getLabel()
   */
  public String getLabel( Feature feature )
  {
    String val = null;
    try
    {
      val = m_label.evaluate( feature );
    }
    catch( FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    return val;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.PolygonColorMapEntry#getTo()
   */
  public double getTo( Feature feature )
  {
    String val = null;
    try
    {
      val = m_to.evaluate( feature );
    }
    catch( FilterEvaluationException e )
    {
      // TODO Auto-generated catch block
      e.printStackTrace();
    }
    if( val != null )
      return Double.parseDouble( val );
    else
      return 0;

  }

}
