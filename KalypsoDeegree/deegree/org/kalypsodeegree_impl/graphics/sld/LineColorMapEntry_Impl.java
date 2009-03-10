/** This file is part of kalypso/deegree.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 * history:
 * 
 * Files in this package are originally taken from deegree and modified here
 * to fit in kalypso. As goals of kalypso differ from that one in deegree
 * interface-compatibility to deegree is wanted but not retained always. 
 * 
 * If you intend to use this software in other ways than in kalypso 
 * (e.g. OGC-web services), you should consider the latest version of deegree,
 * see http://www.deegree.org .
 *
 * all modifications are licensed as deegree, 
 * original copyright:
 *
 * Copyright (C) 2001 by:
 * EXSE, Department of Geography, University of Bonn
 * http://www.giub.uni-bonn.de/exse/
 * lat/lon GmbH
 * http://www.lat-lon.de
 */
package org.kalypsodeegree_impl.graphics.sld;

import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.LineColorMapEntry;
import org.kalypsodeegree.graphics.sld.ParameterValueType;
import org.kalypsodeegree.graphics.sld.Stroke;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.xml.Marshallable;

/**
 * @author Thomas Jung
 */
public class LineColorMapEntry_Impl implements LineColorMapEntry
{

  private Stroke m_stroke;

  private ParameterValueType m_quantity;

  private ParameterValueType m_label;

  public LineColorMapEntry_Impl( Stroke stroke, ParameterValueType label, ParameterValueType quantity )
  {
    m_stroke = stroke;
    m_label = label;
    m_quantity = quantity;
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.LineColorMapEntry#exportAsXML()
   */
  public String exportAsXML( )
  {
    final StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<sldExt:LineColorMapEntry>" );

    if( m_stroke != null )
    {
      sb.append( ((Marshallable) m_stroke).exportAsXML() );
    }

    sb.append( "<sldExt:label>" );
    sb.append( ((Marshallable) m_label).exportAsXML() );
    sb.append( "</sldExt:label>" );

    sb.append( "<sldExt:quantity>" );
    sb.append( ((Marshallable) m_quantity).exportAsXML() );
    sb.append( "</sldExt:quantity>" );

    sb.append( "</sldExt:LineColorMapEntry>" );

    return sb.toString();
  }

  /**
   * @see org.kalypsodeegree.graphics.sld.LineColorMapEntry#getLabel(org.kalypsodeegree.model.feature.Feature)
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
   * @see org.kalypsodeegree.graphics.sld.LineColorMapEntry#getQuantity(org.kalypsodeegree.model.feature.Feature)
   */
  public double getQuantity( Feature feature )
  {
    String val = null;
    try
    {
      val = m_quantity.evaluate( feature );
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
   * @see org.kalypsodeegree.graphics.sld.LineColorMapEntry#getStroke()
   */
  public Stroke getStroke( )
  {
    return m_stroke;
  }

}
