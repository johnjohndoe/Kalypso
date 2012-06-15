/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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

import java.math.BigDecimal;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.kalypso.model.wspm.core.IWspmConstants;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResultNode;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;
import org.kalypso.model.wspm.ui.view.chart.layer.IWspLayerDataElement;

/**
 * @author Gernot Belger
 */
public class TuhhResultDataElement implements IWspLayerDataElement
{
  private final IWspmResultNode m_resultNode;

  public TuhhResultDataElement( final IWspmResultNode resultNode )
  {
    m_resultNode = resultNode;
  }

  /**
   * @see org.kalypso.model.wspm.ui.view.chart.layer.IWspLayerDataElement#getLabel()
   */
  @Override
  public String getLabel( )
  {
    // FIXME: delegate path-labeling to IWspmResults, in order ot handle special cases correctly (polynomes...)
    if( m_resultNode instanceof IWspmResult )
    {
      final IWspmResultNode parent = m_resultNode.getParent();
      return String.format( "%s (%s)", parent.getLabel(), m_resultNode.getLabel() ); //$NON-NLS-1$
    }

    if( m_resultNode == null )
      return toString();

    final IWspmResultNode[] childResults = m_resultNode.getChildResults();
    if( childResults.length > 0 )
      return String.format( "%s (%s)", m_resultNode.getLabel(), childResults[0].getLabel() ); //$NON-NLS-1$

    return m_resultNode.getLabel();
  }

  public String getId( )
  {
    if( m_resultNode == null )
      return ""; //$NON-NLS-1$

    return m_resultNode.getName();
  }

  public double getValue( final BigDecimal station )
  {
    if( m_resultNode == null )
      return Double.NaN;

    return searchValue( m_resultNode, station );
  }

  /**
   * @see java.lang.Object#equals(java.lang.Object)
   */
  @Override
  public boolean equals( final Object obj )
  {
    return EqualsBuilder.reflectionEquals( this, obj );
  }

  /**
   * @see java.lang.Object#hashCode()
   */
  @Override
  public int hashCode( )
  {
    if( m_resultNode == null )
      return "".hashCode(); //$NON-NLS-1$

    return m_resultNode.hashCode();
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    return String.format( "%s: %s", getClass().getSimpleName(), m_resultNode ); //$NON-NLS-1$
  }

  private static double searchValue( final IWspmResultNode resultNode, final BigDecimal station )
  {
    if( resultNode instanceof IWspmResult )
    {
      final IWspmResult result = (IWspmResult) resultNode;
      final WspmResultLengthSection lengthSection = result.getLengthSection();
      final Object value = lengthSection.getValue( station, IWspmConstants.LENGTH_SECTION_PROPERTY_WATERLEVEL );
      if( value instanceof Number )
        return ((Number) value).doubleValue();
    }
    else
    {
      /* A parent node always returns the value of it's first child. */
      final IWspmResultNode[] childResults = resultNode.getChildResults();
      if( childResults.length > 0 )
        return searchValue( childResults[0], station );
    }

    return Double.NaN;
  }

  public IWspmResultNode getResultNode( )
  {
    return m_resultNode;
  }

}
