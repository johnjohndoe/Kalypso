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
package org.kalypsodeegree.model.feature.event;

/**
 * TODO typen überarbeiten und vereinheitlichen, überall verwenden
 * 
 * @author bce
 */
public class ModellEvent
{
  /** TODO comment this stuff! What can have happened, if this event has been sent? (Value 2) */
  public final static long FEATURE_CHANGE = 1 << 1;

// /** (Value 4) */
// public final static long STYLE_CHANGE = 1 << 2;
//
// /** (Value 8) */
// public final static long WIDGET_CHANGE = 1 << 3;
//
  /** Das sendende Objekt hat sich völlig geändert (Value 16) */
  public static final long FULL_CHANGE = 1 << 4;

//
// /** TODO: this is not a modell-event type (it has nothing to do with the workspace) (Value 32) */
// public static final long THEME_ADDED = 1 << 5;
//
// /** TODO: this is not a modell-event type (it has nothing to do with the workspace) (Value 64) */
// public static final long LEGEND_UPDATED = 1 << 6;
//
// /** TODO: this is not a modell-event type (it has nothing to do with the workspace) (Value 128) */
// public static final long THEME_ACTIVATED = 1 << 7;

  private final ModellEventProvider m_eventSource;

  private final long m_bitmaskType;

  public ModellEvent( final ModellEventProvider eventSource, final long bitmaskType )
  {
    m_eventSource = eventSource;
    m_bitmaskType = bitmaskType;
  }

  public boolean isType( final long bitmask )
  {
    return (m_bitmaskType & bitmask) == bitmask;
  }

  public ModellEventProvider getEventSource( )
  {
    return m_eventSource;
  }

  /**
   * @see java.lang.Object#toString()
   */
  @Override
  public String toString( )
  {
    final StringBuffer buf = new StringBuffer( 128 );
    buf.append( "ModelEvent[" );
    // mask
    appendMaskText( m_bitmaskType, buf );
    // source
    buf.append( m_eventSource );
    buf.append( ']' );
    return buf.toString();
  }

  /**
   * compute and append the text representation of the mask. the mask may cover multiple types
   */
  private static final void appendMaskText( final long bitmask, final StringBuffer buf )
  {
    if( (FEATURE_CHANGE & bitmask) == bitmask )
    {
      buf.append( "FEATURE_CHANGE " );
    }

// if( (STYLE_CHANGE & bitmask) == bitmask )
// {
// buf.append( "STYLE_CHANGE " );
// }
//
// if( (WIDGET_CHANGE & bitmask) == bitmask )
// {
// buf.append( "WIDGET_CHANGE " );
// }
//
// if( (FULL_CHANGE & bitmask) == bitmask )
// {
// buf.append( "FULL_CHANGE " );
// }
//
// if( (THEME_ADDED & bitmask) == bitmask )
// {
// buf.append( "THEME_ADDED " );
// }
//
// if( (LEGEND_UPDATED & bitmask) == bitmask )
// {
// buf.append( "LEGEND_UPDATED" );
// }
  }
}