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
package org.kalypso.ogc.gml;

import java.util.Formatter;
import java.util.Properties;

import org.eclipse.core.runtime.CoreException;
import org.kalypsodeegree.model.geometry.GM_Position;

/**
 * Implementors of this interface provide information about a particular theme.<br>
 * Implementors of this interface must provide a default constructor. This class is excpected to be initialized with a
 * call to {@link #init(IKalypsoTheme)}.
 * 
 * @author Gernot Belger
 */
public interface IKalypsoThemeInfo
{
  /**
   * Initialized this info on the given theme.<br>
   * Must be called before any call to another method.
   */
  void init( final IKalypsoTheme theme, final Properties props ) throws CoreException;

  /**
   * Provide information at a given position.<br>
   * This method should return as quickly as possible, as it is usually used to show a ooltip on the map.
   * 
   * @parm pos A geographic position. Must be in the same coordinate system as the current map context.
   * @parm formatter The information gets written to this formatter
   */
  void appendQuickInfo( final Formatter formatter, final GM_Position pos );

  /**
   * Provide information at a given position.<br>
   * This method will be used to regain more specific information about a certain theme. It may take (some) time to
   * return.
   * 
   * @parm pos A geographic position. Must be in the same coordinate system as the current map context.
   * @parm formatter The information gets written to this formatter
   */
  void appendInfo( final Formatter formatter, final GM_Position pos );

}
