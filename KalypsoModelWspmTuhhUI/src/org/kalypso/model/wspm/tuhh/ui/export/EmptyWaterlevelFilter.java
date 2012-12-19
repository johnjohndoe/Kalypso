/** This file is part of Kalypso
 *
 *  Copyright (c) 2012 by
 *
 *  Björnsen Beratende Ingenieure GmbH, Koblenz, Germany (Bjoernsen Consulting Engineers), http://www.bjoernsen.de
 *  Technische Universität Hamburg-Harburg, Institut für Wasserbau, Hamburg, Germany
 *  (Technical University Hamburg-Harburg, Institute of River and Coastal Engineering), http://www.tu-harburg.de/wb/
 *
 *  Kalypso is free software: you can redistribute it and/or modify it under the terms  
 *  of the GNU Lesser General Public License (LGPL) as published by the Free Software 
 *  Foundation, either version 3 of the License, or (at your option) any later version.
 *
 *  Kalypso is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied 
 *  warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with Kalypso.  If not, see <http://www.gnu.org/licenses/>.
 */
package org.kalypso.model.wspm.tuhh.ui.export;

import java.math.BigDecimal;

import org.apache.commons.lang3.ArrayUtils;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerFilter;
import org.kalypso.model.wspm.tuhh.core.results.IWspmResult;
import org.kalypso.model.wspm.tuhh.core.results.WspmResultLengthSection;

/**
 * Hides empty (= 2d-waterlevels) from the result export viewer.
 * 
 * @author Gernot
 */
public class EmptyWaterlevelFilter extends ViewerFilter
{
  @Override
  public boolean select( final Viewer viewer, final Object parentElement, final Object element )
  {
    if( element instanceof IWspmResult )
      return hasValues( (IWspmResult)element );

    return true;
  }

  private boolean hasValues( final IWspmResult result )
  {
    final WspmResultLengthSection lengthSection = result.getLengthSection();
    if( lengthSection == null )
      return false;

    final BigDecimal[] stations = lengthSection.getStations();
    return !ArrayUtils.isEmpty( stations );
  }
}