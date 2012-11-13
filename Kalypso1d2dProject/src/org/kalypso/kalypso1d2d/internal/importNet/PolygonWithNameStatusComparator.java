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
package org.kalypso.kalypso1d2d.internal.importNet;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.jface.viewers.ViewerComparator;

import com.bce.gis.io.zweidm.PolygonWithName;

/**
 * @author Gernot Belger
 */
public class PolygonWithNameStatusComparator extends ViewerComparator
{
  @Override
  public int compare( final Viewer viewer, final Object e1, final Object e2 )
  {
    final IStatus s1 = getStatus( e1 );
    final IStatus s2 = getStatus( e2 );

    /* first by severity ... */
    final int sev1 = s1.getSeverity();
    final int sev2 = s2.getSeverity();

    if( sev1 != sev2 )
      return sev1 - sev2;

    /* ... then by message */
    final String m1 = s1.getMessage();
    final String m2 = s2.getMessage();
    return m1.compareTo( m2 );
  }

  private IStatus getStatus( final Object element )
  {
    if( element instanceof PolygonWithName )
      return ((PolygonWithName)element).getStatus();

    return Status.OK_STATUS;
  }
}