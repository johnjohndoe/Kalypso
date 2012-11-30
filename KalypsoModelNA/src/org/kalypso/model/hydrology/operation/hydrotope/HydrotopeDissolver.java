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
package org.kalypso.model.hydrology.operation.hydrotope;

import java.util.Collection;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;
import org.kalypso.contribs.eclipse.core.runtime.IStatusCollector;
import org.kalypso.contribs.eclipse.core.runtime.StatusCollector;
import org.kalypso.contribs.eclipse.jface.operation.ICoreRunnableWithProgress;
import org.kalypso.contribs.eclipse.ui.progress.ProgressUtilities;
import org.kalypso.model.hydrology.internal.ModelNA;
import org.kalypso.model.hydrology.internal.i18n.Messages;

import com.vividsolutions.jts.geom.Polygon;

/**
 * @author Gernot Belger
 */
public class HydrotopeDissolver implements ICoreRunnableWithProgress
{
  private Collection<HydrotopeUserData> m_result;

  private final String m_logMessage;

  private final List<Polygon> m_intersections;

  public HydrotopeDissolver( final List<Polygon> intersections, final String logMessage )
  {
    m_intersections = intersections;
    m_logMessage = logMessage;
  }

  @Override
  public IStatus execute( final IProgressMonitor monitor )
  {
    final IStatusCollector log = new StatusCollector( ModelNA.PLUGIN_ID );

    final SubMonitor progress = SubMonitor.convert( monitor, Messages.getString("HydrotopeDissolver_0"), 100 ); //$NON-NLS-1$

    final Map<String, HydrotopeUserData> hash = new HashMap<>();

    final int intersectionsSize = m_intersections.size();

    progress.setWorkRemaining( intersectionsSize * 2 );

    for( int i = 0; i < intersectionsSize; i++ )
    {
      if( i % 10 == 0 )
      {
        progress.subTask( String.format( Messages.getString("HydrotopeDissolver_1"), i, intersectionsSize ) ); //$NON-NLS-1$
        ProgressUtilities.worked( progress, 10 );
      }

      try
      {
        final Polygon polygon = m_intersections.get( i );

        final HydrotopeUserData hydrotope = (HydrotopeUserData) polygon.getUserData();
        final String key = hydrotope.getHydrotopeHash();

        /* Add or merge element */
        final HydrotopeUserData existingBean = hash.get( key );
        if( existingBean == null )
          hash.put( key, hydrotope );
        else
        {
          final HydrotopeUserData newBean = existingBean.mergeGeometries( hydrotope );
          hash.put( key, newBean );
        }
      }
      catch( final CoreException e )
      {
        log.add( e.getStatus() );
      }
    }

    m_result = hash.values();

    return log.asMultiStatus( m_logMessage );
  }

  public Collection<HydrotopeUserData> getResult( )
  {
    return Collections.unmodifiableCollection( m_result );
  }
}