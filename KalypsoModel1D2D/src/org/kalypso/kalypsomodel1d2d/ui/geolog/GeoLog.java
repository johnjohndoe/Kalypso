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
package org.kalypso.kalypsomodel1d2d.ui.geolog;

import java.util.Date;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.kalypso.gmlschema.GMLSchemaException;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Object;
import org.kalypsodeegree_impl.gml.binding.commons.IGeoStatus;
import org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection;
import org.kalypsodeegree_impl.model.feature.FeatureFactory;

/**
 * Default implementation of {@link IGeoLog}.<br>
 * The logged stati will we added to a gml-workspace whose root feature is a {@link org.kalypsodeegree_impl.gml.binding.commons.IStatusCollection}.
 * TODO: move to common place<br>
 * 
 * @author Gernot Belger
 */
public class GeoLog implements IGeoLog
{
  private final ILog m_log;

  private String m_pluginId;

  private final IStatusCollection m_statusCollection;

  private Date m_startTime;

  /**
   * @param log
   *          If non <code>null</code>, all logged stati are additionally logged into this log. Also, newly created
   *          stati get the logs plug-in-id.
   */
  public GeoLog( final ILog log ) throws GMLSchemaException
  {
    m_log = log;
    if( log != null )
      m_pluginId = log.getBundle().getSymbolicName();

    // create GML-Workspace which will hold the geo-stati
    final GMLWorkspace workspace = FeatureFactory.createGMLWorkspace( IStatusCollection.QNAME, null, null );
    m_statusCollection = (IStatusCollection)workspace.getRootFeature().getAdapter( IStatusCollection.class );

    if( m_log != null )
      m_pluginId = m_log.getBundle().getSymbolicName();
  }

  @Override
  public IStatusCollection getStatusCollection( )
  {
    return m_statusCollection;
  }

  @Override
  public void close( )
  {
    m_statusCollection.getWorkspace().dispose();
  }

  @Override
  public IGeoStatus formatLog( final int severity, final int code, final String message, final Object... args )
  {
    final String msg = String.format( message, args );
    return log( severity, code, msg, null, null );
  }

  @Override
  public IGeoStatus log( final int severity, final int code, final String message, final GM_Object location, final Throwable t )
  {
    final Date now = new Date();

    // create status into workspace
    final IGeoStatus newStatus = m_statusCollection.createGeoStatus( severity, m_pluginId, code, message, t, location, now );

    logOthers( now, newStatus );

    return newStatus;
  }

  /** Log to external log and remember start time. */
  private void logOthers( final Date time, final IStatus status )
  {
    // log to real ILog
    if( m_log != null )
      m_log.log( status );

    if( m_startTime != null )
      m_startTime = time;
  }

  @Override
  public void log( final IStatus status )
  {
    final Date now = new Date();
    logOthers( now, status );

    addAndCloneStatus( now, status, m_statusCollection );
  }

  /**
   * (Recursively) adds a new status to the given collection and copies all properties from the given status.
   */
  private IGeoStatus addAndCloneStatus( final Date time, final IStatus status, final IStatusCollection collection )
  {
    final int severity = status.getSeverity();
    final String message = status.getMessage();
    final int code = status.getCode();
    final GM_Object location = status instanceof IGeoStatus ? ((IGeoStatus)status).getLocation() : null;
    final Throwable t = status.getException();

    final IGeoStatus newStatus = m_statusCollection.createGeoStatus( severity, m_pluginId, code, message, t, location, time );

    final IStatus[] children = status.getChildren();
    for( final IStatus child : children )
      addAndCloneStatus( time, child, collection );

    return newStatus;
  }

  @Override
  public Date getStartTime( )
  {
    return m_startTime;
  }
}