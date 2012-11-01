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
package org.kalypso.model.rcm.util;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;

import javax.xml.namespace.QName;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.kalypso.commons.command.ICommand;
import org.kalypso.contribs.eclipse.core.runtime.StatusUtilities;
import org.kalypso.ogc.gml.command.ChangeFeaturesCommand;
import org.kalypso.ogc.gml.command.FeatureChange;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.geometry.GM_Exception;
import org.kalypsodeegree.model.geometry.GM_Polygon;

/**
 * @author Gernot Belger
 */
public final class ThiessenAreaJob extends Job
{
  private final boolean m_doThiessen;

  private ChangeFeaturesCommand m_change;

  private final GMLWorkspace m_workspace;

  private final List< ? > m_stations;

  private final QName m_propertyArea;

  private final IBoundaryCalculator m_boundaryCalculator;

  private final ThiessenAreaOperation m_worker;

  public ThiessenAreaJob( final boolean doThiessen, final IBoundaryCalculator boundaryCalculator, final GMLWorkspace workspace, final List< ? > stations, final QName propertyStation, final QName propertyArea, final QName propertyActive )
  {
    super( "Thiessen" ); //$NON-NLS-1$

    m_worker = new ThiessenAreaOperation( propertyStation, propertyActive );

    m_doThiessen = doThiessen;
    m_boundaryCalculator = boundaryCalculator;
    m_workspace = workspace;
    m_stations = stations;
    m_propertyArea = propertyArea;
  }

  @Override
  protected IStatus run( final IProgressMonitor monitor )
  {
    try
    {
      if( m_doThiessen )
        doThiessen( m_boundaryCalculator, monitor );
      else
        monitor.done();
      return Status.OK_STATUS;
    }
    catch( final GM_Exception e )
    {
      e.printStackTrace();
      return StatusUtilities.statusFromThrowable( e );
    }
    catch( final CoreException e )
    {
      return e.getStatus();
    }
  }

  protected void doThiessen( final IBoundaryCalculator boundaryCalculator, final IProgressMonitor monitor ) throws GM_Exception, CoreException
  {
    final Map<Feature, GM_Polygon> changeMap = m_worker.execute( m_stations, boundaryCalculator, monitor );

    final Collection<FeatureChange> changes = new ArrayList<>( changeMap.size() );

    for( final Map.Entry<Feature, GM_Polygon> entry : changeMap.entrySet() )
      changes.add( new FeatureChange( entry.getKey(), m_propertyArea, entry.getValue() ) );

    m_change = new ChangeFeaturesCommand( m_workspace, changes.toArray( new FeatureChange[changes.size()] ) );
  }

  public ICommand getChange( )
  {
    return m_change;
  }
}