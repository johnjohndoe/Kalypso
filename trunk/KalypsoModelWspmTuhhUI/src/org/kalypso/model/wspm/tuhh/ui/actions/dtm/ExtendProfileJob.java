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
package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.model.wspm.core.gml.WspmWaterBody;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.geometry.GM_Point;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Gernot Belger
 */
public class ExtendProfileJob extends UIJob implements ICreateProfileStrategy
{
  private final CreateProfileFromDEMWidget m_widget;

  private final ICoverageCollection m_coverages;

  private final TuhhReach m_reach;

  private final WspmWaterBody m_water;

  private final IMapPanel m_mapPanel;

  private final CommandableWorkspace m_commandableWorkspace;

  public ExtendProfileJob( final CreateProfileFromDEMWidget widget, final CommandableWorkspace commandableWorkspace, final IMapPanel mapPanel, final WspmWaterBody water, final TuhhReach reach, final ICoverageCollection coverages )
  {
    super( "Extend Profile" );
    m_widget = widget;
    m_commandableWorkspace = commandableWorkspace;
    m_mapPanel = mapPanel;
    m_water = water;
    m_reach = reach;
    m_coverages = coverages;
  }

  /**
   * @see org.eclipse.ui.progress.UIJob#runInUIThread(org.eclipse.core.runtime.IProgressMonitor)
   */
  @Override
  public IStatus runInUIThread( final IProgressMonitor monitor )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#getLabel()
   */
  @Override
  public String getLabel( )
  {
    return "Extend profile";
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#adjustPoint(org.kalypsodeegree.model.geometry.GM_Point,
   *      int)
   */
  @Override
  public GM_Point adjustPoint( final GM_Point pos, final int pointCount )
  {
    // TODO Auto-generated method stub
    return null;
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#run()
   */
  @Override
  public void run( )
  {
    final CreateProfileFromDEMWidget widget = m_widget;
    final IMapPanel mapPanel = m_mapPanel;

    final JobChangeAdapter listener = new JobChangeAdapter()
    {
      /**
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent event )
      {
        widget.activate( null, mapPanel );
        ExtendProfileJob.this.removeJobChangeListener( this );
      }
    };

    addJobChangeListener( listener );

    schedule();
  }
}
