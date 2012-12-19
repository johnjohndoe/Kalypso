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
package org.kalypso.kalypsomodel1d2d.ui.map.cline;

import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.IJobChangeListener;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.ui.progress.IProgressConstants2;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypsodeegree.model.geometry.GM_Curve;

/**
 * Represants one 'egde' of a continuity line in process of beeing edited (i.e. two nodes clicked by the user).
 * 
 * @author Gernot Belger
 */
class ContinuityEdge
{
//  private final ISchedulingRule m_previewJobRule = new MutexRule();

  private final IJobChangeListener m_previewJobListener = new JobChangeAdapter()
  {
    @Override
    public void done( final IJobChangeEvent event )
    {
      handlePreviewCalculated( event.getJob() );
    }
  };

//  private final WeightedGraph<IFE1D2DNode, IFE1D2DEdge> m_discGraph;

  private final IFE1D2DNode m_startNode;

  private final IFE1D2DNode m_endNode;

  private final CreateFEContinuityLineWidget m_widget;

  private ContinuityLinePreviewJob m_previewJob = null;

  private GM_Curve m_preview;

  public ContinuityEdge( final CreateFEContinuityLineWidget widget, final IFE1D2DNode startNode, final IFE1D2DNode endNode )
  {
    m_widget = widget;
    m_startNode = startNode;
    m_endNode = endNode;

    m_previewJob = startPreviewJob();
  }

  public void dispose( )
  {
    if( m_previewJob != null )
      m_previewJob.cancel();
  }

  public IFE1D2DNode getStartNode( )
  {
    return m_startNode;
  }

  public IFE1D2DNode getEndNode( )
  {
    return m_endNode;
  }

  private ContinuityLinePreviewJob startPreviewJob( )
  {
    if( m_startNode == null )
      return null;

    if( m_previewJob != null )
      m_previewJob.cancel();

    final ContinuityLinePreviewJob job = new ContinuityLinePreviewJob( new IFE1D2DNode[] { m_startNode, m_endNode } );

    job.setUser( false );
    job.setSystem( true );
    job.setProperty( IProgressConstants2.NO_IMMEDIATE_ERROR_PROMPT_PROPERTY, Boolean.TRUE );

    job.addJobChangeListener( m_previewJobListener );

    job.schedule();

    return job;
  }

  public GM_Curve getPathAsGeometry( )
  {
    return m_preview;
  }

  protected void handlePreviewCalculated( final Job job )
  {
    final ContinuityLinePreviewJob previewJob = (ContinuityLinePreviewJob)job;
    m_preview = previewJob.getPreview();

    m_widget.previewChanged();
  }

  public IFE1D2DNode[] waitForPath( ) throws InterruptedException
  {
    if( m_previewJob == null )
      return new IFE1D2DNode[0];

    m_previewJob.join();

    return m_previewJob.getContinuityNodes();
  }

  public IFE1D2DNode[] getPath( )
  {
    if( m_preview == null )
      return null;

    return m_previewJob.getContinuityNodes();
  }
}