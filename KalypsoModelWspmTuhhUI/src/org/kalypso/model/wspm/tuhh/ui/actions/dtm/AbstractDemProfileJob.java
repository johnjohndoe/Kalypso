package org.kalypso.model.wspm.tuhh.ui.actions.dtm;

import java.awt.Graphics;
import java.awt.Point;

import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.IJobChangeEvent;
import org.eclipse.core.runtime.jobs.JobChangeAdapter;
import org.eclipse.ui.progress.UIJob;
import org.kalypso.grid.RichCoverageCollection;
import org.kalypso.model.wspm.tuhh.core.gml.TuhhReach;
import org.kalypso.model.wspm.tuhh.ui.KalypsoModelWspmTuhhUIPlugin;
import org.kalypso.model.wspm.tuhh.ui.i18n.Messages;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.widgets.builders.LineGeometryBuilder;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.geometry.GM_Curve;
import org.kalypsodeegree_impl.gml.binding.commons.ICoverageCollection;

/**
 * @author Gernot Belger
 */
abstract class AbstractDemProfileJob extends UIJob implements ICreateProfileStrategy
{
  private final CreateProfileFromDEMWidget m_widget;

  private final CommandableWorkspace m_commandableWorkspace;

  private final IMapPanel m_mapPanel;

  private final TuhhReach m_reach;

  private final ICoverageCollection m_coverages;

  private final double m_simplifyDistance;

  private LineGeometryBuilder m_geoBuilder;

  private GM_Curve m_curve;

  public AbstractDemProfileJob( final String title, final CreateProfileFromDEMWidget widget, final CommandableWorkspace commandableWorkspace, final IMapPanel mapPanel, final TuhhReach reach, final ICoverageCollection coverages, final double simplifyDistance )
  {
    super( title );

    m_widget = widget;
    m_commandableWorkspace = commandableWorkspace;
    m_mapPanel = mapPanel;

    m_reach = reach;
    m_coverages = coverages;
    m_simplifyDistance = simplifyDistance;

    m_geoBuilder = new LineGeometryBuilder( 0, mapPanel.getMapModell().getCoordinatesSystem() );
  }

  @Override
  public void dispose( )
  {
    if( m_geoBuilder != null )
    {
      m_geoBuilder.reset();
      m_geoBuilder = null;
    }
  }

  @Override
  public IStatus runInUIThread( final IProgressMonitor arg0 )
  {
    try
    {
      /* The builder for a profile from a DEM. */
      final RichCoverageCollection richCoverages = new RichCoverageCollection( m_coverages );

      return runJob( m_curve, richCoverages );
    }
    catch( final Exception e )
    {
      e.printStackTrace();

      return new Status( IStatus.ERROR, KalypsoModelWspmTuhhUIPlugin.getID(), Messages.getString("AbstractDemProfileJob_0") ); //$NON-NLS-1$
    }
  }

  protected abstract IStatus runJob( GM_Curve curve, RichCoverageCollection richCoverages ) throws Exception;

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#run()
   */
  @Override
  public void run( )
  {
    final CreateProfileFromDEMWidget widget = m_widget;
    final IMapPanel mapPanel = m_mapPanel;

    try
    {
      // remove last point: as we are using leftPressed, we always get two point on double clicks
      getGeoBuilder().removeLastPoint();
      m_curve = (GM_Curve) m_geoBuilder.finish();
      m_geoBuilder = null;
    }
    catch( final Exception e )
    {
      e.printStackTrace();
      return;
    }

    final JobChangeAdapter listener = new JobChangeAdapter()
    {
      /**
       * @see org.eclipse.core.runtime.jobs.JobChangeAdapter#done(org.eclipse.core.runtime.jobs.IJobChangeEvent)
       */
      @Override
      public void done( final IJobChangeEvent event )
      {
        widget.activate( null, mapPanel );
        AbstractDemProfileJob.this.removeJobChangeListener( this );
      }
    };

    addJobChangeListener( listener );

    schedule();
  }

  /**
   * @see org.kalypso.model.wspm.tuhh.ui.actions.dtm.ICreateProfileStrategy#paint(java.awt.Graphics,
   *      org.kalypso.ogc.gml.map.IMapPanel, java.awt.Point)
   */
  @Override
  public void paint( final Graphics g, final IMapPanel mapPanel, final Point currentPoint )
  {
    if( currentPoint == null || m_geoBuilder == null )
      return;

    final GeoTransform projection = mapPanel.getProjection();
    m_geoBuilder.paint( g, projection, currentPoint );
  }

  protected final LineGeometryBuilder getGeoBuilder( )
  {
    return m_geoBuilder;
  }

  protected final TuhhReach getReach( )
  {
    return m_reach;
  }

  protected final CommandableWorkspace getWorkspace( )
  {
    return m_commandableWorkspace;
  }

  protected final IMapPanel getMapPanel( )
  {
    return m_mapPanel;
  }

  protected final double getSimplifyDistance( )
  {
    return m_simplifyDistance;
  }
}