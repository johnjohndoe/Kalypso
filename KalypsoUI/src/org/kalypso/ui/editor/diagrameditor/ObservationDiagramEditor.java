/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
package org.kalypso.ui.editor.diagrameditor;

import java.awt.Frame;
import java.io.Writer;
import java.net.URL;

import org.eclipse.core.resources.IStorage;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.IStorageEditorInput;
import org.eclipse.ui.views.contentoutline.IContentOutlinePage;
import org.jfree.chart.ChartPanel;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagramTemplateUtils;
import org.kalypso.ogc.sensor.diagview.impl.DiagViewTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.proxy.AutoProxyFactory;
import org.kalypso.ogc.sensor.template.ITemplateEventListener;
import org.kalypso.ogc.sensor.template.TemplateEvent;
import org.kalypso.ogc.sensor.template.TemplateStorage;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.editor.AbstractEditorPart;

/**
 * Observation Diagram Editor.
 * 
 * @author schlienger
 */
public class ObservationDiagramEditor extends AbstractEditorPart implements
    ITemplateEventListener
{
  protected final DiagViewTemplate m_template = new DiagViewTemplate();

  protected Frame m_diagFrame = null;

  protected ObservationChart m_obsChart = null;

  protected ObsDiagOutlinePage m_outline = null;

  private boolean m_dirty = false;

  // TODO: maybe set a preference for this flag. It is currently always true.
  private boolean m_useAutoProxy = true;

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    m_diagFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    // listener on template in order to set dirty flag when template changes
    m_template.addTemplateEventListener( this );

    try
    {
      m_obsChart = new ObservationChart( m_template );
      m_template.addTemplateEventListener( m_obsChart );

      // chart panel without any popup menu
      final ChartPanel chartPanel = new ChartPanel( m_obsChart, false, false,
          false, false, false );
      chartPanel.setMouseZoomable( true, false );
      m_diagFrame.add( chartPanel );

      m_diagFrame.setVisible( true );
    }
    catch( SensorException e )
    {
      e.printStackTrace();
    }
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#getAdapter(java.lang.Class)
   */
  public Object getAdapter( Class adapter )
  {
    if( adapter == IContentOutlinePage.class )
    {
      // lazy loading
      if( m_outline == null || m_outline.getControl() != null
          && m_outline.getControl().isDisposed() )
      {
        // dispose when not null (not sure if this is ok)
        if( m_outline != null )
          m_outline.dispose();

        m_outline = new ObsDiagOutlinePage();
        m_outline.setTemplate( m_template );
      }

      return m_outline;
    }

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
    if( m_template != null )
    {
      m_template.removeTemplateEventListener( this );
      m_template.removeTemplateEventListener( m_obsChart );
      m_template.dispose();
    }

    if( m_outline != null )
      m_outline.dispose();

    if( m_obsChart != null )
      m_obsChart.dispose();

    super.dispose();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor,
      IFileEditorInput input ) throws CoreException
  {
    if( m_template == null )
      return;

    final SetContentHelper thread = new SetContentHelper()
    {
      protected void write( Writer writer ) throws Throwable
      {
        final ObsdiagviewType type = DiagramTemplateUtils
            .buildDiagramTemplateXML( m_template );

        DiagramTemplateUtils.saveDiagramTemplateXML( type, writer );

        resetDirty();
      }
    };

    thread.setFileContents( input.getFile(), false, true, monitor );

    fireDirty();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor,
      final IStorageEditorInput input )
  {
    monitor.beginTask( "Vorlage Laden", IProgressMonitor.UNKNOWN );

    try
    {
      final IStorage storage = input.getStorage();

      if( storage instanceof TemplateStorage )
      {
        final TemplateStorage ts = (TemplateStorage) storage;
        m_template.setTitle( ts.getName() );

        if( m_useAutoProxy )
          m_template.setProxyFactory( AutoProxyFactory.getInstance() );

        m_template.addObservation( ts.getName(), ts.getContext(), ts.getHref(),
            "zml", false, null );
      }
      else
      {
        final ObsdiagviewType baseTemplate = DiagramTemplateUtils
            .loadDiagramTemplateXML( storage.getContents() );

        final String strUrl = ResourceUtilities.createURLSpec( input
            .getStorage().getFullPath() );
        m_template.setBaseTemplate( baseTemplate, new URL( strUrl ) );
      }
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
    finally
    {
      monitor.done();
    }
  }

  /**
   * @see org.kalypso.ogc.sensor.template.ITemplateEventListener#onTemplateChanged(org.kalypso.ogc.sensor.template.TemplateEvent)
   */
  public void onTemplateChanged( TemplateEvent evt )
  {
    if( evt.isType( TemplateEvent.TYPE_ADD | TemplateEvent.TYPE_REMOVE
        | TemplateEvent.TYPE_REMOVE_ALL ) )
    {
      m_dirty = true;

      getSite().getShell().getDisplay().asyncExec( new Runnable()
      {
        public void run( )
        {
          fireDirty();
        }
      } );
    }
  }

  protected void resetDirty( )
  {
    m_dirty = false;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#isDirty()
   */
  public boolean isDirty( )
  {
    return m_dirty;
  }

  /**
   * @return chart
   */
  public ObservationChart getChart( )
  {
    return m_obsChart;
  }

  /**
   * @return template
   */
  public DiagViewTemplate getTemplate( )
  {
    return m_template;
  }
}