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
import org.jfree.chart.ChartPanel;
import org.kalypso.eclipse.core.resources.ResourceUtilities;
import org.kalypso.eclipse.util.SetContentHelper;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewTemplate;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.template.TemplateStorage;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.editor.abstractobseditor.AbstractObservationEditor;

/**
 * Observation Diagram Editor.
 * 
 * @author schlienger
 */
public class ObservationDiagramEditor extends AbstractObservationEditor
{
  protected Frame m_diagFrame = null;

  protected ObservationChart m_obsChart = null;

  // TODO: maybe set a preference for this flag. It is currently always true.
  private boolean m_useAutoProxy = true;

  public ObservationDiagramEditor()
  {
    super( new DiagViewTemplate( true ) );
  }
  
  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    // SWT-AWT Brücke für die Darstellung von JFreeChart
    m_diagFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT
        | SWT.EMBEDDED ) );

    try
    {
      m_obsChart = new ObservationChart( (DiagViewTemplate) getTemplate() );
      
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
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose( )
  {
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
    final DiagViewTemplate template = (DiagViewTemplate) getTemplate();
    if( template == null )
      return;

    final SetContentHelper helper = new SetContentHelper()
    {
      protected void write( Writer writer ) throws Throwable
      {
        final ObsdiagviewType type = DiagViewUtils
            .buildDiagramTemplateXML( template );

        DiagViewUtils.saveDiagramTemplateXML( type, writer );
      }
    };

    helper.setFileContents( input.getFile(), false, true, monitor );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#loadInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void loadInternal( final IProgressMonitor monitor,
      final IStorageEditorInput input )
  {
    monitor.beginTask( "Diagramm-Vorlage laden", IProgressMonitor.UNKNOWN );

    final DiagViewTemplate template = (DiagViewTemplate) getTemplate();
    
    try
    {
      final IStorage storage = input.getStorage();

      if( storage instanceof TemplateStorage )
      {
        final TemplateStorage ts = (TemplateStorage) storage;
        template.setTitle( ts.getName() );

        template.addObservation( ts.getName(), ts.getContext(), ts.getHref(),
            "zml", false, null );
      }
      else
      {
        final ObsdiagviewType baseTemplate = DiagViewUtils
            .loadDiagramTemplateXML( storage.getContents() );

        final String strUrl = ResourceUtilities.createURLSpec( input
            .getStorage().getFullPath() );
        template.setBaseTemplate( baseTemplate, new URL( strUrl ) );
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
   * @return chart
   */
  public ObservationChart getChart( )
  {
    return m_obsChart;
  }
}