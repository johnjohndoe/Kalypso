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
import java.io.OutputStreamWriter;

import org.apache.commons.configuration.Configuration;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.IWizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.jfree.chart.ChartPanel;
import org.kalypso.commons.resources.SetContentHelper;
import org.kalypso.metadoc.IExportableObject;
import org.kalypso.metadoc.IExportableObjectFactory;
import org.kalypso.metadoc.configuration.IPublishingConfiguration;
import org.kalypso.metadoc.ui.ImageExportPage;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.diagview.jfreechart.ExportableChart;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.ui.editor.abstractobseditor.AbstractObservationEditor;

/**
 * Observation Diagram Editor.
 * 
 * @author schlienger
 */
public class ObservationDiagramEditor extends AbstractObservationEditor implements IExportableObjectFactory
{
  protected Frame m_diagFrame = null;

  protected ObservationChart m_obsChart = null;

  private Composite m_swingContainer;

  public ObservationDiagramEditor()
  {
    super( new DiagView( true ) );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#dispose()
   */
  public void dispose()
  {
    if( m_obsChart != null )
      m_obsChart.dispose();

    super.dispose();
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#createPartControl(org.eclipse.swt.widgets.Composite)
   */
  public void createPartControl( Composite parent )
  {
    super.createPartControl( parent );

    m_swingContainer = new Composite( parent, SWT.RIGHT | SWT.EMBEDDED );
    m_diagFrame = SWT_AWT.new_Frame( m_swingContainer );

    try
    {
      m_obsChart = new ObservationChart( (DiagView)getView() );

      // chart panel without any popup menu
      final ChartPanel chartPanel = new ChartPanel( m_obsChart, false, false, false, false, false );
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
   * @see org.kalypso.ui.editor.abstractobseditor.AbstractObservationEditor#getAdapter(java.lang.Class)
   */
  public Object getAdapter( final Class adapter )
  {
    if( adapter == IExportableObjectFactory.class )
      return this;

    return super.getAdapter( adapter );
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#doSaveInternal(org.eclipse.core.runtime.IProgressMonitor,
   *      org.eclipse.ui.IFileEditorInput)
   */
  protected void doSaveInternal( IProgressMonitor monitor, IFileEditorInput input ) throws CoreException
  {
    final DiagView template = (DiagView)getView();
    if( template == null )
      return;

    final SetContentHelper helper = new SetContentHelper()
    {
      protected void write( final OutputStreamWriter writer ) throws Throwable
      {
        final ObsdiagviewType type = DiagViewUtils.buildDiagramTemplateXML( template );

        DiagViewUtils.saveDiagramTemplateXML( type, writer );
      }
    };

    helper.setFileContents( input.getFile(), false, true, monitor );
  }

  /**
   * @return chart
   */
  public ObservationChart getChart()
  {
    return m_obsChart;
  }

  /**
   * @see org.kalypso.ui.editor.AbstractEditorPart#setFocus()
   */
  public void setFocus()
  {
    if( m_swingContainer != null )
      m_swingContainer.setFocus();
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createExportableObjects(org.apache.commons.configuration.Configuration,
   *      org.apache.commons.configuration.Configuration)
   */
  public IExportableObject[] createExportableObjects( final Configuration conf, final Configuration metadataExtensions )
  {
    return new IExportableObject[]
    { new ExportableChart( m_obsChart, conf.getString( ImageExportPage.CONF_IMAGE_FORMAT,
        ExportableChart.DEFAULT_FORMAT ),
        conf.getInt( ImageExportPage.CONF_IMAGE_WIDTH, ExportableChart.DEFAULT_WIDTH ), conf.getInt(
            ImageExportPage.CONF_IMAGE_HEIGHT, ExportableChart.DEFAULT_HEIGHT ), metadataExtensions ) };
  }

  /**
   * @see org.kalypso.metadoc.IExportableObjectFactory#createWizardPages(IPublishingConfiguration)
   */
  public IWizardPage[] createWizardPages( final IPublishingConfiguration configuration )
  {
    final ImageDescriptor imgDesc = AbstractUIPlugin.imageDescriptorFromPlugin( KalypsoGisPlugin.getId(),
        "icons/util/img_props.gif" );
    final IWizardPage page = new ImageExportPage( configuration, "diagprops", "Bildexport Optionen", imgDesc );

    return new IWizardPage[]
    { page };
  }
}