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
package org.kalypso.ogc.sensor.jface;

import java.awt.Frame;
import java.net.URL;

import javax.swing.BorderFactory;
import javax.swing.JScrollPane;

import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.TitleAreaDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.graphics.Point;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Shell;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagViewTemplate;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.tableview.TableViewTemplate;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ui.KalypsoGisPlugin;
import org.kalypso.util.pool.IPoolListener;
import org.kalypso.util.pool.IPoolableObjectType;
import org.kalypso.util.pool.PoolableObjectType;
import org.kalypso.util.pool.ResourcePool;
import org.kalypso.zml.obslink.TimeseriesLink;

/**
 * @author belger
 */
public class ObservationLinkDialog extends TitleAreaDialog implements IPoolListener
{
//  private IPoolableObjectType m_key;
  private final ResourcePool m_pool;

  private final TimeseriesLink m_timeserie;

  private final DiagViewTemplate m_diagTemplate = new DiagViewTemplate();
  private final TableViewTemplate m_tableTemplate = new TableViewTemplate();

  private ObservationChart m_chart;
  private ObservationTable m_table;


  public ObservationLinkDialog( final Shell parentShell, final TimeseriesLink obslink,
      final URL context )
  {
    super( parentShell );

    setShellStyle( getShellStyle() | SWT.RESIZE );

    m_timeserie = obslink;

    m_pool = KalypsoGisPlugin.getDefault().getPool();

    if( obslink != null )
    {
      final PoolableObjectType m_key = new PoolableObjectType( obslink.getLinktype(), obslink.getHref(), context );
      m_pool.addPoolListener( this, m_key );
    }
  }

  public void dispose()
  {
    m_pool.removePoolListener( this );
    m_diagTemplate.removeTemplateEventListener( m_chart );
    
    m_table.dispose();
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#getInitialSize()
   */
  protected Point getInitialSize()
  {
    return new Point( 500, 400 );
  }

  public TimeseriesLink getResult()
  {
    return m_timeserie;
  }

  /**
   * @see org.eclipse.jface.dialogs.TitleAreaDialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    final Composite control = new Composite( parent, SWT.NONE );
    final GridLayout layout = new GridLayout();
    layout.marginHeight = 0;
    layout.marginWidth = 0;
    layout.verticalSpacing = 50;
    control.setLayout( layout );
    final GridData titleGridData = new GridData( GridData.FILL_HORIZONTAL );
    final GridData layoutGridData = titleGridData;
    control.setLayoutData( layoutGridData );
    control.setFont( parent.getFont() );

    // Build the separator line
    final Label titleBarSeparator = new Label( control, SWT.HORIZONTAL | SWT.SEPARATOR );
    titleBarSeparator.setLayoutData( titleGridData );

    final SashForm sashForm = new SashForm( parent, SWT.HORIZONTAL );
    sashForm.setLayoutData( new GridData( GridData.FILL_BOTH ) );

    createDiagram( sashForm );
    createTable( sashForm );

    setMessage( "Zeitreihenansicht" );

    return control;
  }

  private void createDiagram( final Composite parent )
  {
    try
    {
      m_chart = new ObservationChart( m_diagTemplate );
    }
    catch( SensorException e )
    {
      MessageDialog.openError( parent.getShell(), "", e.getLocalizedMessage() );
      return;
    }

    m_diagTemplate.addTemplateEventListener( m_chart );

    ChartPanel chartPanel = new ChartPanel( m_chart );
    chartPanel.setMouseZoomable( true, false );

    Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );
  }

  private void createTable( final Composite parent )
  {
    m_table = new ObservationTable( m_tableTemplate );

    final Frame vFrame = SWT_AWT.new_Frame( new Composite( parent, SWT.RIGHT | SWT.EMBEDDED ) );

    vFrame.setVisible( true );
    m_table.setVisible( true );

    final JScrollPane pane = new JScrollPane( m_table );
    pane.setBorder( BorderFactory.createEmptyBorder() );
    vFrame.add( pane );
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectLoaded(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object, org.eclipse.core.runtime.IStatus)
   */
  public void objectLoaded( IPoolableObjectType key, Object newValue, IStatus status )
  {
    if( status.isOK() )
    {
      try
      {
        final IObservation obs = (IObservation) newValue;
        
        //m_diagTemplate.removeAllCurves();
        m_diagTemplate.setObservation( obs, null );

        //m_tableTemplate.removeAllColumns();
        m_tableTemplate.setObservation( obs, null );
      }
      catch( Exception e )
      {
        e.printStackTrace();
      }
    }
  }

  /**
   * @see org.kalypso.util.pool.IPoolListener#objectInvalid(org.kalypso.util.pool.IPoolableObjectType, java.lang.Object)
   */
  public void objectInvalid( IPoolableObjectType key, Object oldValue )
  {
    //
  }
}