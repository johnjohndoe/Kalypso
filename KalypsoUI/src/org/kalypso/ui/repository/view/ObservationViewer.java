/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal
 * engineering Denickestr. 22 21073 Hamburg, Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany
 * http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.ui.repository.view;

import java.awt.Frame;
import java.net.MalformedURLException;
import java.net.URL;

import org.bce.eclipse.ui.views.propertysheet.SimplePropertySheetViewer;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.awt.SWT_AWT;
import org.eclipse.swt.custom.SashForm;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Text;
import org.jfree.chart.ChartPanel;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.ogc.sensor.tableview.TableView;
import org.kalypso.ogc.sensor.tableview.swing.ObservationTable;
import org.kalypso.ogc.sensor.template.NameUtils;
import org.kalypso.ogc.sensor.template.ObsView;
import org.kalypso.ogc.sensor.template.PlainObsProvider;
import org.kalypso.ogc.sensor.template.ObsView.ItemData;
import org.kalypso.ogc.sensor.view.propertySource.ObservationPropertySource;
import org.kalypso.ogc.sensor.zml.ZmlFactory;
import org.kalypso.util.url.UrlResolverSingleton;

/**
 * ObservationViewer
 * <p>
 * 
 * @author schlienger (23.05.2005)
 */
public class ObservationViewer extends Composite
{
  private Label m_lblObs;
  private Label m_lblFilter;

  protected Text m_txtHref;
  private Text m_txtFilter;

  private Button m_btnSelectObs;
  private Button m_btnSelectFilter;

  private SimplePropertySheetViewer m_mdViewer;

  private final DiagView m_diagView = new DiagView();
  private ObservationChart m_chart;

  private final TableView m_tableView = new TableView();
  private ObservationTable m_table;

  private URL m_context;
  private String m_href;

  public ObservationViewer( final Composite parent, final int style )
  {
    super( parent, style );

    createControl();
  }

  private final void createControl()
  {
    final GridLayout gridLayout = new GridLayout( 3, false );
    setLayout( gridLayout );
    setLayoutData( new GridData( GridData.FILL_BOTH ) );

    createHeaderForm();

    final SashForm form = new SashForm( this, SWT.HORIZONTAL );
    final GridData gd = new GridData( GridData.FILL_BOTH );
    gd.horizontalSpan = 3;
    gd.verticalSpan = 3;
    form.setLayoutData( gd );

    createMetadataAndTableForm( form );
    createDiagramForm( form );
  }

  private void createHeaderForm()
  {
    m_lblObs = new Label( this, SWT.LEFT );
    m_lblObs.setText( "Zeitreihe:" );

    m_txtHref = new Text( this, SWT.MULTI | SWT.WRAP );
    m_txtHref.setSize( 400, m_txtHref.getSize().y );
    final GridData gd1 = new GridData( GridData.FILL_HORIZONTAL
        | GridData.VERTICAL_ALIGN_FILL );
    gd1.verticalSpan = 2;
    m_txtHref.setLayoutData( gd1 );
    m_txtHref.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        setHref( m_txtHref.getText() );
      }
    } );

    m_btnSelectObs = new Button( this, SWT.NONE );
    m_btnSelectObs.setText( "..." );
    //m_btnSelect.setLayoutData( new GridData( GridData.HORIZONTAL_ALIGN_END )
    // );
    m_btnSelectObs.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        final ObservationChooserDialog dlg = new ObservationChooserDialog(
            getShell() );
        dlg.setSelectedObservation( m_txtHref.getText() );
        if( dlg.open() == Window.OK )
        {
          final String href = dlg.getSelectedObservation();
          if( href != null )
            m_txtHref.setText( href );
        }
      }

      public void widgetDefaultSelected( final SelectionEvent e )
      {
        // nothing
      }
    } );

    m_lblFilter = new Label( this, SWT.LEFT );
    m_lblFilter.setText( "Filter:" );

    m_txtFilter = new Text( this, SWT.MULTI | SWT.WRAP );
    m_txtFilter.setSize( 400, m_txtFilter.getSize().y );
    final GridData gd2 = new GridData( GridData.FILL_HORIZONTAL
        | GridData.VERTICAL_ALIGN_FILL );
    gd2.verticalSpan = 2;
    m_txtFilter.setLayoutData( gd2 );
    m_txtFilter.addModifyListener( new ModifyListener()
    {
      public void modifyText( final ModifyEvent e )
      {
        //        setHref( m_txtHref.getText() );
      }
    } );

    m_btnSelectFilter = new Button( this, SWT.NONE );
    m_btnSelectFilter.setText( "..." );
    //m_btnSelectFilter.setLayoutData( new GridData(
    // GridData.HORIZONTAL_ALIGN_END ) );
    m_btnSelectFilter.addSelectionListener( new SelectionListener()
    {
      public void widgetSelected( final SelectionEvent e )
      {
        // TODO
      }

      public void widgetDefaultSelected( final SelectionEvent e )
      {
        // nothing
      }
    } );
  }

  private void createDiagramForm( final SashForm form )
  {
    try
    {
      m_chart = new ObservationChart( m_diagView );
    }
    catch( final SensorException e1 )
    {
      e1.printStackTrace();
      throw new IllegalStateException( e1.getLocalizedMessage() );
    }
    // chart panel without any popup menu
    final ChartPanel chartPanel = new ChartPanel( m_chart, false, false, false,
        false, false );
    chartPanel.setMouseZoomable( true, false );
    final Composite chartComp = new Composite( form, SWT.RIGHT | SWT.EMBEDDED );
    final Frame vFrame = SWT_AWT.new_Frame( chartComp );
    vFrame.setVisible( true );
    chartPanel.setVisible( true );
    vFrame.add( chartPanel );
  }

  private void createMetadataAndTableForm( final SashForm form )
  {
    final SashForm form2 = new SashForm( form, SWT.VERTICAL );

    // METADATA
    m_mdViewer = new SimplePropertySheetViewer( form2 );

    // TABLE
    m_table = new ObservationTable( m_tableView );
    final Composite tableComp = new Composite( form2, SWT.RIGHT | SWT.EMBEDDED );
    final Frame vFrame = SWT_AWT.new_Frame( tableComp );
    vFrame.setVisible( true );
    vFrame.add( m_table );
  }

  /**
   * @see org.eclipse.swt.widgets.Widget#dispose()
   */
  public void dispose()
  {
    m_diagView.dispose();
    if( m_chart != null )
      m_chart.dispose();

    m_tableView.dispose();
    if( m_table != null )
      m_table.dispose();

    super.dispose();
  }

  protected void setHref( final String href )
  {
    final URL url;
    try
    {
      url = UrlResolverSingleton.resolveUrl( m_context, href );
    }
    catch( final MalformedURLException e )
    {
      return;
    }

    try
    {
      final IObservation obs = ZmlFactory.parseXML( url, href );

      m_href = href;

      updateViewer( obs );
    }
    catch( final SensorException e )
    {
      e.printStackTrace();

      MessageDialog.openError( getShell(), "Fehler beim Laden", e
          .getLocalizedMessage() );
    }
  }

  public void setHref( final URL context, final String href )
  {
    m_context = context;

    m_txtHref.setText( href );

    //setHref( href ); is called implicitely through m_txtHref.setText();
  }

  public String getHref()
  {
    return m_href;
  }

  private void updateViewer( final IObservation obs )
  {
    m_mdViewer.setInput( new ObservationPropertySource( obs ) );

    final PlainObsProvider pop = new PlainObsProvider( obs, null );
    final ItemData itd = new ObsView.ItemData( false, null );

    m_diagView.removeAllItems();
    m_diagView.addObservation( pop, NameUtils.DEFAULT_ITEM_NAME, null, itd );

    m_tableView.removeAllItems();
    m_tableView.addObservation( pop, NameUtils.DEFAULT_ITEM_NAME, null, itd );
  }
}
