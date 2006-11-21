/*
 * --------------- Kalypso-Header --------------------------------------------
 * 
 * This file is part of kalypso. Copyright (C) 2004, 2005 by:
 * 
 * Technical University Hamburg-Harburg (TUHH) Institute of River and coastal engineering Denickestr. 22 21073 Hamburg,
 * Germany http://www.tuhh.de/wb
 * 
 * and
 * 
 * Bjoernsen Consulting Engineers (BCE) Maria Trost 3 56070 Koblenz, Germany http://www.bjoernsen.de
 * 
 * This library is free software; you can redistribute it and/or modify it under the terms of the GNU Lesser General
 * Public License as published by the Free Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License along with this library; if not, write to
 * the Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 * Contact:
 * 
 * E-Mail: belger@bjoernsen.de schlienger@bjoernsen.de v.doemming@tuhh.de
 * 
 * ------------------------------------------------------------------------------------
 */
package org.kalypso.ogc.sensor.view;

import java.net.URL;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.eclipse.compare.internal.ResizableDialog;
import org.eclipse.jface.window.Window;
import org.eclipse.swt.SWT;
import org.eclipse.swt.dnd.Clipboard;
import org.eclipse.swt.dnd.TextTransfer;
import org.eclipse.swt.dnd.Transfer;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.ui.controls.ButtonControl;
import org.kalypso.contribs.java.util.ValueIterator;
import org.kalypso.ogc.sensor.IAxis;
import org.kalypso.ogc.sensor.IObservation;
import org.kalypso.ogc.sensor.ITuppleModel;
import org.kalypso.ogc.sensor.MetadataList;
import org.kalypso.ogc.sensor.SensorException;
import org.kalypso.ogc.sensor.impl.SimpleObservation;
import org.kalypso.ogc.sensor.impl.SimpleTuppleModel;
import org.kalypso.ogc.sensor.timeseries.TimeserieUtils;
import org.kalypso.ogc.sensor.zml.ZmlFactory;

/**
 * ObservationViewerDialog
 * <p>
 * 
 * @author schlienger (24.05.2005)
 */
public class ObservationViewerDialog extends ResizableDialog
{
  private ObservationViewer m_viewer;

  private URL m_context;

  private boolean m_withHeader;

  private boolean m_withMetaDataTable;

  private boolean m_withChart;

  private final int m_buttonControls;

  // button types are bitmask !
  public final static int NO_BUTTON = 0;

  public final static int BUTTON_NEW = 1;

  public final static int BUTTON_REMOVE = 2;

  public final static int BUTTON_NEW_IDEAL_LANDUSE = 32;

  public static final int BUTTON_EXEL_IMPORT = 4;

  public static final int BUTTON_EXEL_EXPORT = 8;

  final String[] m_axisTypes;

  private Object m_input = null;

  public ObservationViewerDialog( final Shell parent, boolean withHeaderForm, boolean withMetaDataAndTable,
      boolean withChart, final int buttonControls, final String[] axisTypes )
  {
    super( parent, null );

    m_withHeader = withHeaderForm;
    m_withMetaDataTable = withMetaDataAndTable;
    m_withChart = withChart;
    m_buttonControls = buttonControls;
    m_axisTypes = axisTypes;
  }

  public ObservationViewerDialog( final Shell parent )
  {
    super( parent, null );

    m_withHeader = true;
    m_withMetaDataTable = true;
    m_withChart = true;
    m_buttonControls = NO_BUTTON;
    m_axisTypes = null;
  }

  /**
   * @see org.eclipse.jface.dialogs.Dialog#createDialogArea(org.eclipse.swt.widgets.Composite)
   */
  protected Control createDialogArea( final Composite parent )
  {
    final Composite composite = (Composite)super.createDialogArea( parent );
    composite.setLayout( new FillLayout() );

    m_viewer = new ObservationViewer( composite, SWT.NONE, m_withHeader, m_withChart, m_withMetaDataTable,
        createButtonControls() );
    updateViewer();
    // TODO label
    getShell().setText( "Zeitreihenlink-Editor" );
    return composite;
  }

  public void setContext( final URL context )
  {
    m_context = context;
    updateViewer();
  }

  public void setInput( Object newInput )
  {
    m_input = newInput;
    updateViewer();
  }

  private void updateViewer()
  {
    if( m_viewer != null )
    {
      m_viewer.setContext( m_context );
      m_viewer.setInput( m_input );
    }
  }

  /**
   * @return buttoncontrols
   */
  private ButtonControl[] createButtonControls()
  {
    final List result = new ArrayList();
    final IAxis[] axis = TimeserieUtils.createDefaultAxes( m_axisTypes, true );

    if( ( m_buttonControls & BUTTON_REMOVE ) == BUTTON_REMOVE )
    {
      final SelectionListener removeListener = new SelectionListener()
      {
        public void widgetSelected( SelectionEvent e )
        {
          setInput( null );
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        // TODO Auto-generated method stub
        }
      };
      result.add( new ButtonControl( removeListener, "loeschen", "Zeitreihe loeschen", SWT.PUSH ) );
    }
    if( ( m_buttonControls & BUTTON_NEW ) == BUTTON_NEW )
    {
      final SelectionListener newListener = new SelectionListener()
      {
        public void widgetSelected( SelectionEvent e )
        {
          final AxisRangeDialog dialog = new AxisRangeDialog( getShell(), null, m_axisTypes[0] );
          if( dialog.open() == Window.OK )
          {
            if( !dialog.isValid() )
              return;// TODO messagebox
            final String name = dialog.getName().toString();
            final Object min = dialog.getMin();
            final Object intervall = dialog.getInt();
            final int rows = dialog.getCount();

            final Object[][] values = new Object[rows][axis.length];
            final Iterator iterator = new ValueIterator( min, intervall, rows );
            for( int row = 0; row < rows; row++ )
            {
              values[row][0] = iterator.next();
              for( int ax = 1; ax < axis.length; ax++ )
                values[row][ax] = dialog.getDefault();
            }
            final ITuppleModel model = new SimpleTuppleModel( axis, values );
            setInput( new SimpleObservation( null, null, name, true, null, new MetadataList(), axis, model ) );
          }
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        // TODO Auto-generated method stub
        }
      };
      result.add( new ButtonControl( newListener, "neu", "Neue Zeitreihe anlegen", SWT.PUSH ) );
    }
    if( ( m_buttonControls & BUTTON_NEW_IDEAL_LANDUSE ) == BUTTON_NEW_IDEAL_LANDUSE )
    {
      final SelectionListener newListener = new SelectionListener()
      {
        public void widgetSelected( SelectionEvent e )
        {

          final String name = "Idealisierter Jahres Vegetationszyklus";
          final Calendar startDate = Calendar.getInstance();
          startDate.set( 2000, 11, 15 );
          final Calendar idealMonth = Calendar.getInstance();
          idealMonth.setTimeInMillis( 30 * 24 * 60 * 60 * 1000 );
          final Object intervall = new Date( idealMonth.getTimeInMillis() );
          final Object min = new Date( startDate.getTimeInMillis() );
          final int months = 12;

          final Object[][] values = new Object[months][axis.length];
          final Iterator iterator = new ValueIterator( min, intervall, months );
          for( int row = 0; row < months; row++ )
          {
            values[row][0] = iterator.next();
            for( int ax = 1; ax < axis.length; ax++ )
              values[row][ax] = new Double( 0 );
          }
          final ITuppleModel model = new SimpleTuppleModel( axis, values );
          setInput( new SimpleObservation( null, null, name, true, null, new MetadataList(), axis, model ) );

        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        // TODO Auto-generated method stub
        }
      };
      result.add( new ButtonControl( newListener, "neu", "Neue Vegetationsperiode anlegen", SWT.PUSH ) );
    }
    if( ( m_buttonControls & BUTTON_EXEL_IMPORT ) == BUTTON_EXEL_IMPORT )
    {
      final SelectionListener exelImportListener = new SelectionListener()
      {
        public void widgetSelected( SelectionEvent e )
        {
          final Clipboard clipboard = new Clipboard( getShell().getDisplay() );
          Object content = clipboard.getContents( TextTransfer.getInstance() );
          if( content != null && content instanceof String )
            setInput( ZmlFactory.createZMLFromClipboardString( ( (IObservation)m_input ).getName(), (String)content,
                axis ) );
          else
            ; // TODO messagebox
          clipboard.dispose();
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        // TODO Auto-generated method stub
        }
      };
      result.add( new ButtonControl( exelImportListener, "Import aus Zwischenspeicher",
          "Aus Zwischenspeicher importieren", SWT.PUSH ) );
    }
    if( ( m_buttonControls & BUTTON_EXEL_EXPORT ) == BUTTON_EXEL_EXPORT )
    {
      final SelectionListener exelExportListener = new SelectionListener()
      {
        public void widgetSelected( SelectionEvent e )
        {
          final Clipboard clipboard = new Clipboard( getShell().getDisplay() );
          final Object input = getInput();
          if( input != null && input instanceof IObservation )
          {
            try
            {
              final String content = ZmlFactory.createClipboardStringFrom( (IObservation)input, null );
              clipboard.setContents( new Object[]
              { content }, new Transfer[]
              { TextTransfer.getInstance() } );
            }
            catch( SensorException e1 )
            {
              // TODO messagebox ??
            }
          }
          clipboard.dispose();
        }

        public void widgetDefaultSelected( SelectionEvent e )
        {
        // TODO Auto-generated method stub
        }
      };
      result.add( new ButtonControl( exelExportListener, "Export in Zwischenspeicher",
          "In Zwischenspeicher exportieren", SWT.PUSH ) );
    }
    return (ButtonControl[])result.toArray( new ButtonControl[result.size()] );
  }

  /**
   * @return input
   */
  public Object getInput()
  {
    if( m_viewer != null )
      return m_viewer.getInput();
    return m_input;
  }

}
