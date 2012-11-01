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
package org.kalypso.kalypsomodel1d2d.ui.map.grid;

import java.net.URL;

import org.apache.commons.lang3.StringUtils;
import org.eclipse.core.runtime.FileLocator;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.jface.layout.GridLayoutFactory;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.KalypsoModel1D2DStrings;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

/**
 * @author Patrice Congo
 */
class GridWidgetFace
{
  private static final String HANDLE_WIDTH_NAME = "x.handleWidth"; //$NON-NLS-1$

  private static final String LINE_COLOR_0 = "LINE_COLOR_0"; //$NON-NLS-1$

  private static final String LINE_COLOR_1 = "LINE_COLOR_1"; //$NON-NLS-1$

  private static final String LINE_COLOR_2 = "LINE_COLOR_2"; //$NON-NLS-1$

  private static final String LINE_COLOR_3 = "LINE_COLOR_3"; //$NON-NLS-1$

  private final Runnable updateTableOperation = new Runnable()
  {
    @Override
    public void run( )
    {
      updateTable();
    }
  };

  private final IGridPointCollectorStateListener m_tableUpdater = new IGridPointCollectorStateListener()
  {
    @Override
    public void stateChanged( final GridPointColectorChangeEvent changeEvent )
    {
      handleStateChanged();
    }
  };

  private final IPropertyChangeListener storePropertyChangeListener = new IPropertyChangeListener()
  {
    @Override
    public void propertyChange( final PropertyChangeEvent event )
    {
      handlePropertyChanged( event );
    }
  };

  private static final IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private final CreateGridWidget m_widget;

  private final GridPointCollector m_gridPointCollector;

  private IntegerFieldEditor handleWidth;

  private TableViewer m_tableViewer;

  private final ColorFieldEditor lineColorFieldEditor[] = new ColorFieldEditor[4];

  private ScrolledForm m_scrolledForm;

  private Button m_buttonConvertToModel;

  public GridWidgetFace( final CreateGridWidget widget, final GridPointCollector gridPointCollector )
  {
    m_widget = widget;
    m_gridPointCollector = gridPointCollector;

    m_gridPointCollector.addGridPointCollectorStateChangeListener( m_tableUpdater );
  }

  protected void handleStateChanged( )
  {
    if( m_scrolledForm == null || m_scrolledForm.isDisposed() )
      return;

    final Display display = getDisplay();
    display.syncExec( updateTableOperation );
  }

  public Control createControl( final Composite parent, final FormToolkit toolkit )
  {
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );

    initStoreDefaults();

    m_scrolledForm = toolkit.createScrolledForm( parent );

    final Composite body = m_scrolledForm.getBody();
    GridLayoutFactory.swtDefaults().applyTo( body );

    /* Status */
    final Section workSection = toolkit.createSection( body, Section.EXPANDED | Section.TITLE_BAR );
    workSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.5" ) ); //$NON-NLS-1$
    workSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    createWorkStatus( toolkit, workSection );

    /* conversion to model composite */
    final Composite compConversion = toolkit.createComposite( body, SWT.FILL );
    compConversion.setLayout( new GridLayout( 2, false ) );

    m_buttonConvertToModel = toolkit.createButton( compConversion, StringUtils.EMPTY, SWT.PUSH | SWT.FLAT );
    m_buttonConvertToModel.setLayoutData( new GridData( SWT.FILL, SWT.CENTER, true, false, 2, 1 ) );

    m_buttonConvertToModel.setText( KalypsoModel1D2DStrings.APPLY_BUTTON_LABEL );
    m_buttonConvertToModel.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.15" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    m_buttonConvertToModel.setImage( imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.OK ) );

    m_buttonConvertToModel.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        handleConvertButtonSelected();
      }
    } );
    // toolkit.createLabel( compConversion, buttonLabel, SWT.NULL );

    /* Config */
    final Section configSection = toolkit.createSection( body, Section.TITLE_BAR );
    configSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.6" ) ); //$NON-NLS-1$
    configSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, false ) );
    createConfigSection( toolkit, configSection );

    /* Help */
    final Section helpSection = toolkit.createSection( body, Section.TITLE_BAR );
    helpSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.7" ) ); //$NON-NLS-1$
    helpSection.setLayoutData( new GridData( SWT.FILL, SWT.FILL, true, true ) );
    createHelp( toolkit, helpSection );

    updateTable();

    m_scrolledForm.reflow( true );

    return m_scrolledForm;
  }

  protected void handleConvertButtonSelected( )
  {
    m_widget.convertToModell();
  }

  private final void createWorkStatus( final FormToolkit toolkit, final Section workStatusSection )
  {
    final Table table = toolkit.createTable( workStatusSection, SWT.FILL );
    workStatusSection.setClient( table );

    final TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.10" ) ); //$NON-NLS-1$
    lineColumn.pack();
    lineColumn.setResizable( false );

    final TableColumn actualPointNum = new TableColumn( table, SWT.LEFT );
    actualPointNum.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.11" ) ); //$NON-NLS-1$
    actualPointNum.pack();
    actualPointNum.setResizable( false );

    final TableColumn targetPointNum = new TableColumn( table, SWT.LEFT | SWT.WRAP );
    targetPointNum.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.12" ) ); //$NON-NLS-1$
    targetPointNum.pack();
    targetPointNum.setResizable( false );

    table.setHeaderVisible( true );
    table.setLinesVisible( false );

    m_tableViewer = new TableViewer( table );
    m_tableViewer.setContentProvider( getTableContentProvider() );
    m_tableViewer.setLabelProvider( new GridWorkStatusCnCProvider( this ) );
    m_tableViewer.setInput( m_gridPointCollector );

    table.pack();
  }

  protected void updateTable( )
  {
    m_tableViewer.refresh();

    final LinePointCollectorConfig currentLPCConfig = m_gridPointCollector.getCurrentLPCConfig();
    if( currentLPCConfig != null )
    {
      m_tableViewer.setSelection( new StructuredSelection( currentLPCConfig ) );
    }

    final IStatus valid = m_gridPointCollector.isValid();

    final boolean hasAllSides = m_gridPointCollector.getHasAllSides();
    final boolean canFinish = valid.isOK() && hasAllSides;
    m_buttonConvertToModel.setEnabled( canFinish );
  }

  public void disposeControl( )
  {
    preferenceStore.removePropertyChangeListener( storePropertyChangeListener );

    if( handleWidth != null )
    {
      handleWidth.setPropertyChangeListener( null );
      handleWidth.store();
    }
  }

  private void initStoreDefaults( )
  {
    if( !preferenceStore.contains( HANDLE_WIDTH_NAME ) )
    {
      preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
      preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
    }

    final String[] keys = new String[] { LINE_COLOR_0, LINE_COLOR_1, LINE_COLOR_2, LINE_COLOR_3 };
    final java.awt.Color colors[] = new java.awt.Color[] { java.awt.Color.BLUE, java.awt.Color.DARK_GRAY, java.awt.Color.RED, java.awt.Color.GREEN };

    for( int i = 0; i < keys.length; i++ )
    {
      if( !preferenceStore.contains( keys[i] ) )
      {
        final RGB rgb = new RGB( colors[i].getRed(), colors[i].getGreen(), colors[i].getBlue() );
        PreferenceConverter.setDefault( preferenceStore, keys[i], rgb );
        PreferenceConverter.setValue( preferenceStore, keys[i], rgb );
      }
    }
  }

  public static java.awt.Color[] getLineColors( )
  {
    if( !preferenceStore.contains( HANDLE_WIDTH_NAME ) )
    {
      preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
      preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
    }

    final String[] keys = new String[] { LINE_COLOR_0, LINE_COLOR_1, LINE_COLOR_2, LINE_COLOR_3 };
    final java.awt.Color colors[] = new java.awt.Color[] { java.awt.Color.BLUE, java.awt.Color.DARK_GRAY, java.awt.Color.RED, java.awt.Color.GREEN };

    for( int i = 0; i < keys.length; i++ )
    {
      if( !preferenceStore.contains( keys[i] ) )
      {
        final RGB rgb = new RGB( colors[i].getRed(), colors[i].getGreen(), colors[i].getBlue() );
        PreferenceConverter.setDefault( preferenceStore, keys[i], rgb );
        PreferenceConverter.setValue( preferenceStore, keys[i], rgb );
      }
    }

    return new java.awt.Color[] { makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_0 ) ), makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_1 ) ),
        makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_2 ) ), makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_3 ) ) };
  }

  public static final int getPointRectSize( )
  {
    if( !preferenceStore.contains( HANDLE_WIDTH_NAME ) )
    {
      preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
      preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
    }
    return preferenceStore.getInt( HANDLE_WIDTH_NAME );
  }

  private void createConfigSection( final FormToolkit toolkit, final Section configSection )
  {
    final Composite clientComposite = toolkit.createComposite( configSection, SWT.FLAT );
    configSection.setClient( clientComposite );
    clientComposite.setLayout( new GridLayout() );

    handleWidth = new IntegerFieldEditor( HANDLE_WIDTH_NAME, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.19" ), clientComposite ); //$NON-NLS-1$
    handleWidth.setPreferenceStore( preferenceStore );
    handleWidth.load();
    handleWidth.setPropertyChangeListener( storePropertyChangeListener );

    lineColorFieldEditor[0] = new ColorFieldEditor( LINE_COLOR_0, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.20" ), clientComposite ); //$NON-NLS-1$
    lineColorFieldEditor[1] = new ColorFieldEditor( LINE_COLOR_1, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.21" ), clientComposite ); //$NON-NLS-1$
    lineColorFieldEditor[2] = new ColorFieldEditor( LINE_COLOR_2, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.22" ), clientComposite ); //$NON-NLS-1$
    lineColorFieldEditor[3] = new ColorFieldEditor( LINE_COLOR_3, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.23" ), clientComposite ); //$NON-NLS-1$

    for( final ColorFieldEditor colorFieldEditor : lineColorFieldEditor )
    {
      colorFieldEditor.setPreferenceStore( preferenceStore );
      colorFieldEditor.setPropertyChangeListener( storePropertyChangeListener );
      colorFieldEditor.getColorSelector().addListener( storePropertyChangeListener );
      colorFieldEditor.load();
    }
  }

  private void createHelp( final FormToolkit toolkit, final Section helpSection )
  {
    final Browser browser = new Browser( helpSection, SWT.NONE );
    toolkit.adapt( browser );

    helpSection.setClient( browser );

    try
    {
      // FIXME: translate!
      final URL htmlURL = KalypsoModel1D2DPlugin.getDefault().getBundle().getEntry( "/help/grid_widget_small_help.html" ); //$NON-NLS-1$
      browser.setUrl( FileLocator.toFileURL( htmlURL ).toExternalForm() );
    }
    catch( final Exception e )
    {
      e.printStackTrace();
    }
  }

  private IContentProvider getTableContentProvider( )
  {
    return new IStructuredContentProvider()
    {
      @Override
      public Object[] getElements( final Object inputElement )
      {
        if( inputElement instanceof GridPointCollector )
        {
          return ((GridPointCollector)inputElement).getSideconfigsAsArray();
        }
        else
        {
          return new Object[] {};
        }
      }

      @Override
      public void dispose( )
      {

      }

      @Override
      public void inputChanged( final Viewer viewer, final Object oldInput, final Object newInput )
      {

      }
    };
  }

  protected void handlePropertyChanged( final PropertyChangeEvent event )
  {
    final Object source = event.getSource();
    final String property = event.getProperty();

    if( source instanceof FieldEditor )
    {
      ((FieldEditor)source).store();
    }
    else if( source instanceof ColorSelector )
    {
      // ColorFieldEditor edi=null;
      //
      // ((ColorSelector)source).
    }
    else if( LINE_COLOR_0.equals( property ) )
    {
      m_gridPointCollector.setColor( 0, makeAWTColor( (RGB)event.getNewValue() ) );
    }
    else if( LINE_COLOR_1.equals( property ) )
    {
      m_gridPointCollector.setColor( 1, makeAWTColor( (RGB)event.getNewValue() ) );
    }
    else if( LINE_COLOR_2.equals( property ) )
    {
      m_gridPointCollector.setColor( 2, makeAWTColor( (RGB)event.getNewValue() ) );
    }
    else if( LINE_COLOR_3.equals( property ) )
    {
      m_gridPointCollector.setColor( 3, makeAWTColor( (RGB)event.getNewValue() ) );
    }
    else if( HANDLE_WIDTH_NAME.equals( property ) )
    {
      m_gridPointCollector.setPointRectSize( (Integer)event.getNewValue() );
    }
    else
    {
      System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
    }
  }

  static private final java.awt.Color makeAWTColor( final RGB rgb )
  {
    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
  }

  Display getDisplay( )
  {
    if( m_scrolledForm == null )
      return null;

    return m_scrolledForm.getDisplay();
  }
}