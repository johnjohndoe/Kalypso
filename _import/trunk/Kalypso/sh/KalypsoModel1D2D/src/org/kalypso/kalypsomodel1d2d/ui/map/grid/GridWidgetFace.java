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

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.i18n.Messages;

class GridWidgetFace
{

  /**
   * @author Patrice Congo
   */
  private final class GridWorkStatusCnCProvider implements ITableLabelProvider, IColorProvider
  {
    public Image getColumnImage( Object element, int columnIndex )
    {
      return null;
    }

    public String getColumnText( Object element, int columnIndex )
    {
      if( element instanceof LinePointCollectorConfig )
      {
        return getColumnText( (LinePointCollectorConfig) element, columnIndex );
      }
      else
      {
        return "" + element;
      }
    }

    private String getColumnText( LinePointCollectorConfig elementConfig, int columnIndex )
    {
      // System.out.println( "Getting label:" + elementConfig );
      switch( columnIndex )
      {
        case 0:
        {
          return elementConfig.getName();
        }
        case 1:
        {
          LinePointCollector configLinePointCollector = elementConfig.getConfigLinePointCollector();
          if( configLinePointCollector != null )
          {
            int curPointCnt = configLinePointCollector.getCurrentPointCnt();
            return String.valueOf( curPointCnt );
          }
          else
          {
            return "0";
          }
        }
        case 2:
        {
          LinePointCollector configLinePointCollector = elementConfig.getConfigLinePointCollector();
          if( configLinePointCollector != null )
          {
            int pointCnt = configLinePointCollector.getPointCnt();
            return String.valueOf( pointCnt );
          }
          else
          {
            return "0";
          }
        }
        default:
        {
          return "" + elementConfig + "_" + columnIndex;
        }
      }

    }

    public void addListener( ILabelProviderListener listener )
    {

    }

    public void dispose( )
    {

    }

    public boolean isLabelProperty( Object element, String property )
    {
      return false;
    }

    public void removeListener( ILabelProviderListener listener )
    {

    }

    /**
     * @see org.eclipse.jface.viewers.IColorProvider#getBackground(java.lang.Object)
     */
    public Color getBackground( Object element )
    {
      if( element instanceof LinePointCollectorConfig )
      {
        // java.awt.Color awtColor=((LinePointCollectorConfig)element).getColor();
        // awtColor = awtColor.brighter();
        // return toolkit.getColors().createColor(
        // ((LinePointCollectorConfig)element).getName(),
        // awtColor.getRed(),
        // awtColor.getGreen(),
        // awtColor.getBlue() );

        return null;
      }
      else
      {
        return null;
      }
    }

    /**
     * @see org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
     */
    public Color getForeground( Object element )
    {
      if( element instanceof LinePointCollectorConfig )
      {
        java.awt.Color awtColor = ((LinePointCollectorConfig) element).getColor();
        awtColor = awtColor.darker();

        Color swtColor = toolkit.getColors().createColor( ((
            LinePointCollectorConfig) element).getName(),
            awtColor.getRed(),
            awtColor.getGreen(),
            awtColor.getBlue() );
        return swtColor;
      }
      else
      {
        return null;
      }
    }
  }

  // private CreateGridWidget m_widget;
  private IGridPointCollectorStateListener tableUpdater = new IGridPointCollectorStateListener()
  {

    public void stateChanged( GridPointColectorChangeEvent changeEvent )
    {
      Display display = rootPanel.getDisplay();
      display.syncExec( new Runnable()
      {
        public void run( )
        {
          tableViewer.setInput( gridPointCollector );
          tableViewer.setItemCount( 4 );
          LinePointCollectorConfig currentLPCConfig = gridPointCollector.getCurrentLPCConfig();
          if( currentLPCConfig != null )
          {
            tableViewer.setSelection( new StructuredSelection( currentLPCConfig ) );
          }
        }
      } );
    }

  };

  private Composite rootPanel;

  private FormToolkit toolkit;

  private TableViewer tableViewer;

  private GridPointCollector gridPointCollector;

  static private IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private IPropertyChangeListener storePropertyChangeListener = createPropertyChangeLis();

  private ColorFieldEditor lineColorFieldEditor[] = new ColorFieldEditor[4];

  CreateGridWidget m_widget;

  public GridWidgetFace( CreateGridWidget widget )
  {
    m_widget = widget;
  }

  public Control createControl( Composite parent, final FormToolkit toolkit, final CreateGridWidget widget )
  {
    this.toolkit = toolkit;
    m_widget = widget;
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );
    initStoreDefaults();

    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    final ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // FormData fd;
    //
    // fd = new FormData();
    // //fd.width = 270;// TODO check how not to use width
    // fd.left = new FormAttachment( 0, 0 );
    // fd.bottom = new FormAttachment( 100, 0 );
    // fd.top = new FormAttachment( 0, 0 );

    Section workStatus = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    workStatus.setText( "aktueller Bearbeitungsstatus" );
    TableWrapData tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    workStatus.setLayoutData( tableWrapData );
    workStatus.setExpanded( true );

    Section configSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    configSection.setText( "Konfiguration" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.align = TableWrapData.FILL_GRAB;
    configSection.setLayoutData( tableWrapData );
    configSection.setExpanded( false );

    // help
    final Section helpSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    helpSection.setText( "Hilfe" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    helpSection.setLayoutData( tableWrapData );
    helpSection.setExpanded( false );
    helpSection.addExpansionListener( new ExpansionAdapter()
    {
      /**
       * @see org.eclipse.ui.forms.events.ExpansionAdapter#expansionStateChanged(org.eclipse.ui.forms.events.ExpansionEvent)
       */
      @Override
      public void expansionStateChanged( ExpansionEvent e )
      {
        scrolledForm.reflow( true );
      }
    } );

    createWorkStatus( workStatus );
    createConfigSection( configSection );
    createHelp( helpSection );

    /* conversion to model composite */
    final Composite compConversion = toolkit.createComposite( scrolledForm.getBody(), SWT.FILL );
    compConversion.setLayout( new GridLayout( 2, false ) );

    final Button m_buttonConvertToModel = toolkit.createButton( compConversion, "", SWT.PUSH );
    m_buttonConvertToModel.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.15" ) ); //$NON-NLS-1$
    final Image convImage = KalypsoModel1D2DUIImages.ID_OK.createImage();
    m_buttonConvertToModel.setImage( convImage );

    m_buttonConvertToModel.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_widget.convertToModell();
      }
    } );

    toolkit.createLabel( compConversion, "FE-Netz generieren", SWT.NULL );

    return rootPanel;
  }

  private final void createWorkStatus( Section workStatusSection )
  {

    workStatusSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    clientComposite.setLayout( new GridLayout() );
    Table table = toolkit.createTable( clientComposite, SWT.FILL );

    GridData gridData = new GridData( SWT.BEGINNING, SWT.FILL, false, true );
    table.setLayoutData( gridData );

    TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setText( "Liniennr." );
    // lineColumn.setWidth( 50 );
    lineColumn.pack();

    TableColumn actualPointNum = new TableColumn( table, SWT.LEFT );
    actualPointNum.setText( "Ist" );
    // actualPointNum.setWidth( 40 );
    actualPointNum.pack();

    TableColumn targetPointNum = new TableColumn( table, SWT.LEFT | SWT.WRAP );
    targetPointNum.setText( "Soll" );
    // targetPointNum.setWidth( 40 );
    targetPointNum.setResizable( false );
    targetPointNum.pack();

    table.setHeaderVisible( true );
    table.setLinesVisible( false );

    table.pack();

    tableViewer = new TableViewer( table );
    tableViewer.setContentProvider( getTableContentProvider() );
    tableViewer.setLabelProvider( getTableLabelProvider() );
  }

  public void disposeControl( )
  {
    preferenceStore.removePropertyChangeListener( storePropertyChangeListener );
    if( rootPanel == null )
    {
      System.out.println( "Disposing null root panel" );
      return;
    }
    if( !rootPanel.isDisposed() )
    {
      handleWidth.setPropertyChangeListener( null );
      handleWidth.store();
      rootPanel.dispose();
    }

  }

  public void setInput( Object input )
  {
    Object oldInput = tableViewer.getInput();
    if( oldInput == input )
    {
      tableViewer.refresh();
    }
    else
    {
      tableViewer.setInput( input );
      if( input instanceof GridPointCollector )
      {
        LinePointCollectorConfig currentLPCConfig = ((GridPointCollector) input).getCurrentLPCConfig();
        if( currentLPCConfig != null )
        {
          tableViewer.setSelection( new StructuredSelection( currentLPCConfig ) );
        }
        this.gridPointCollector = (GridPointCollector) input;
        this.gridPointCollector.addGridPointCollectorStateChangeListener( tableUpdater );
      }

    }
  }

  private IntegerFieldEditor handleWidth;

  public static final String HANDLE_WIDTH_NAME = "x.handleWidth";

  public static final String LINE_COLOR_0 = "LINE_COLOR_0";

  public static final String LINE_COLOR_1 = "LINE_COLOR_1";

  public static final String LINE_COLOR_2 = "LINE_COLOR_2";

  public static final String LINE_COLOR_3 = "LINE_COLOR_3";

  private void initStoreDefaults( )
  {

    if( !preferenceStore.contains( HANDLE_WIDTH_NAME ) )
    {
      preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
      preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
    }
    String[] keys = new String[] { LINE_COLOR_0, LINE_COLOR_1, LINE_COLOR_2, LINE_COLOR_3 };
    java.awt.Color colors[] = new java.awt.Color[] { java.awt.Color.BLUE, java.awt.Color.DARK_GRAY, java.awt.Color.RED, java.awt.Color.GREEN };

    for( int i = 0; i < keys.length; i++ )
    {
      if( !preferenceStore.contains( keys[i] ) )
      {
        RGB rgb = new RGB( colors[i].getRed(), colors[i].getGreen(), colors[i].getBlue() );
        PreferenceConverter.setDefault( preferenceStore, LINE_COLOR_0, rgb );
        PreferenceConverter.setValue( preferenceStore, LINE_COLOR_0, rgb );

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
    String[] keys = new String[] { LINE_COLOR_0, LINE_COLOR_1, LINE_COLOR_2, LINE_COLOR_3 };
    java.awt.Color colors[] = new java.awt.Color[] { java.awt.Color.BLUE, java.awt.Color.DARK_GRAY, java.awt.Color.RED, java.awt.Color.GREEN };

    for( int i = 0; i < keys.length; i++ )
    {
      if( !preferenceStore.contains( keys[i] ) )
      {
        RGB rgb = new RGB( colors[i].getRed(), colors[i].getGreen(), colors[i].getBlue() );
        PreferenceConverter.setDefault( preferenceStore, keys[i],// LINE_COLOR_0,
        rgb );
        PreferenceConverter.setValue( preferenceStore, keys[i],// LINE_COLOR_0,
        rgb );

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

  private void createConfigSection( Section configSection )
  {
    configSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( configSection, SWT.FLAT );
    configSection.setClient( clientComposite );
    clientComposite.setLayout( new GridLayout() );

    handleWidth = new IntegerFieldEditor( HANDLE_WIDTH_NAME, "Punktfangweite", clientComposite );
    handleWidth.setPreferenceStore( preferenceStore );
    handleWidth.load();
    handleWidth.setPropertyChangeListener( storePropertyChangeListener );

    lineColorFieldEditor[0] = new ColorFieldEditor( LINE_COLOR_0, "Farbe f¸r Linie 1", clientComposite );
    lineColorFieldEditor[1] = new ColorFieldEditor( LINE_COLOR_1, "Farbe f¸r Linie 2", clientComposite );
    lineColorFieldEditor[2] = new ColorFieldEditor( LINE_COLOR_2, "Farbe f¸r Linie 3", clientComposite );
    lineColorFieldEditor[3] = new ColorFieldEditor( LINE_COLOR_3, "Farbe f¸r Linie 4", clientComposite );

    for( ColorFieldEditor colorFieldEditor : lineColorFieldEditor )
    {
      colorFieldEditor.setPreferenceStore( preferenceStore );
      colorFieldEditor.setPropertyChangeListener( storePropertyChangeListener );
      colorFieldEditor.getColorSelector().addListener( storePropertyChangeListener );
      colorFieldEditor.load();
    }

    // toolkit.adapt( handleWidth.get, true, true );

    // Table table = toolkit.createTable( clientComposite, SWT.FILL );
    //      
    // // final int WIDTH=clientComposite.getClientArea().width;
    // GridData gridData = new GridData(GridData.FILL_BOTH);
    // gridData.grabExcessVerticalSpace = true;
    // gridData.grabExcessHorizontalSpace = true;
    // // gridData.widthHint=200;
    // // gridData.horizontalSpan = 1;
    // table.setLayoutData(gridData);
  }

  private void createHelp( Section helpSection )
  {
    helpSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( helpSection, SWT.FILL );
    helpSection.setClient( clientComposite );
    clientComposite.setLayout( new FillLayout() );
    helpSection.setSize( 350, 350 );
    Browser browser = new Browser( clientComposite, SWT.FILL );
    // browser.setSize( browser.computeSize( clientComposite.getSize().x, 300, false ) );
    toolkit.adapt( browser );

    try
    {
      URL htmlURL = KalypsoModel1D2DPlugin.getDefault().getBundle().getEntry( "/help/grid_widget_small_help.html" );
      // URL htmlURL =
      // GridWidgetFace.class.getResource( "grid_widget_small_help.html" );
      browser.setUrl( FileLocator.toFileURL( htmlURL ).toExternalForm() );

      // System.out.println( "URI=" + htmlURL.toURI().toASCIIString() );
    }
    catch( Exception e )
    {
      e.printStackTrace();
    }
  }

  private IContentProvider getTableContentProvider( )
  {
    return new IStructuredContentProvider()
    {

      public Object[] getElements( Object inputElement )
      {
        if( inputElement instanceof GridPointCollector )
        {
          return ((GridPointCollector) inputElement).getSideconfigsAsArray();
        }
        else
        {
          return new Object[] {};
        }
      }

      public void dispose( )
      {

      }

      public void inputChanged( Viewer viewer, Object oldInput, Object newInput )
      {

      }

    };
  }

  private ITableLabelProvider getTableLabelProvider( )
  {
    return new GridWorkStatusCnCProvider();
  }

  private IPropertyChangeListener createPropertyChangeLis( )
  {
    return new IPropertyChangeListener()
    {

      public void propertyChange( PropertyChangeEvent event )
      {
        Object source = event.getSource();
        String property = event.getProperty();

        if( source instanceof FieldEditor )
        {
          ((FieldEditor) source).store();
        }
        else if( source instanceof ColorSelector )
        {
          // ColorFieldEditor edi=null;
          //            
          // ((ColorSelector)source).
        }
        else if( LINE_COLOR_0.equals( property ) )
        {
          gridPointCollector.setColor( 0, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( LINE_COLOR_1.equals( property ) )
        {
          gridPointCollector.setColor( 1, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( LINE_COLOR_2.equals( property ) )
        {
          gridPointCollector.setColor( 2, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( LINE_COLOR_3.equals( property ) )
        {
          gridPointCollector.setColor( 3, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( HANDLE_WIDTH_NAME.equals( property ) )
        {
          gridPointCollector.setPointRectSize( (Integer) event.getNewValue() );
        }
        else
        {
          System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() );
        }
      }

    };
  }

  static private final java.awt.Color makeAWTColor( RGB rgb )
  {

    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
  }

}