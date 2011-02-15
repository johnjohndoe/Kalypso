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
import java.util.ArrayList;
import java.util.List;

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
import org.kalypso.commons.eclipse.core.runtime.PluginImageProvider;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;

class GridWidgetFace
{

  /**
   * @author Patrice Congo
   */
  protected final class GridWorkStatusCnCProvider implements ITableLabelProvider, IColorProvider
  {
    private final List<Color> m_colorList = new ArrayList<Color>();

    @Override
    public Image getColumnImage( Object element, int columnIndex )
    {
      return null;
    }

    @Override
    public String getColumnText( Object element, int columnIndex )
    {
      if( element instanceof LinePointCollectorConfig )
        return getColumnText( (LinePointCollectorConfig) element, columnIndex );
      else
        return "" + element; //$NON-NLS-1$
    }

    private String getColumnText( LinePointCollectorConfig elementConfig, int columnIndex )
    {
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
            return "0"; //$NON-NLS-1$
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
            return "0"; //$NON-NLS-1$
        }
        default:
        {
          return "" + elementConfig + "_" + columnIndex; //$NON-NLS-1$ //$NON-NLS-2$
        }
      }

    }

    @Override
    public void addListener( ILabelProviderListener listener )
    {

    }

    @Override
    public void dispose( )
    {
      if( m_colorList == null )
        return;

      for( Color color : m_colorList )
        color.dispose();

    }

    @Override
    public boolean isLabelProperty( Object element, String property )
    {
      return false;
    }

    @Override
    public void removeListener( ILabelProviderListener listener )
    {

    }

    /**
     * @see org.eclipse.jface.viewers.IColorProvider#getBackground(java.lang.Object)
     */
    @Override
    public Color getBackground( Object element )
    {
      return null;
    }

    /**
     * @see org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
     */
    @Override
    @SuppressWarnings("synthetic-access")
    public Color getForeground( Object element )
    {
      if( element instanceof LinePointCollectorConfig )
      {
        java.awt.Color awtColor = ((LinePointCollectorConfig) element).getColor();
        awtColor = awtColor.darker();
        Color swtColor = null;
        String strKey = ((LinePointCollectorConfig) element).getName();
        int red = awtColor.getRed();
        int green = awtColor.getGreen();
        int blue = awtColor.getBlue();
        try{
          swtColor = m_toolkit.getColors().createColor( strKey, red, green, blue );
        }
        catch (Exception e) {
        }
        if( swtColor == null || swtColor.isDisposed() ){
          swtColor = new Color( null, red, green, blue );
          if( swtColor == null || swtColor.isDisposed() )
            swtColor = m_toolkit.getColors().getInactiveBackground();
        }
        m_colorList.add( swtColor );

        return swtColor;
      }
      else
      {
        return null;
      }
    }
  }

  // private CreateGridWidget m_widget;
  private final IGridPointCollectorStateListener tableUpdater = new IGridPointCollectorStateListener()
  {

    @Override
    public void stateChanged( GridPointColectorChangeEvent changeEvent )
    {
      Display display = m_rootPanel.getDisplay();
      display.syncExec( new Runnable()
      {
        @Override
        @SuppressWarnings("synthetic-access")
        public void run( )
        {
          m_tableViewer.setInput( m_gridPointCollector );
          m_tableViewer.setItemCount( 4 );
          LinePointCollectorConfig currentLPCConfig = m_gridPointCollector.getCurrentLPCConfig();
          if( currentLPCConfig != null )
          {
            m_tableViewer.setSelection( new StructuredSelection( currentLPCConfig ) );
          }
        }
      } );
    }

  };

  protected Composite m_rootPanel;

  private FormToolkit m_toolkit;

  private TableViewer m_tableViewer;

  private GridPointCollector m_gridPointCollector;

  static private IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private final IPropertyChangeListener storePropertyChangeListener = createPropertyChangeLis();

  private final ColorFieldEditor lineColorFieldEditor[] = new ColorFieldEditor[4];

  CreateGridWidget m_widget;

  public GridWidgetFace( CreateGridWidget widget )
  {
    m_widget = widget;
  }

  public Control createControl( Composite parent, final FormToolkit toolkit, final CreateGridWidget widget )
  {
    m_toolkit = toolkit;
    m_widget = widget;
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );
    initStoreDefaults();

    parent.setLayout( new FillLayout() );
    m_rootPanel = new Composite( parent, SWT.FILL );
    m_rootPanel.setLayout( new FillLayout() );
    final ScrolledForm scrolledForm = toolkit.createScrolledForm( m_rootPanel );

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    Section workStatus = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    workStatus.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.5" ) ); //$NON-NLS-1$
    TableWrapData tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    workStatus.setLayoutData( tableWrapData );
    workStatus.setExpanded( true );

    Section configSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    configSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.6" ) ); //$NON-NLS-1$
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.align = TableWrapData.FILL_GRAB;
    configSection.setLayoutData( tableWrapData );
    configSection.setExpanded( false );

    // help
    final Section helpSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    helpSection.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.7" ) ); //$NON-NLS-1$
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

    final Button m_buttonConvertToModel = toolkit.createButton( compConversion, "", SWT.PUSH ); //$NON-NLS-1$
    m_buttonConvertToModel.setToolTipText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.channeledit.CreateMainChannelComposite.15" ) ); //$NON-NLS-1$

    final PluginImageProvider imageProvider = KalypsoModel1D2DPlugin.getImageProvider();
    final Image okImage = imageProvider.getImage( KalypsoModel1D2DUIImages.IMGKEY.OK );

    m_buttonConvertToModel.setImage( okImage );

    m_buttonConvertToModel.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        m_widget.convertToModell();
      }
    } );

    toolkit.createLabel( compConversion, Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.9" ), SWT.NULL ); //$NON-NLS-1$

    return m_rootPanel;
  }

  private final void createWorkStatus( Section workStatusSection )
  {

    workStatusSection.setLayout( new FillLayout() );

    Composite clientComposite = m_toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    clientComposite.setLayout( new GridLayout() );
    Table table = m_toolkit.createTable( clientComposite, SWT.FILL );

    GridData gridData = new GridData( SWT.BEGINNING, SWT.FILL, false, true );
    table.setLayoutData( gridData );

    TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.10" ) ); //$NON-NLS-1$
    // lineColumn.setWidth( 50 );
    lineColumn.pack();

    TableColumn actualPointNum = new TableColumn( table, SWT.LEFT );
    actualPointNum.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.11" ) ); //$NON-NLS-1$
    // actualPointNum.setWidth( 40 );
    actualPointNum.pack();

    TableColumn targetPointNum = new TableColumn( table, SWT.LEFT | SWT.WRAP );
    targetPointNum.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.12" ) ); //$NON-NLS-1$
    // targetPointNum.setWidth( 40 );
    targetPointNum.setResizable( false );
    targetPointNum.pack();

    table.setHeaderVisible( true );
    table.setLinesVisible( false );

    table.pack();

    m_tableViewer = new TableViewer( table );
    m_tableViewer.setContentProvider( getTableContentProvider() );
    m_tableViewer.setLabelProvider( getTableLabelProvider() );
  }

  public void disposeControl( )
  {
    preferenceStore.removePropertyChangeListener( storePropertyChangeListener );
    if( m_rootPanel == null )
    {
      System.out.println( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.grid.GridWidgetFace.13" ) ); //$NON-NLS-1$
      return;
    }
    if( !m_rootPanel.isDisposed() )
    {
      handleWidth.setPropertyChangeListener( null );
      handleWidth.store();
      m_rootPanel.dispose();
    }

    m_tableViewer.getControl().dispose();
  }

  public void setInput( Object input )
  {
    Object oldInput = m_tableViewer.getInput();
    if( oldInput == input )
    {
      m_tableViewer.refresh();
    }
    else
    {
      m_tableViewer.setInput( input );
      if( input instanceof GridPointCollector )
      {
        LinePointCollectorConfig currentLPCConfig = ((GridPointCollector) input).getCurrentLPCConfig();
        if( currentLPCConfig != null )
        {
          m_tableViewer.setSelection( new StructuredSelection( currentLPCConfig ) );
        }
        this.m_gridPointCollector = (GridPointCollector) input;
        this.m_gridPointCollector.addGridPointCollectorStateChangeListener( tableUpdater );
      }

    }
  }

  private IntegerFieldEditor handleWidth;

  public static final String HANDLE_WIDTH_NAME = "x.handleWidth"; //$NON-NLS-1$

  public static final String LINE_COLOR_0 = "LINE_COLOR_0"; //$NON-NLS-1$

  public static final String LINE_COLOR_1 = "LINE_COLOR_1"; //$NON-NLS-1$

  public static final String LINE_COLOR_2 = "LINE_COLOR_2"; //$NON-NLS-1$

  public static final String LINE_COLOR_3 = "LINE_COLOR_3"; //$NON-NLS-1$

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

    Composite clientComposite = m_toolkit.createComposite( configSection, SWT.FLAT );
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

    for( ColorFieldEditor colorFieldEditor : lineColorFieldEditor )
    {
      colorFieldEditor.setPreferenceStore( preferenceStore );
      colorFieldEditor.setPropertyChangeListener( storePropertyChangeListener );
      colorFieldEditor.getColorSelector().addListener( storePropertyChangeListener );
      colorFieldEditor.load();
    }
  }

  private void createHelp( Section helpSection )
  {
    helpSection.setLayout( new FillLayout() );

    Composite clientComposite = m_toolkit.createComposite( helpSection, SWT.FILL );
    helpSection.setClient( clientComposite );
    clientComposite.setLayout( new FillLayout() );
    helpSection.setSize( 350, 350 );
    Browser browser = new Browser( clientComposite, SWT.FILL );
    m_toolkit.adapt( browser );

    try
    {
      URL htmlURL = KalypsoModel1D2DPlugin.getDefault().getBundle().getEntry( "/help/grid_widget_small_help.html" ); //$NON-NLS-1$
      browser.setUrl( FileLocator.toFileURL( htmlURL ).toExternalForm() );
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

      @Override
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

      @Override
      public void dispose( )
      {

      }

      @Override
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

      @Override
      @SuppressWarnings("synthetic-access")
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
          m_gridPointCollector.setColor( 0, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( LINE_COLOR_1.equals( property ) )
        {
          m_gridPointCollector.setColor( 1, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( LINE_COLOR_2.equals( property ) )
        {
          m_gridPointCollector.setColor( 2, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( LINE_COLOR_3.equals( property ) )
        {
          m_gridPointCollector.setColor( 3, makeAWTColor( (RGB) event.getNewValue() ) );
        }
        else if( HANDLE_WIDTH_NAME.equals( property ) )
        {
          m_gridPointCollector.setPointRectSize( (Integer) event.getNewValue() );
        }
        else
        {
          System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
      }

    };
  }

  static private final java.awt.Color makeAWTColor( RGB rgb )
  {
    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
  }

}