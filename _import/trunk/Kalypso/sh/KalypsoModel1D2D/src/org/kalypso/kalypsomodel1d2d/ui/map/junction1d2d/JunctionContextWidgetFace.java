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
package org.kalypso.kalypsomodel1d2d.ui.map.junction1d2d;

import java.net.URL;
import java.util.Collection;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Device;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.List;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.forms.FormColors;
import org.eclipse.ui.forms.events.ExpansionAdapter;
import org.eclipse.ui.forms.events.ExpansionEvent;
import org.eclipse.ui.forms.widgets.ExpandableComposite;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;

class JunctionContextWidgetFace
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
       return "" + element;     
    }

    private String getColumnText( 
                        Collection elementConfig, int columnIndex )
    {
      // System.out.println( "Getting label:" + elementConfig );
      switch( columnIndex )
      {
        case 0:
        {
          return ""+elementConfig;
        }
        case 1:
        {
          return "c2";
          
        }
        case 2:
        {
          return "c3";
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
      return null;
    }

    /**
     * @see org.eclipse.jface.viewers.IColorProvider#getForeground(java.lang.Object)
     */
    public Color getForeground( Object element )
    {
      return null;
    }
  }



  private Composite rootPanel;

  private FormToolkit toolkit;

  private TableViewer tableViewer;

  private JunctionContextWidgetDataModel gridPointCollector;

  static private IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private IPropertyChangeListener storePropertyChangeListener = createPropertyChangeLis();
  
  public static final String CRITICAL_DISTANCE_NAME = "x.critical_distance";
  private IntegerFieldEditor criticalDistance;

  

  public JunctionContextWidgetFace(  )
  {

  }

  public Control createControl( 
                    final Composite parent, 
                    final FormToolkit toolkit)
  {
    this.toolkit = toolkit;
//    m_widget = widget;
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );
    initStoreDefaults();

    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    final ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    Section workStatus = 
        toolkit.createSection( 
            scrolledForm.getBody(), 
            Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    workStatus.setText( "aktueller Bearbeitungsstatus" );
    TableWrapData tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    workStatus.setLayoutData( tableWrapData );
    workStatus.setExpanded( true );

    Section configSection = 
        toolkit.createSection( 
              scrolledForm.getBody(), 
              Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
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

    final Button m_buttonConvertToModel = 
      toolkit.createButton( compConversion, "", SWT.PUSH );
    m_buttonConvertToModel.setToolTipText( "Build Junction element" );
    final Image convImage = 
            KalypsoModel1D2DUIImages.ID_OK.createImage();
    m_buttonConvertToModel.setImage( convImage );

    m_buttonConvertToModel.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
//        m_widget.convertToModell();
      }
    } );

    toolkit.createLabel( 
        compConversion, 
        "Junction generieren", 
        SWT.NULL );

    return rootPanel;
  }

//  private final void createWorkStatus( Section workStatusSection )
//  {
//
//    workStatusSection.setLayout( new FillLayout() );
//
//    Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
//    workStatusSection.setClient( clientComposite );
//    clientComposite.setLayout( new GridLayout() );
//    Table table = toolkit.createTable( clientComposite, SWT.FILL );
//
//    GridData gridData = new GridData( SWT.BEGINNING, SWT.FILL, false, true );
//    table.setLayoutData( gridData );
//
//    TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
//    lineColumn.setText( "Liniennr." );
//    // lineColumn.setWidth( 50 );
//    lineColumn.pack();
//
//    TableColumn actualPointNum = new TableColumn( table, SWT.LEFT );
//    actualPointNum.setText( "Ist" );
//    // actualPointNum.setWidth( 40 );
//    actualPointNum.pack();
//
//    TableColumn targetPointNum = new TableColumn( table, SWT.LEFT | SWT.WRAP );
//    targetPointNum.setText( "Soll" );
//    // targetPointNum.setWidth( 40 );
//    targetPointNum.setResizable( false );
//    targetPointNum.pack();
//
//    table.setHeaderVisible( true );
//    table.setLinesVisible( false );
//
//    table.pack();
//
//    tableViewer = new TableViewer( table );
//    tableViewer.setContentProvider( getTableContentProvider() );
//    tableViewer.setLabelProvider( getTableLabelProvider() );
//  }
  
  private final void createWorkStatus( Section workStatusSection )
  {

    workStatusSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
//    clientComposite.setLayout( new GridLayout() );
    FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );
    ///1D Kanten
    Label label1DKante = 
              toolkit.createLabel( 
                  clientComposite, 
                  "Auswahl 1D Kante" );
    
    FormData formData = new FormData();
//    formData.top = new FormAttachment(0);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    label1DKante.setLayoutData( formData );
    
//    //section 
//    ExpandableComposite section1D = 
//      toolkit.createExpandableComposite( clientComposite, SWT.FLAT );
    
    //sep
    FormColors colors = toolkit.getColors();
    RGB systemRedRGB = colors.getSystemColor( SWT.COLOR_RED );
    Color createColor = colors.createColor( 
        "_INVALID_SELECTION", new RGB(255,0,0) );
    System.out.println("Create Color"+createColor);
    
    Label sepLabel1 = 
      createLabel( clientComposite, createColor );
    
    
    formData = new FormData();
    formData.top = new FormAttachment(label1DKante);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    formData.height = 2;
    sepLabel1.setLayoutData( formData );
    
    //List von kanten
    List kanten1DList = new List(clientComposite,SWT.NONE);
    kanten1DList.setItems( new String[]{"1","2","3"} );
    formData = new FormData();
    formData.top = new FormAttachment(sepLabel1);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    kanten1DList.setLayoutData( formData );
    toolkit.adapt( kanten1DList, true, true );
    
    //2D Kanten    
    Label label2DKante = 
            toolkit.createLabel( 
                clientComposite, 
                "Auswahl 2D Kanten" );
    
    formData = new FormData();
    formData.top = new FormAttachment(kanten1DList,5);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    label2DKante.setLayoutData( formData );
    
    //list kanten 2D
    
    Table table = toolkit.createTable( clientComposite, SWT.NONE );
    formData = new FormData();
    formData.top = new FormAttachment(label2DKante);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    table.setLayoutData( formData );
    
//    GridData gridData = new GridData( SWT.BEGINNING, SWT.FILL, false, true );
//    table.setLayoutData( gridData );

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
      criticalDistance.setPropertyChangeListener( null );
      criticalDistance.store();
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
      
    }
  }

  
  Composite createCompositeSeparator(final Composite parent, final Color color) {
    
    final Composite composite = new Composite(parent, SWT.HORIZONTAL);
    
    composite.addListener(SWT.Paint, new Listener() 
    {
        public void handleEvent(Event e) {
            if (composite.isDisposed())
                return;
            Rectangle bounds = composite.getBounds();
            bounds = new Rectangle(bounds.x,bounds.y,(int)(bounds.width*0.8),bounds.height);
            GC gc = e.gc;
            gc.setForeground(color);
            gc.fillGradientRectangle(0, 0, bounds.width, bounds.height,
                    false);
        }
    });
    if (parent instanceof Section)
        ((Section) parent).setSeparatorControl(composite);
    return composite;
}
  
  private Label createLabel( 
                        final Composite parent, 
                        final Color color ) 
  {
    final Label label = new Label(parent,SWT.SEPARATOR|SWT.HORIZONTAL);
    label.addListener(SWT.Paint, new Listener() 
    {
        public void handleEvent(Event e) {
            if (label.isDisposed())
                return;
            Rectangle bounds = label.getBounds();
            GC gc = e.gc;
            gc.setForeground(color);
            gc.fillGradientRectangle(0, 0, bounds.width, bounds.height,
                    false);
        }
    });
    return label;
  }
  
  private void initStoreDefaults( )
  {

    if( !preferenceStore.contains( CRITICAL_DISTANCE_NAME ) )
    {
      preferenceStore.setDefault( CRITICAL_DISTANCE_NAME, 6 );
      preferenceStore.setValue( CRITICAL_DISTANCE_NAME, 6 );
    }
  }

  public static java.awt.Color[] getLineColors( )
  {
    if( !preferenceStore.contains( CRITICAL_DISTANCE_NAME ) )
    {
      preferenceStore.setDefault( CRITICAL_DISTANCE_NAME, 6 );
      preferenceStore.setValue( CRITICAL_DISTANCE_NAME, 6 );
    }
    
    return new java.awt.Color[] { };
  }

  public static final int getCriticalDistance( )
  {
    if( !preferenceStore.contains( CRITICAL_DISTANCE_NAME ) )
    {
      preferenceStore.setDefault( CRITICAL_DISTANCE_NAME, 6 );
      preferenceStore.setValue( CRITICAL_DISTANCE_NAME, 6 );
    }
    return preferenceStore.getInt( CRITICAL_DISTANCE_NAME );
  }

  private void createConfigSection( Section configSection )
  {
    configSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( configSection, SWT.FLAT );
    configSection.setClient( clientComposite );
    clientComposite.setLayout( new GridLayout() );

    criticalDistance = new IntegerFieldEditor( CRITICAL_DISTANCE_NAME, "Punktfangweite", clientComposite );
    criticalDistance.setPreferenceStore( preferenceStore );
    criticalDistance.load();
    criticalDistance.setPropertyChangeListener( storePropertyChangeListener );

       
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
        if( inputElement instanceof JunctionContextWidgetDataModel )
        {
          JunctionContextWidgetDataModel jcDataModel = ((JunctionContextWidgetDataModel) 
                              inputElement);
          return new Object[]{
                      jcDataModel.getSelected1D(),
                      jcDataModel.getSelected2D()};
          
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
          
        }
//        else if( LINE_COLOR_0.equals( property ) )
//        {
////          gridPointCollector.setColor( 0, makeAWTColor( (RGB) event.getNewValue() ) );
//        }
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