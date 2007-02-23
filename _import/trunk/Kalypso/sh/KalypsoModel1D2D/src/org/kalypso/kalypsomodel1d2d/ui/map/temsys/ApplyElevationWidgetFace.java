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
package org.kalypso.kalypsomodel1d2d.ui.map.temsys;

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
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;

class ApplyElevationWidgetFace
  {
  
  
/**
   * @author Patrice Congo
   * @author Madanagopal
   */
  private final class GridWorkStatusCnCProvider 
                            implements ITableLabelProvider, IColorProvider
  {
    public Image getColumnImage( Object element, int columnIndex )
    {
      return null;
    }

    public String getColumnText( Object element, int columnIndex )
    {
//      if(element instanceof LinePointCollectorConfig)
//      {
//        return getColumnText( (LinePointCollectorConfig)element, columnIndex ); 
//      }
//      else
//      {
//        return ""+element;
//      }
      return null;
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


    static private IPreferenceStore preferenceStore = 
          KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();
    private IPropertyChangeListener storePropertyChangeListener=
                                                 createPropertyChangeLis();
    private ColorFieldEditor lineColorFieldEditor[] = new ColorFieldEditor[4];
    
    public ApplyElevationWidgetFace()
    {      
    }
    
    public Control createControl( Composite parent )
    {
      preferenceStore.addPropertyChangeListener( storePropertyChangeListener );
      initStoreDefaults();
      
      parent.setLayout( new FillLayout() );
      rootPanel=new Composite(parent, SWT.FILL);
      rootPanel.setLayout( new FillLayout() );
      toolkit= new FormToolkit(parent.getDisplay());
      ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );
      
      scrolledForm.getBody().setLayout(new TableWrapLayout() );
      
//      FormData fd;
//
//      fd = new FormData();
//      //fd.width = 270;// TODO check how not to use width
//      fd.left = new FormAttachment( 0, 0 );
//      fd.bottom = new FormAttachment( 100, 0 );
//      fd.top = new FormAttachment( 0, 0 );
      
      Section terrainSelectStatus = 
                  toolkit.createSection( 
                      scrolledForm.getBody(), 
                      Section.TREE_NODE | Section.CLIENT_INDENT | 
                        Section.TWISTIE | Section.DESCRIPTION | 
                        Section.TITLE_BAR);
      terrainSelectStatus.setText( "Select Elevation Model" );
      TableWrapData tableWrapData = 
            new TableWrapData(TableWrapData.LEFT,TableWrapData.TOP,1,1);
      tableWrapData.grabHorizontal=true;
      terrainSelectStatus.setLayoutData(tableWrapData );
      terrainSelectStatus.setExpanded( true );
      
      
      Section areaSelectSection = 
          toolkit.createSection( 
              scrolledForm.getBody(), 
              Section.TREE_NODE | Section.CLIENT_INDENT | 
                Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
      areaSelectSection.setText( "Select A Region" );
      tableWrapData = 
          new TableWrapData(TableWrapData.LEFT,TableWrapData.TOP,1,1);
      tableWrapData.grabHorizontal=true;
      tableWrapData.align=TableWrapData.FILL_GRAB;
      areaSelectSection.setLayoutData(tableWrapData );
      areaSelectSection.setExpanded( false );
//      
//      //help
//      Section helpSection = 
//        toolkit.createSection( 
//            scrolledForm.getBody(), 
//            Section.TREE_NODE | Section.CLIENT_INDENT | 
//              Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
//      helpSection.setText( "Hilfe" );
//      tableWrapData = new TableWrapData(TableWrapData.LEFT,TableWrapData.TOP,1,1);
//      tableWrapData.grabHorizontal=true;
//      helpSection.setLayoutData(tableWrapData );
//      helpSection.setExpanded( true );
//      
      
      createConfigSection( areaSelectSection );
      createWorkStatus( terrainSelectStatus );
   //   createHelp( helpSection );
      
      return rootPanel;
    }
    
    private final void createWorkStatus(Section workStatusSection)
    {
      
      workStatusSection.setLayout( new FillLayout() );
      
      Composite clientComposite = 
              toolkit.createComposite( workStatusSection , SWT.FLAT);
      workStatusSection.setClient( clientComposite );
      clientComposite.setLayout( new GridLayout() );
      Table table = toolkit.createTable( clientComposite, SWT.FILL );
      
//      final int WIDTH=clientComposite.getClientArea().width;
      GridData gridData = new GridData(GridData.FILL_BOTH);
      gridData.grabExcessVerticalSpace = true;
      gridData.grabExcessHorizontalSpace = true;
//      gridData.widthHint=200;
//      gridData.horizontalSpan = 1;
      table.setLayoutData(gridData);
        
      TableColumn lineColumn= new TableColumn(table,SWT.LEFT);
       lineColumn.setText( "Linie" ); 
       lineColumn.setWidth( 100/1 );
       
      TableColumn actualPointNum= new TableColumn(table,SWT.LEFT);
      actualPointNum.setText( "Akt. PunktAnzahl" ); 
    actualPointNum.setWidth( 100/2 );  
      
      TableColumn targetPointNum= new TableColumn(table,SWT.LEFT|SWT.WRAP);
      targetPointNum.setText( "Ziel Punktanzahl" );
      targetPointNum.setWidth( 100/2 );
      targetPointNum.setResizable( true );
      
      table.setHeaderVisible( true );
      table.setLinesVisible( true );
      
      
      tableViewer = new TableViewer(table);         
      tableViewer.setContentProvider( 
                    getTableContentProvider());
      tableViewer.setLabelProvider( getTableLabelProvider() );
      
      
      
    }
    
    public void disposeControl( )
    {
      preferenceStore.removePropertyChangeListener( storePropertyChangeListener );
      if(rootPanel==null)
      {
        System.out.println("Disposing null root panel");
        return;
      }
      if(!rootPanel.isDisposed())
      {
        handleWidth.setPropertyChangeListener( null );
        handleWidth.store();
        rootPanel.dispose();
        toolkit.dispose();
      }
      
    }
    
    public void setInput(Object input)
    {
      Object oldInput = tableViewer.getInput();
      if(oldInput==input)
      {
        tableViewer.refresh();
      }
      else
      {
        tableViewer.setInput( input );
               
      }
    }
    
    private IntegerFieldEditor handleWidth;
    public static final String HANDLE_WIDTH_NAME="x.handleWidth";
    public static final String LINE_COLOR_0="LINE_COLOR_0";
    public static final String LINE_COLOR_1="LINE_COLOR_1";
    public static final String LINE_COLOR_2="LINE_COLOR_2";
    public static final String LINE_COLOR_3="LINE_COLOR_3";
    
    private void initStoreDefaults()
    {
      
      if(!preferenceStore.contains( HANDLE_WIDTH_NAME ))
      {
        preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
        preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
      }
      String[] keys =
        new String[]{LINE_COLOR_0, LINE_COLOR_1,LINE_COLOR_2,LINE_COLOR_3};
      java.awt.Color colors[] =
          new java.awt.Color[]{
          java.awt.Color.BLUE, java.awt.Color.DARK_GRAY, 
          java.awt.Color.RED, java.awt.Color.GREEN};
      
      for(int i=0;i<keys.length;i++)
      {
        if(!preferenceStore.contains( keys[i]))
        {
          RGB rgb= new RGB(
                      colors[i].getRed(),
                      colors[i].getGreen(), 
                      colors[i].getBlue());
          PreferenceConverter.setDefault( 
                preferenceStore, 
                LINE_COLOR_0, 
                rgb );
          PreferenceConverter.setValue( 
              preferenceStore, 
              LINE_COLOR_0, 
              rgb );
          
        }
      }
    }
    
    public static java.awt.Color[] getLineColors()
    {
      if(!preferenceStore.contains( HANDLE_WIDTH_NAME ))
      {
        preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
        preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
      }
      String[] keys =
        new String[]{LINE_COLOR_0, LINE_COLOR_1,LINE_COLOR_2,LINE_COLOR_3};
      java.awt.Color colors[] =
          new java.awt.Color[]{
          java.awt.Color.BLUE, java.awt.Color.DARK_GRAY, 
          java.awt.Color.RED, java.awt.Color.GREEN};
      
      for(int i=0;i<keys.length;i++)
      {
        if(!preferenceStore.contains( keys[i]))
        {
          RGB rgb= new RGB(
                      colors[i].getRed(),
                      colors[i].getGreen(), 
                      colors[i].getBlue());
          PreferenceConverter.setDefault( 
                preferenceStore, 
                keys[i],//LINE_COLOR_0, 
                rgb );
          PreferenceConverter.setValue( 
              preferenceStore, 
              keys[i],//LINE_COLOR_0, 
              rgb );
          
        }
      }
      
      return new java.awt.Color[]{
         makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_0 ) ),
         makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_1 ) ),
         makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_2 ) ),
         makeAWTColor( PreferenceConverter.getColor( preferenceStore, LINE_COLOR_3 ) )
          };
    }
    
    public static final int getPointRectSize()
    {
      if(!preferenceStore.contains( HANDLE_WIDTH_NAME ))
      {
        preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
        preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
      }
      return preferenceStore.getInt( HANDLE_WIDTH_NAME );
    }
    
    private void createConfigSection(Section configSection)
    {
      configSection.setLayout( new FillLayout() );
      
      Composite clientComposite = 
              toolkit.createComposite( configSection , SWT.FLAT);
      configSection.setClient( clientComposite );
      clientComposite.setLayout( new GridLayout() );
      
     handleWidth=
        new IntegerFieldEditor(HANDLE_WIDTH_NAME,"Handle Breite", clientComposite);
     handleWidth.setPreferenceStore( preferenceStore ); 
     handleWidth.load();
     handleWidth.setPropertyChangeListener( storePropertyChangeListener );
      
     lineColorFieldEditor[0]=
       new ColorFieldEditor(LINE_COLOR_0,"Farbe Linie0",clientComposite);
     lineColorFieldEditor[1]=
       new ColorFieldEditor(LINE_COLOR_1,"Farbe Linie1",clientComposite);
     lineColorFieldEditor[2]=
       new ColorFieldEditor(LINE_COLOR_2,"Farbe Linie2",clientComposite);
     lineColorFieldEditor[3]=
       new ColorFieldEditor(LINE_COLOR_3,"Farbe Linie3",clientComposite);
     
     for(ColorFieldEditor colorFieldEditor:lineColorFieldEditor)
     {
       colorFieldEditor.setPreferenceStore( preferenceStore );
       colorFieldEditor.setPropertyChangeListener( storePropertyChangeListener );
       colorFieldEditor.getColorSelector().addListener( storePropertyChangeListener );
       colorFieldEditor.load();
     }
     
     
//      toolkit.adapt( handleWidth.get, true, true );
      
//      Table table = toolkit.createTable( clientComposite, SWT.FILL );
//      
////      final int WIDTH=clientComposite.getClientArea().width;
//      GridData gridData = new GridData(GridData.FILL_BOTH);
//      gridData.grabExcessVerticalSpace = true;
//      gridData.grabExcessHorizontalSpace = true;
////      gridData.widthHint=200;
////      gridData.horizontalSpan = 1;
//      table.setLayoutData(gridData);
    }
    

    
    private IContentProvider getTableContentProvider()
    {
      return new IStructuredContentProvider()
      {

        public Object[] getElements( Object inputElement )
        {
          return new Object[]{};
          
        }

        public void dispose( )
        {
          
        }

        public void inputChanged( 
                        Viewer viewer, 
                        Object oldInput, 
                        Object newInput )
        {
          
        }
        
      };
    }
    
    private ITableLabelProvider getTableLabelProvider()
    {
      return new GridWorkStatusCnCProvider();
    }
    
    private IPropertyChangeListener createPropertyChangeLis( )
    {
      return new IPropertyChangeListener ()
      {

        public void propertyChange( PropertyChangeEvent event )
        {
          Object source=event.getSource();
          String property = event.getProperty();
          
          if(source instanceof FieldEditor)
          {
            ((FieldEditor)source).store();
          }
          else if(source instanceof ColorSelector)
          {
          }
          else
          {
            System.out.println("Property changed="+event.getProperty()+
                " "+event.getNewValue()+" "+source.getClass());
          }
        }
        
      };
    }
    
    static private final java.awt.Color makeAWTColor(RGB rgb)
    {
      
      return new java.awt.Color(rgb.red, rgb.green, rgb.blue);
    }
    

  }