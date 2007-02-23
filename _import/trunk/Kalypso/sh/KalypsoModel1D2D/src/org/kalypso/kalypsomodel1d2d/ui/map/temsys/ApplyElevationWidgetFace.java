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
import java.util.ArrayList;
import java.util.List;

import javax.swing.BoxLayout;

import org.eclipse.core.runtime.FileLocator;
import org.eclipse.jface.preference.ColorFieldEditor;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.preference.PreferenceConverter;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IBaseLabelProvider;
import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
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
    private ListViewer areaViewer;
    private Section elevationSelectStatus;
    private Section areaSelectSection;


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
      
      elevationSelectStatus = 
                  toolkit.createSection( 
                      scrolledForm.getBody(), 
                      Section.TREE_NODE | Section.CLIENT_INDENT | 
                        Section.TWISTIE | Section.DESCRIPTION | 
                        Section.TITLE_BAR);
      elevationSelectStatus.setText( "Select Elevation Model" );
      TableWrapData tableWrapData = 
            new TableWrapData(TableWrapData.LEFT,TableWrapData.TOP,1,1);
      tableWrapData.grabHorizontal=true;
      elevationSelectStatus.setLayoutData(tableWrapData );
      elevationSelectStatus.setExpanded( true );
      
      
      areaSelectSection = 
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
      areaSelectSection.setEnabled( false );
      
      createAreaSelectSection( areaSelectSection );
      createElevationModelSelectStatus( elevationSelectStatus );
        
      return rootPanel;
    }
    
    private final void createElevationModelSelectStatus(Section workStatusSection)
    {
      workStatusSection.setLayout(null );
      
      Composite clientComposite = 
              toolkit.createComposite( workStatusSection , SWT.FLAT);
      workStatusSection.setClient( clientComposite );
      clientComposite.setLayout( null );
     
      
      
//        terrainModelLabel = new Label(this, SWT.NONE);
//        terrainModelLabel.setBounds(new Rectangle(15, 15, 121, 13));
//        terrainModelLabel.setText("Select the Terrain Model");
//        terrainModelList = new List(this, SWT.NONE);
//        terrainModelList.setBounds(new Rectangle(62, 38, 119, 83));
//        autoFocusLabel = new Label(this, SWT.NONE);
//        autoFocusLabel.setBounds(new Rectangle(15, 135, 106, 13));
//        autoFocusLabel.setText("Action : Auto Focus");
//        focusButton = new Button(this, SWT.CHECK);
//        focusButton.setBounds(new Rectangle(135, 135, 13, 16));
//        gotoTerrainModel = new Button(this, SWT.NONE);
//        gotoTerrainModel.setBounds(new Rectangle(81, 163, 115, 23));
//        gotoTerrainModel.setText("Goto Terrain Model");
//        setSize(new Point(300, 200));
//        setLayout(null);
      
      Label terrainModelLabel = new Label(clientComposite, SWT.NONE);
      terrainModelLabel.setBounds(new Rectangle(15, 15, 121, 13));
      terrainModelLabel.setText("Select the Terrain Model");
      
      Composite smallComposite = new Composite(clientComposite, SWT.None);
      smallComposite.setLayout( new RowLayout() );
      
      ListViewer elevationList = new ListViewer(smallComposite, SWT.FILL|SWT.BORDER);
      elevationList.setContentProvider( new ArrayContentProvider());
      elevationList.setInput(new String[]{"ele1", "ele2"}/*Person.example()*/);      
      
      elevationList.addSelectionChangedListener(
         new ISelectionChangedListener() {
         public void selectionChanged(
            SelectionChangedEvent event) {
            IStructuredSelection selection =
               (IStructuredSelection) event.getSelection();
            System.out.println("Selected: "
               + selection.getFirstElement());
            areaSelectSection.setEnabled( true );
            areaSelectSection.setExpanded( true );
         }
      });

      
      Label autoFocus = new Label(clientComposite,SWT.FLAT);
      autoFocus.setBounds(new Rectangle(15, 135, 106, 13));
      autoFocus.setText( "Action - Auto Focus" );
      
      Button focusButton = new Button(clientComposite,SWT.CHECK);
      focusButton.setBounds(new Rectangle(135, 135, 13, 16));
      focusButton.addSelectionListener( new SelectionAdapter() {
        public void widgetSelected(SelectionEvent event) {

        
        }
        
      });

      Button showTerrain = new Button(clientComposite,SWT.PUSH);
      
      showTerrain.setText( "Goto Terrain" );
      showTerrain.setBounds(new Rectangle(81, 163, 115, 23));
      showTerrain.addSelectionListener( new SelectionAdapter() {
        public void widgetSelected(SelectionEvent event) {
          areaSelectSection.setEnabled( true );
          areaSelectSection.setExpanded( true );

       }

      });
      clientComposite.setSize( 300, 200 );
      
 /*
  *      
      ListViewer elevationList = new ListViewer(clientComposite, SWT.FILL|SWT.BORDER);
//      elevationList.setLabelProvider(elevationLabelProvider());
      elevationList.setContentProvider( new ArrayContentProvider());
      elevationList.setInput(Person.example());      
      
      elevationList.addSelectionChangedListener(
         new ISelectionChangedListener() {
         public void selectionChanged(
            SelectionChangedEvent event) {
            IStructuredSelection selection =
               (IStructuredSelection) event.getSelection();
            System.out.println("Selected: "
               + selection.getFirstElement());
            areaSelectSection.setEnabled( true );
            areaSelectSection.setExpanded( true );
         }
      });

      
      Label autoFocus = new Label(clientComposite,SWT.FLAT);
      autoFocus.setText( "Action - Auto Focus" );
      
      Button focusButton = new Button(clientComposite,SWT.CHECK);
      focusButton.addSelectionListener( new SelectionAdapter() {
        public void widgetSelected(SelectionEvent event) {

        
        }
        
      });

      Button showTerrain = new Button(clientComposite,SWT.PUSH);
      showTerrain.setText( "Goto Terrain" );
      showTerrain.addSelectionListener( new SelectionAdapter() {
        public void widgetSelected(SelectionEvent event) {
          areaSelectSection.setEnabled( true );
          areaSelectSection.setExpanded( true );

       }

      });
  */
      
    }
    
      
   

    private IBaseLabelProvider elevationLabelProvider( )
    {
      // TODO Auto-generated method stub
      return null;
    }

    private IBaseLabelProvider elevationListLabelProvider( )
    {
      // TODO Auto-generated method stub
      return null;
    }

    private IContentProvider getContentProvider( )
    {
      // TODO Auto-generated method stub
      return null;
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
      Object oldInput = areaViewer.getInput();
      if(oldInput==input)
      {
        areaViewer.refresh();
      }
      else
      {
        areaViewer.setInput( input );
               
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
    }
    
   
    private void createAreaSelectSection(Section configSection)
    {
      configSection.setLayout( new FillLayout() );
      
      Composite clientComposite = 
              toolkit.createComposite( configSection , SWT.FLAT);
      configSection.setClient( clientComposite );
      clientComposite.setLayout( new GridLayout() );
    
     Label infoLabel = new Label (clientComposite,SWT.FLAT);
     infoLabel.setText("Selected Terrain Model");
     Text inputText = new Text (clientComposite,SWT.FLAT);
     inputText.setText( "One of the Field Name Comes Here" );
     
     Label areaSelectLabel = new Label (clientComposite,SWT.FLAT);
     areaSelectLabel.setText("Select Area");
     
     
     Table table = toolkit.createTable( clientComposite, SWT.FILL|SWT.BORDER );
     
     TableColumn lineColumn= new TableColumn(table,SWT.LEFT);
     lineColumn.setText( "Node" ); 
     lineColumn.setWidth( 100/1 );  
     TableColumn actualPointNum= new TableColumn(table,SWT.LEFT);
     actualPointNum.setText( "Elevation" ); 
     actualPointNum.setWidth( 100/2 );
     table.setHeaderVisible( true );
     table.setLinesVisible( true );
     ListViewer areaViewer = new ListViewer(table);         
     areaViewer.setContentProvider( 
                   getTableContentProvider());
    // areaViewer.setLabelProvider( getTableLabelProvider() );
     
     Button applyAll = new Button(clientComposite,SWT.PUSH);
     applyAll.setText( "Apply All" );
     
     Button applySelected = new Button(clientComposite,SWT.PUSH);
     applySelected.setText( "Apply Selected" );
     
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
  }