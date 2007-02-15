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
package xp;

import org.eclipse.jface.viewers.IColorProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ILabelProviderListener;
import org.eclipse.jface.viewers.IStructuredContentProvider;
import org.eclipse.jface.viewers.ITableLabelProvider;
import org.eclipse.jface.viewers.StructuredSelection;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.jface.viewers.Viewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;

class GridWidgetFace
  {
/**
   * @author Patrice Congo
   *
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
      if(element instanceof LinePointCollectorConfig)
      {
        return getColumnText( (LinePointCollectorConfig)element, columnIndex ); 
      }
      else
      {
        return ""+element;
      }
    }

    private  String getColumnText( 
                    LinePointCollectorConfig elementConfig, 
                    int columnIndex )
    {
      System.out.println("Getting label:"+elementConfig);
      switch(columnIndex)
      {
        case 0:
        {
          return elementConfig.getName();
        }
        case 1:
        {
          LinePointCollector configLinePointCollector = 
                  elementConfig.getConfigLinePointCollector();
          if(configLinePointCollector!=null)
          {
            int curPointCnt =
              configLinePointCollector.getCurrentPointCnt();
            return String.valueOf( curPointCnt );
          }
          else
          {
            return "NaN";
          }
        }
        case 2:
        {
          LinePointCollector configLinePointCollector = 
                  elementConfig.getConfigLinePointCollector();
          if(configLinePointCollector!=null)
          {
            int pointCnt =
              configLinePointCollector.getPointCnt();
            return String.valueOf( pointCnt );
          }
          else
          {
            return "NaN";
          }
        }
        default:
        {
          return ""+elementConfig+"_"+columnIndex;
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
      if(element instanceof LinePointCollectorConfig)
      {
//        java.awt.Color awtColor=((LinePointCollectorConfig)element).getColor();
//        awtColor = awtColor.brighter();
//        return toolkit.getColors().createColor( 
//            ((LinePointCollectorConfig)element).getName(), 
//            awtColor.getRed(), 
//            awtColor.getGreen(), 
//            awtColor.getBlue() );
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
      if(element instanceof LinePointCollectorConfig)
      {
        java.awt.Color awtColor=((LinePointCollectorConfig)element).getColor();
        awtColor = awtColor.darker();
        
        Color swtColor=
          toolkit.getColors().createColor( 
            ((LinePointCollectorConfig)element).getName(), 
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

    //    private CreateGridWidget m_widget;
    private IGridPointCollectorStateListener tableUpdater=
      new IGridPointCollectorStateListener()
    {

      public void stateChanged( GridPointColectorChangeEvent changeEvent )
      {
        Display display=rootPanel.getDisplay();
        display.syncExec(
            new Runnable() {
              public void run()
              {
                tableViewer.setInput( gridPointCollector );
                tableViewer.setItemCount( 4 );
                tableViewer.setSelection( 
                    new StructuredSelection(
                        gridPointCollector.getCurrentLPCConfig()));
              }
            });
      }
      
    };
    private Composite rootPanel;
    private FormToolkit toolkit;
    private TableViewer tableViewer;
    private GridPointCollector gridPointCollector;
    
    
    public GridWidgetFace(CreateGridWidget widget)
    {
//      m_widget = widget;      
    }
    
    public Control createControl( Composite parent )
    {
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
      
      Section workStatus = 
                  toolkit.createSection( 
                      scrolledForm.getBody(), 
                      Section.TREE_NODE | Section.CLIENT_INDENT | 
                        Section.TWISTIE | Section.DESCRIPTION | 
                        Section.TITLE_BAR);
      workStatus.setText( "Aktuelle Bearbeitungsstatus" );
      TableWrapData tableWrapData = new TableWrapData(TableWrapData.LEFT,TableWrapData.TOP,1,1);
      tableWrapData.grabHorizontal=true;
      workStatus.setLayoutData(tableWrapData );
      workStatus.setExpanded( true );
      
      
      Section config = 
          toolkit.createSection( 
              scrolledForm.getBody(), 
              Section.TREE_NODE | Section.CLIENT_INDENT | 
                Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
      config.setText( "Konfiguration" );
      tableWrapData = new TableWrapData(TableWrapData.LEFT,TableWrapData.TOP,1,1);
      tableWrapData.grabHorizontal=true;
      config.setLayoutData(tableWrapData );
      config.setExpanded( false );
      
      
      createWorkStatus( workStatus );
      
      
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
      if(!rootPanel.isDisposed())
      {
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
        if(input instanceof GridPointCollector)
        {
          LinePointCollectorConfig currentLPCConfig = 
                ((GridPointCollector)input).getCurrentLPCConfig();
          if(currentLPCConfig!=null)
          {
            tableViewer.setSelection( 
                new StructuredSelection(currentLPCConfig) );
          }
          this.gridPointCollector=(GridPointCollector)input;
          this.gridPointCollector.addGridPointCollectorStateChangeListener( tableUpdater );
        }
        
      }
    }
    
    
    private IContentProvider getTableContentProvider()
    {
      return new IStructuredContentProvider()
      {

        public Object[] getElements( Object inputElement )
        {
          if(inputElement instanceof GridPointCollector)
          {
            return ((GridPointCollector)
                        inputElement).getSideconfigsAsArray();
          }
          else
          {
            return new Object[]{};
          }
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

  }