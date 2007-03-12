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

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;


import org.apache.tools.ant.util.optional.NoExitSecurityManager;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.preference.ColorSelector;
import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.preference.IntegerFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.jface.viewers.ArrayContentProvider;
import org.eclipse.jface.viewers.IContentProvider;
import org.eclipse.jface.viewers.ISelectionChangedListener;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.LabelProvider;
import org.eclipse.jface.viewers.ListViewer;
import org.eclipse.jface.viewers.SelectionChangedEvent;
import org.eclipse.jface.viewers.TableViewer;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.PaintEvent;
import org.eclipse.swt.events.PaintListener;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Canvas;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Group;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Spinner;
import org.eclipse.swt.widgets.Table;
import org.eclipse.swt.widgets.TableColumn;
import org.eclipse.swt.widgets.Text;
import org.eclipse.swt.widgets.Widget;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.eclipse.ui.ide.IDE;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.ui.map.temsys.viz.ElevationTheme;
import org.kalypso.kalypsosimulationmodel.core.IFeatureWrapperCollection;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModel;
import org.kalypso.kalypsosimulationmodel.core.terrainmodel.ITerrainElevationModelSystem;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.IMapModell;
import org.kalypso.ogc.gml.selection.EasyFeatureWrapper;
import org.kalypso.ogc.gml.selection.IFeatureSelection;
import org.kalypso.ogc.gml.selection.IFeatureSelectionListener;
import org.kalypsodeegree.model.feature.Feature;

// import org.eclipse.draw2d.FigureUtilities;
// import org.eclipse.draw2d.FigureUtilities.*;

/**
 * @author Patrice Congo
 * @author Madanagopal
 */
class ApplyElevationWidgetFace
{
  ListViewer elevationList;

  static int index = 0;

  public String nameSel = "";

  Text inputText;

  private Composite rootPanel;

  private FormToolkit toolkit;

  private ListViewer areaViewer;

  private Section elevationSelectStatus;

  private Section areaSelectSection;

  static private IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  private IPropertyChangeListener storePropertyChangeListener = createPropertyChangeLis();

  private ApplyElevationWidgetDataModel dataModel;

  private PainterElevationColorModel paintModel;
  private Section elevationColorSection;

  private PaintListener drawListener;

  private Canvas windowCanvas;

  private int selectedRects = 1;

  private GC gc;

  private Label minLabel;

  private RGB RGBChoice;

  private Color colorChoice;

  private Widget leftComposite;

  private Display disp;// = optionsColorGroup.getDisplay();

  private Button checkBtnOptionMinMax;

  private IFeatureSelectionListener featureSelectionListener=
      new IFeatureSelectionListener()
  {

    @SuppressWarnings("synthetic-access")
    public void selectionChanged( IFeatureSelection selection )
    {
      if(selection instanceof IStructuredSelection)
      {
        
        
        final List<IFE1D2DNode> nodeList= new ArrayList<IFE1D2DNode>();
        Feature selecFeature=null;
        IFE1D2DNode selecNode=null;
        for(Object selected:selection.toList())
        {
          if(selected instanceof Feature)
          {
            selecFeature=(Feature)selected;
          }
          else if(selected instanceof EasyFeatureWrapper)
          {
            selecFeature = ((EasyFeatureWrapper)selected).getFeature(); 
          }
          
          if(selecFeature!=null)
          {
            selecNode=(IFE1D2DNode) selecFeature.getAdapter( IFE1D2DNode.class );
            if(selecNode!=null)
            {
              nodeList.add( selecNode );
            }
          }
          
        }
//        IFE1D2DNode[] nodes = new IFE1D2DNode[]{};
        
        if(nodeElevationViewer.getControl().isDisposed())
        {
          return;
        }
        try
        {
          
          IWorkbench workbench = PlatformUI.getWorkbench();
          
          nodeElevationViewer.getControl().getDisplay().syncExec( 
                new Runnable()
                {

                  public void run( )
                  {
                    IContentProvider cp = nodeElevationViewer.getContentProvider();
                    if(cp instanceof ArrayContentProvider) 
                    {
                      //because it is complaining about content provider not set
                      nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
                    }
                    else
                    {
                      nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
                    }
                    nodeElevationViewer.setInput( nodeList.toArray( new IFE1D2DNode[]{}) );
                  }
                  
                });
          
          IWorkbenchWindow activeWorkbenchWindow = workbench.getActiveWorkbenchWindow();
          if(activeWorkbenchWindow==null)
          {
            System.out.println("Active workbench is null");
            return;
          }
          
        }
        catch (Throwable th) 
        {
          th.printStackTrace();
        }
        
        
      }
    }
    
  };

  
  
  public ApplyElevationWidgetFace( )
  {
  
  }

  public ApplyElevationWidgetFace( 
              ApplyElevationWidgetDataModel dataModel )
  {
    this.dataModel = dataModel;

  }

  public Control createControl( Composite parent )
  {
    this.dataModel.getMapPanel().getSelectionManager().addSelectionListener( featureSelectionListener );
    
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );
    initStoreDefaults();

    paintModel = new PainterElevationColorModel();
    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    toolkit = new FormToolkit( parent.getDisplay() );
    ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    TableWrapData tableWrapData;

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    // Creates Section for "Select Elevation Model"
    elevationSelectStatus = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    elevationSelectStatus.setText( "Select Elevation Model" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    elevationSelectStatus.setLayoutData( tableWrapData );
    elevationSelectStatus.setExpanded( true );

    // Creates Section for "Select A Region - among the List of Nodes drawn on the Viewer Pane"
    areaSelectSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    areaSelectSection.setText( "Select A Region" );
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.align = TableWrapData.FILL_GRAB;
    areaSelectSection.setLayoutData( tableWrapData );
    areaSelectSection.setExpanded( false );
    areaSelectSection.setEnabled( false );

    // Creates Section to Configure the Color for Different Elevations
    elevationColorSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    elevationColorSection.setText( "Select Colors for MAX Elevation and MIN Elevation " );
    // elevationColorSection.addPaintListener( drawListener );

    tableWrapData = new TableWrapData();// TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.grabVertical = true;
    tableWrapData.heightHint = 170;
    tableWrapData.align = TableWrapData.FILL_GRAB;
    elevationColorSection.setLayoutData( tableWrapData );

    elevationColorSection.setExpanded( false );
    elevationColorSection.setEnabled( true );

    createElevationColorSetup( elevationColorSection );
    createAreaSelectSection( areaSelectSection );
    createElevationModelSelectStatus( elevationSelectStatus );
    // createElevationColorSetup(elevationColorSection, drawListener);

    return rootPanel;
  }

  protected Canvas createCanvas( Composite parent, int style, PaintListener pl )
  {
    Canvas c = new Canvas( parent, style );
    if( pl != null )
    {
      c.addPaintListener( pl );
    }
    return c;
  }

  protected Canvas createCanvas( Composite parent, PaintListener pl )
  {
    return createCanvas( parent, SWT.NONE, pl );
  }

  private void createElevationColorSetup( Section elevationColorConfig )
  {
    elevationColorConfig.setLayout( new GridLayout() );

    final Composite clientComposite = toolkit.createComposite( elevationColorConfig, SWT.FLAT );
    elevationColorConfig.setClient( clientComposite );

    FormLayout elevationColorGrid = new FormLayout();
    clientComposite.setLayout( elevationColorGrid );

    FormData formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 0 );
    // formData.width=160;

    // formData.right = new FormAttachment(5,0);
    clientComposite.setLayoutData( formData );

    // Min Max Grouping
    final Group minMaxGroup = new Group( clientComposite, SWT.NULL );
    minMaxGroup.setLayoutData( formData );

    final Group optionsColorGroup = new Group( clientComposite, SWT.NULL );
    formData = new FormData();
    formData.left = new FormAttachment( minMaxGroup, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, 0 );
    formData.right = new FormAttachment( 100, 0 );
    optionsColorGroup.setLayoutData( formData );
    firstGroup( minMaxGroup );
    secondGroup( optionsColorGroup );


  }

  private void firstGroup( Group minMaxGroup )
  {

    int MIN_MAX_WIDTH = 20;
    FormLayout minMaxLayout = new FormLayout();
    minMaxGroup.setText( "Color Space" );
    minMaxGroup.setLayout( minMaxLayout );

    FormData formData;
    Label maxLabel = new Label( minMaxGroup, SWT.NONE );
    maxLabel.setText( "Max" );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    formData.top = new FormAttachment( 0, 5 );
    formData.right = new FormAttachment( MIN_MAX_WIDTH, 0 );
    maxLabel.setLayoutData( formData );
    disp = minMaxGroup.getDisplay();

    windowCanvas = createCanvas( minMaxGroup, new PaintListener()
    {
      public void paintControl( PaintEvent e )
      {
        gc = new GC( windowCanvas );
        paintElevationColorSelection( gc );
      }


    } );

    windowCanvas.setForeground( minMaxGroup.getDisplay().getSystemColor( SWT.COLOR_GRAY ) );

    formData = new FormData();
    formData.width = 22;// 22;
    formData.height = 100;// 106;
    // windowCanvasFormData.top = new FormAttachment(0,1,0);
    formData.top = new FormAttachment( 0, 5 );
    formData.bottom = new FormAttachment( 100, -5 );
    formData.left = new FormAttachment( maxLabel, 5 );
    windowCanvas.setLayoutData( formData );

    minLabel = new Label( minMaxGroup, SWT.BOTTOM );
    minLabel.setText( "Min" );
    formData = new FormData();
    formData.left = new FormAttachment( 0, 5 );
    // formData.top = new FormAttachment(maxLabel,100);
    formData.bottom = new FormAttachment( 100, -5 );
    formData.right = new FormAttachment( MIN_MAX_WIDTH, 0 );
    minLabel.setLayoutData( formData );
  }
  
  private void paintElevationColorSelection( GC graphicCanvas )
  {
    int coord = (((int) (Math.ceil( 100D / selectedRects ))));

    float hsb[] = colorChoice.getRGB().getHSB();
    float part = ((float) (0.917 - hsb[2])) / selectedRects;
    float brightnessCount = 0;
    
   if( !checkBtnOptionMinMax.getSelection() )
    {
     paintModel.setFirstColor(
         new Color(disp,
         (new RGB( (colorChoice.getRGB().getHSB()[0]),
                    colorChoice.getRGB().getHSB()[1],
                    colorChoice.getRGB().getHSB()[2] + brightnessCount )) ) );
      for( int i = 0; i < selectedRects; i++ )
      {
        if( colorChoice != null )
        {
          graphicCanvas.setBackground( new Color( disp, (new RGB( (colorChoice.getRGB().getHSB()[0]), colorChoice.getRGB().getHSB()[1], colorChoice.getRGB().getHSB()[2] + brightnessCount )) ) );

          graphicCanvas.setForeground( new Color( disp, (new RGB( (colorChoice.getRGB().getHSB()[0]), colorChoice.getRGB().getHSB()[1], colorChoice.getRGB().getHSB()[2] + brightnessCount )) ) );
          graphicCanvas.fillRectangle( 0, (coord) * i, 20, coord );
        }
        else
        {
          System.out.println( "Out of Range" );
        }
        brightnessCount = brightnessCount + part;
      }
      paintModel.setSecondColor(
          new Color(disp,
          (new RGB( (colorChoice.getRGB().getHSB()[0]),
                     colorChoice.getRGB().getHSB()[1],
                     colorChoice.getRGB().getHSB()[2] + brightnessCount-part )) ) );      
    }
    else
    {
      brightnessCount = 0;
      paintModel.setSecondColor(
          new Color(disp,
          (new RGB( (colorChoice.getRGB().getHSB()[0]),
                     colorChoice.getRGB().getHSB()[1],
                     colorChoice.getRGB().getHSB()[2] + brightnessCount )) ) );
      for( int i = selectedRects - 1; i >= 0; i-- )
      {
        if( colorChoice != null )
        {
          graphicCanvas.setBackground( new Color( disp, (new RGB( (colorChoice.getRGB().getHSB()[0]), colorChoice.getRGB().getHSB()[1], colorChoice.getRGB().getHSB()[2] + brightnessCount )) ) );

          graphicCanvas.setForeground( new Color( disp, (new RGB( (colorChoice.getRGB().getHSB()[0]), colorChoice.getRGB().getHSB()[1], colorChoice.getRGB().getHSB()[2] + brightnessCount )) ) );

          graphicCanvas.fillRectangle( 0, (coord) * i, 20, coord );
        }
        else
        {
          System.out.println( "Out Of Range" );
        }

        brightnessCount = brightnessCount + part;
      }
      paintModel.setFirstColor(
          new Color(disp,
          (new RGB( (colorChoice.getRGB().getHSB()[0]),
                     colorChoice.getRGB().getHSB()[1],
                     colorChoice.getRGB().getHSB()[2] + brightnessCount-part )) ) );
    }
  }

  private void secondGroup( Group optionsColorGroup )
  {
    FormData optionsColorFormData;
    FormLayout optionsColorGrpLayout = new FormLayout();
    optionsColorGroup.setText( "Further Options" );
    optionsColorGroup.setLayout( optionsColorGrpLayout );

    Label maximumColor = new Label( optionsColorGroup, SWT.FLAT );
    maximumColor.setText( "Choose Color" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( 0, 5 );
    // formData.right = new FormAttachment()
    maximumColor.setLayoutData( optionsColorFormData );

    final ColorSelector colorSelector = new ColorSelector( optionsColorGroup );
    Button maxColorBtn = colorSelector.getButton();// new Button(optionsColorGroup,SWT.None);
    maxColorBtn.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        RGBChoice = colorSelector.getColorValue();        
        colorChoice = new Color( disp, RGBChoice );
        paintModel.setBaseColor( colorChoice );
        //selectedRects = 1;
        windowCanvas.redraw();

      }
    } );
    maxColorBtn.setText( "SELECT" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( maximumColor, 5 );
    optionsColorFormData.top = new FormAttachment( 0, 5 );

    maxColorBtn.setLayoutData( optionsColorFormData );

    Label noElevationColorLabel = new Label( optionsColorGroup, SWT.NONE );
    noElevationColorLabel.setText( "No Elevation Color" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( maxColorBtn, 8 );
    noElevationColorLabel.setLayoutData( optionsColorFormData );

    final ColorSelector noColorSelector = new ColorSelector( optionsColorGroup );
    Button noElevationColorBtn = noColorSelector.getButton();
    noElevationColorBtn.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        RGBChoice = colorSelector.getColorValue();
        colorChoice = new Color( disp, RGBChoice );
        paintModel.setNoElevationColor( colorChoice );
      }
    } );

    optionsColorFormData = new FormData();
    noElevationColorBtn.setText( "SELECT" );
    optionsColorFormData.left = new FormAttachment( noElevationColorLabel, 5 );
    optionsColorFormData.top = new FormAttachment( maxColorBtn, 8 );
    noElevationColorBtn.setLayoutData( optionsColorFormData );

    Label colorNumberCells = new Label( optionsColorGroup, SWT.NONE );
    colorNumberCells.setText( "Discrete Color Number" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( noElevationColorBtn, 8 );
    colorNumberCells.setLayoutData( optionsColorFormData );

    final Spinner stepper = new Spinner( optionsColorGroup, SWT.BORDER );
    stepper.setMinimum( 0 );
    stepper.setIncrement( 10 );
    stepper.setMaximum( 50 );

    stepper.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {

        selectedRects = stepper.getSelection();
        if( selectedRects == 0 )
          selectedRects = 1;
        System.out.println( "Selected" + selectedRects );
        windowCanvas.redraw();
      }
    } );

    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( colorNumberCells, 5 );
    optionsColorFormData.top = new FormAttachment( noElevationColorBtn, 8 );
    stepper.setLayoutData( optionsColorFormData );

    Label optionMinMax = new Label( optionsColorGroup, SWT.NONE );
    optionMinMax.setText( "Go Darker from Max to Min Elevation" );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( 0, 5 );
    optionsColorFormData.top = new FormAttachment( stepper, 8 );
    optionMinMax.setLayoutData( optionsColorFormData );

    checkBtnOptionMinMax = new Button( optionsColorGroup, SWT.CHECK );
    optionsColorFormData = new FormData();
    optionsColorFormData.left = new FormAttachment( optionMinMax, 5 );
    optionsColorFormData.top = new FormAttachment( stepper, 8 );
    // optionsColorFormData.bottom = new FormAttachment( 100, -5 );
    checkBtnOptionMinMax.setLayoutData( optionsColorFormData );
    checkBtnOptionMinMax.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent e )
      {
        System.out.println( "Val :" + checkBtnOptionMinMax.getSelection() );
        windowCanvas.redraw();
      }
    } );
    // checkBtnOptionMinMax.getEnabled()= false;
  }

  private final void createElevationModelSelectStatus( Section workStatusSection )
  {
    workStatusSection.setLayout( new GridLayout() );

    Composite clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    workStatusSection.setClient( clientComposite );
    // clientComposite.setSize( 400, 300 );
    GridLayout elevationGrid = new GridLayout( 2, false );
    clientComposite.setLayout( elevationGrid );
    Label terrainModelLabel = new Label( clientComposite, SWT.NONE );
    GridData labelGridData = new GridData( GridData.FILL_BOTH );
    labelGridData.horizontalSpan = 2;
    terrainModelLabel.setText( "Select the Terrain Model" );
    terrainModelLabel.setLayoutData( labelGridData );

    elevationList = new ListViewer( clientComposite, SWT.FILL | SWT.BORDER );
    elevationList.setContentProvider( new ArrayContentProvider() );
    elevationList.setLabelProvider( new ElevationListLabelProvider() );
    ITerrainElevationModelSystem elevationModelSystem = dataModel.getElevationModelSystem();
    if( elevationModelSystem == null )
    {
      elevationList.setInput( new Object[] {} );
    }
    else
    {
      IFeatureWrapperCollection<ITerrainElevationModel> terrainElevationModels = elevationModelSystem.getTerrainElevationModels();
      if( terrainElevationModels == null )
      {
        elevationList.setInput( new Object[] {} );
      }
      else
      {
        elevationList.setInput( terrainElevationModels.toArray() );
      }
    }
    elevationList.addSelectionChangedListener( new ISelectionChangedListener()
    {
      public void selectionChanged( SelectionChangedEvent event )
      {
        IStructuredSelection selection = (IStructuredSelection) event.getSelection();

        // nameSel = (String) selection.getFirstElement();
        if( selection.getFirstElement() == null )
          throw new NullPointerException( "Null Value while selection.getFirstElement() :" + selection.getFirstElement() );
        else
        {
          if( selection.getFirstElement() instanceof ITerrainElevationModel )
          {
            ITerrainElevationModel firstElement = (ITerrainElevationModel) selection.getFirstElement();
            dataModel.setElevationModel( firstElement );
            inputText.setText( firstElement.getName() );
          }
        }
        areaSelectSection.setEnabled( true );
        areaSelectSection.setExpanded( true );
      }
    } );

    // Dummy Label to create a Empty Cell in GridLayout
    Label autoFocus1 = new Label( clientComposite, SWT.FLAT );
    Label autoFocus = new Label( clientComposite, SWT.FLAT );
    autoFocus.setText( "Action - Auto Focus" );
    Button focusButton = new Button( clientComposite, SWT.CHECK );
    focusButton.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        // TODO Send the Focus on the particular area of the Map
      }

    } );

    Button showTerrain = new Button( clientComposite, SWT.PUSH );
    showTerrain.setText( "Goto Terrain" );
    showTerrain.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        areaSelectSection.setEnabled( true );
        areaSelectSection.setExpanded( true );
//        IMapModell mapModell = dataModel.getMapModell();
//        System.out.println( "themes=" + Arrays.asList( mapModell.getAllThemes() ) );
//        ElevationTheme elevationTheme = dataModel.getElevationTheme();
//        elevationTheme.setTerrainElevationModel( elevationModel );
//        dataModel.getMapPanel().setBoundingBox( elevationTheme.getBoundingBox() );
        
        ITerrainElevationModel elevationModel = dataModel.getElevationModel();
        if(elevationModel!=null)
        {
          dataModel.getMapPanel().setBoundingBox(elevationModel.getBoundingBox());
        }
//        elevationTheme.fireModellEvent( null );
        // mapModell.fireModellEvent( null );

      }

    } );
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
      rootPanel.dispose();
      toolkit.dispose();
    }

    
    MapPanel mapPanel = dataModel.getMapPanel();
    if(mapPanel!=null)
    {
      mapPanel.getSelectionManager().addSelectionListener( featureSelectionListener );
    }
    
  }

  public void setInput( Object input )
  {
    Object oldInput = areaViewer.getInput();
    if( oldInput == input )
    {
      areaViewer.refresh();
    }
    else
    {
      areaViewer.setInput( input );

    }
  }

  class ElevationListLabelProvider extends LabelProvider
  {

    public Image getImage( Object element )
    {

      return null;
    }

    public String getText( Object element )
    {
      if( element instanceof ITerrainElevationModel )
      {
        String name = ((ITerrainElevationModel) element).getName();
        if( name != null )
        {
          return name;
        }
        else
        {
          return ((ITerrainElevationModel) element).getGmlID();
        }
      }
      else
      {
        throw new RuntimeException( "Only terrain elevation model are supported:" + "but got \n\tclass=" + (element == null ? null : element.getClass()) + "\n\t value=" + element );
      }
    }
  }

  private IntegerFieldEditor handleWidth;

  public static final String HANDLE_WIDTH_NAME = "x.handleWidth";

  public static final String LINE_COLOR_0 = "LINE_COLOR_0";

  public static final String LINE_COLOR_1 = "LINE_COLOR_1";

  public static final String LINE_COLOR_2 = "LINE_COLOR_2";

  public static final String LINE_COLOR_3 = "LINE_COLOR_3";

  private List selectionNodeList;

  private Table table;

  private TableViewer nodeElevationViewer;

  private void initStoreDefaults( )
  {

    if( !preferenceStore.contains( HANDLE_WIDTH_NAME ) )
    {
      preferenceStore.setDefault( HANDLE_WIDTH_NAME, 6 );
      preferenceStore.setValue( HANDLE_WIDTH_NAME, 6 );
    }
  }

  private void createAreaSelectSection( Section configSection )
  {
    configSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( configSection, SWT.FLAT );
    configSection.setClient( clientComposite );
    clientComposite.setLayout( new GridLayout( 4, false ) );

    Label infoLabel = new Label( clientComposite, SWT.FLAT );
    infoLabel.setText( "Selected Terrain Model" );
    GridData infoLabelGridData = new GridData( GridData.FILL_HORIZONTAL );
    infoLabelGridData.horizontalSpan = 2;
    infoLabelGridData.verticalSpan = 1;
    infoLabel.setLayoutData( infoLabelGridData );

    inputText = new Text( clientComposite, SWT.FLAT | SWT.BORDER );
    inputText.setEditable( false );
    inputText.setText( nameSel );
    GridData inputTextGridData = new GridData( GridData.BEGINNING );
    inputTextGridData.horizontalSpan = 2;
    inputTextGridData.verticalSpan = 1;
    inputText.setLayoutData( inputTextGridData );

    Label areaSelectLabel = new Label( clientComposite, SWT.FLAT );
    areaSelectLabel.setText( "Select Area" );
    GridData areaSelectGridData = new GridData( GridData.FILL_HORIZONTAL );
    areaSelectGridData.horizontalSpan = 4;
    areaSelectGridData.verticalSpan = 1;
    areaSelectLabel.setLayoutData( areaSelectGridData );

    // Dummy Label to Provide a Empty Cell in the GridLayout
    // Label areaSelectLabel1 = new Label( clientComposite, SWT.FLAT );

    nodeElevationViewer = new TableViewer( clientComposite, SWT.FULL_SELECTION | SWT.BORDER | SWT.MULTI );
    table = nodeElevationViewer.getTable();
    GridData tableGridData = new GridData( GridData.FILL_BOTH );
    tableGridData.horizontalSpan = 3;
    tableGridData.verticalSpan = 3;
    // nodeElevationViewer.set
    table.setLayoutData( tableGridData );
    TableColumn lineColumn = new TableColumn( table, SWT.LEFT );
    lineColumn.setText( "Node" );
    lineColumn.setWidth( 100 / 1 );
    TableColumn actualPointNum = new TableColumn( table, SWT.LEFT );
    actualPointNum.setText( "Elevation" );
    actualPointNum.setWidth( 100 / 2 );
    table.setHeaderVisible( true );
    table.setLinesVisible( true );

    nodeElevationViewer.setLabelProvider( new FENodeLabelProvider() );
    nodeElevationViewer.setContentProvider( new ArrayContentProvider() );
    List<IFE1D2DNode> selectedNode = dataModel.getSelectedNode();
    if( selectedNode == null )
    {
      nodeElevationViewer.setInput( new IFE1D2DNode[] {} );
    }
    else
    {
      nodeElevationViewer.setInput( selectedNode.toArray( new IFE1D2DNode[] {} ) );
    }

    nodeElevationViewer.addSelectionChangedListener( new ISelectionChangedListener()
    {

      public void selectionChanged( SelectionChangedEvent event )
      {
        IStructuredSelection selection = (IStructuredSelection) event.getSelection();
        selectionNodeList = new ArrayList();
        selectionNodeList = selection.toList();
        // System.out.println("Selected :"+ selList.size());
      }
    } );

    Button selectAll = new Button( clientComposite, SWT.PUSH );
    GridData applyAllGridData = new GridData( GridData.FILL_HORIZONTAL );
    applyAllGridData.horizontalSpan = 1;
    applyAllGridData.verticalSpan = 1;
    selectAll.setLayoutData( applyAllGridData );
    selectAll.setText( "Select All" );
    // applyAll.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    selectAll.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        table.selectAll();
      }

    } );

    Button deSelectAll = new Button( clientComposite, SWT.PUSH );
    GridData deSelectGridData = new GridData( GridData.FILL_HORIZONTAL );
    deSelectGridData.horizontalSpan = 1;
    deSelectGridData.verticalSpan = 1;
    deSelectAll.setLayoutData( deSelectGridData );
    deSelectAll.setText( "DeSelect All" );
    // deSelectAll.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    deSelectAll.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        table.deselectAll();
        selectionNodeList.clear();
        // nodeElevationViewer.setSelection( selection, reveal )
      }

    } );

    Button applySelected = new Button( clientComposite, SWT.PUSH );
    GridData applySelectedGridData = new GridData( GridData.FILL_HORIZONTAL );
    applySelectedGridData.horizontalSpan = 1;
    applySelectedGridData.verticalSpan = 1;
    applySelected.setLayoutData( applySelectedGridData );
    applySelected.setText( "Apply Selected" );
    applySelected.setLayoutData( new GridData( GridData.FILL_HORIZONTAL ) );
    applySelected.addSelectionListener( new SelectionAdapter()
    {
      public void widgetSelected( SelectionEvent event )
      {
        System.out.println( "List of Elements Selected " + selectionNodeList.size() );
      }

    } );

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
        else
        {
          System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() );
        }
      }

    };
  }
  
  //TODO patrice use scheduling rule to wait for map load
  //See OpenMapViewCommand
  private static final void waitOnMap(IMapModell mapModell, MapPanel mapPanel)
  {
//      final Job job = 
//        new Job("Wait on map start") 
//      {
//        @Override
//        protected IStatus run( final IProgressMonitor monitor )
//        {
//          final IKalypsoTheme activeTheme = mapModell.getActiveTheme();
//          if( activeTheme != null && m_featureType.equals( NO_LAYER ) )
//          {
//            mapModell.activateTheme( null );
//          }
//          else if( !m_featureType.equals( activeTheme != null ? activeTheme.getContext() : null ) )
//          {
//            final IKalypsoTheme[] allThemes = mapModell.getAllThemes();
//            for( final IKalypsoTheme theme : allThemes )
//            {
//              if( !theme.isLoaded() )
//              {
//                theme.addKalypsoThemeListener( OpenMapViewCommandHandler.this );
//              }
//              else
//              {
//                maybeActivateTheme( theme );
//              }
//            }
//          }
//          return Status.OK_STATUS;
//        }
//      };
//      job.setRule( mapPanel.getSchedulingRule().getActivateLayerSchedulingRule() );
//      job.setUser( true );
//      job.schedule();
   
  }
}
