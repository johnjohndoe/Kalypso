/*----------------    FILE HEADER KALYPSO ------------------------------------------
 *
 *  This file is part of kalypso.
 *  Copyright (C) 2004 by:
 * 
 *  Technical University Hamburg-Harburg (TUHH)
 *  Institute of River and coastal engineering
 *  Denickestraße 22
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
import org.eclipse.swt.SWT;
import org.eclipse.swt.browser.Browser;
import org.eclipse.swt.events.SelectionAdapter;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.graphics.Color;
import org.eclipse.swt.graphics.Font;
import org.eclipse.swt.graphics.GC;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.RGB;
import org.eclipse.swt.graphics.Rectangle;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.FormAttachment;
import org.eclipse.swt.layout.FormData;
import org.eclipse.swt.layout.FormLayout;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Control;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Label;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.ui.forms.FormColors;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.eclipse.ui.forms.widgets.ScrolledForm;
import org.eclipse.ui.forms.widgets.Section;
import org.eclipse.ui.forms.widgets.TableWrapData;
import org.eclipse.ui.forms.widgets.TableWrapLayout;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DEdge;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck.VALIDITY_STATE;

/**
 * Face for widget creating junction contexts.
 * This Face mainly reflects the content of {@link JunctionContextWidgetDataModel}
 * 
 * @author Patrice Congo
 */
class JunctionContextWidgetFace implements KeyBasedDataModelChangeListener 
{

  public static final String EGDE_1D_NR_STR_PREFFIX = Messages.getString("JunctionContextWidgetFace.0"); //$NON-NLS-1$
  
  public static final String DISTANCE_JUNCTION_STR_PREFIX = Messages.getString("JunctionContextWidgetFace.1"); //$NON-NLS-1$
  
  /**
   * The root panel for any control shon in this face
   */
  private Composite rootPanel;

  /**
   * The form kit for creating this face
   */
  private FormToolkit toolkit;

  /**
   * The data model this face is showing
   */
  private JunctionContextWidgetDataModel dataModel;

  /**
   * The preference store this face recover its setting from
   */
  private static  final IPreferenceStore preferenceStore = KalypsoModel1D2DPlugin.getDefault().getPreferenceStore();

  /**
   * does listen to the store event and update certain face data
   */  
  private IPropertyChangeListener storePropertyChangeListener = createPropertyChangeLis();
  
  /**
   * Key for the critical distance of the coppling gap 
   */
  public static final String CRITICAL_DISTANCE_NAME = "x.critical_distance"; //$NON-NLS-1$
  
  /**
   * Key for storing {@link org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck.VALIDITY_STATE}
   * in separator as data
   */
  public static final String COLOR_DATA_KEY = "_color_data_key_"; //$NON-NLS-1$
  
  /**
   * Field editor used to edit the critical distance for the coppling gap
   */
  private IntegerFieldEditor criticalDistance;

  /**
   * Forms color that cache the color to style the control acordig the checks
   */
  private FormColors colors;

  private Color createColor;

  /**
   * The section showing the current work status
   */
  private Section workStatus;

  /**
   * label holding a text reflecting the number of selected 1d edges
   */
  private Label edgeNr1D;

  /**
   * label holding a text reflecting the number of selected 2d edges
   */
  private Label edgeNr2D;

  /**
   * label that hold the length of the koppling distance
   */
  private Label labelDistance1D2D;
  
  /**
   * line separator that is style according to the 1d selection check
   */
  private Label sepLabel1D;
  
  /**
   * line separator that is style according to the 2d selection check
   */
  private Label sepLabel2D;
  
  /**
   * child composite of work status section that holds the control
   * showing the status info
   */
  private Composite clientComposite;
  
  /**
   * Button that convers the selection into a junction element
   */
  private Button buttonConvertToModel;
  

  /**
   * Create a widget face showing the given data model
   */
  public JunctionContextWidgetFace( 
            final JunctionContextWidgetDataModel dataModel )
  {
    this.dataModel = dataModel;
    dataModel.addKeyBasedDataChangeListener( this );
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener#dataChanged(java.lang.String, java.lang.Object)
   */
  public void dataChanged( String key, Object newValue )
  {
    updateFace();
  }
  
  /**
   * create the face controls
   */
  public Control createControl( 
                    final Composite parent, 
                    final FormToolkit toolkit)
  {
    this.toolkit = toolkit;
    
    preferenceStore.addPropertyChangeListener( storePropertyChangeListener );
    initStoreDefaults();

    parent.setLayout( new FillLayout() );
    rootPanel = new Composite( parent, SWT.FILL );
    rootPanel.setLayout( new FillLayout() );
    final ScrolledForm scrolledForm = toolkit.createScrolledForm( rootPanel );

    scrolledForm.getBody().setLayout( new TableWrapLayout() );

    this.workStatus = 
        toolkit.createSection( 
            scrolledForm.getBody(), 
            Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    workStatus.setText( Messages.getString("JunctionContextWidgetFace.4") ); //$NON-NLS-1$
    TableWrapData tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    workStatus.setLayoutData( tableWrapData );
    workStatus.setExpanded( true );

    Section configSection = 
        toolkit.createSection( 
              scrolledForm.getBody(), 
              Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
    configSection.setText( Messages.getString("JunctionContextWidgetFace.5") ); //$NON-NLS-1$
    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
    tableWrapData.grabHorizontal = true;
    tableWrapData.align = TableWrapData.FILL_GRAB;
    configSection.setLayoutData( tableWrapData );
    configSection.setExpanded( false );

//    // help
//    final Section helpSection = toolkit.createSection( scrolledForm.getBody(), Section.TREE_NODE | Section.CLIENT_INDENT | Section.TWISTIE | Section.DESCRIPTION | Section.TITLE_BAR );
//    helpSection.setText( "Hilfe" );
//    tableWrapData = new TableWrapData( TableWrapData.LEFT, TableWrapData.TOP, 1, 1 );
//    tableWrapData.grabHorizontal = true;
//    tableWrapData.grabVertical = true;
//    helpSection.setLayoutData( tableWrapData );
//    helpSection.setExpanded( false );
//    helpSection.addExpansionListener( new ExpansionAdapter()
//    {
//      /**
//       * @see org.eclipse.ui.forms.events.ExpansionAdapter#expansionStateChanged(org.eclipse.ui.forms.events.ExpansionEvent)
//       */
//      @Override
//      public void expansionStateChanged( ExpansionEvent e )
//      {
//        scrolledForm.reflow( true );
//      }
//    } );

    createWorkStatus( workStatus );
    createConfigSection( configSection );
//    createHelp( helpSection );

    /* conversion to model composite */
    final Composite compConversion = toolkit.createComposite( scrolledForm.getBody(), SWT.FILL );
    compConversion.setLayout( new GridLayout( 2, false ) );

    this.buttonConvertToModel = 
      toolkit.createButton( compConversion, "", SWT.PUSH ); //$NON-NLS-1$
    buttonConvertToModel.setToolTipText( Messages.getString("JunctionContextWidgetFace.7") ); //$NON-NLS-1$
    
    final Image convImage = 
            KalypsoModel1D2DUIImages.ID_OK.createImage();
    buttonConvertToModel.setImage( convImage );
    
    buttonConvertToModel.addSelectionListener( new SelectionAdapter()
    {
      @Override
      public void widgetSelected( final SelectionEvent e )
      {
        IDataModelCommand command = dataModel.getCreateModelPart();
        if( command == null )
        {
          //todo Patrice well be creatif
          System.out.println(Messages.getString("JunctionContextWidgetFace.8")); //$NON-NLS-1$
        }
        else
        {
          command.execute();
          ///TODO Patrice reset data model
        }
      }
    } );

    toolkit.createLabel( 
        compConversion, 
        Messages.getString("JunctionContextWidgetFace.9"),  //$NON-NLS-1$
        SWT.NULL );
    dataModel.resetSelections();
    return rootPanel;
  }

  /**
   * call to update the face, making it showing the
   * curent state of the data model
   */
  private final void updateFace()
  {
    if(rootPanel == null )
    {
      System.out.println(Messages.getString("JunctionContextWidgetFace.10")); //$NON-NLS-1$
      return;
    }
    
    Display display = rootPanel.getDisplay();//getShell().getDisplay();
   
    if(display!=null)
    {
      Runnable updateRunnable = new Runnable()
      {
        /**
         * @see java.lang.Runnable#run()
         */
        public void run( )
        {
          {
            //update edgeNr1D label text
            Collection<IFE1D2DEdge> selected1D = dataModel.getSelected1D();
            final String size = 
              selected1D!=null? String.valueOf( selected1D.size() ) : "0"; //$NON-NLS-1$
            edgeNr1D.setText( EGDE_1D_NR_STR_PREFFIX + size );
          }
          
          {
//          update edgeNr2D label text
            Collection<IFE1D2DEdge> selected2D = dataModel.getSelected2D();
            final String size = selected2D!=null? String.valueOf( selected2D.size() ) : "0"; //$NON-NLS-1$
            edgeNr2D.setText( EGDE_1D_NR_STR_PREFFIX + size );
          }
          
          {
            //update labelDistace1d2d text
            Double distance1D2D = dataModel.getDistance1D2D();
            final String distStr = 
                distance1D2D != null? distance1D2D.toString() : Messages.getString("JunctionContextWidgetFace.13"); //$NON-NLS-1$
            labelDistance1D2D.setText( DISTANCE_JUNCTION_STR_PREFIX + distStr );
          }
          {
            //update separator color
            IDataModelCheck check1d = 
              dataModel.getDataCheck( JunctionContextWidgetDataModel.SELECTED_ELEMENT1D );
            VALIDITY_STATE validityState = check1d.getValidityState();
            sepLabel1D.setData( COLOR_DATA_KEY, validityState.name() );
            sepLabel1D.redraw();//update();
          }
          
          {
            //update separator color
            IDataModelCheck check2d = 
              dataModel.getDataCheck( JunctionContextWidgetDataModel.SELECTED_ELEMENT2D );
            VALIDITY_STATE validityState = check2d.getValidityState();
            sepLabel2D.setData( COLOR_DATA_KEY, validityState.name() );
            sepLabel2D.redraw();//pack();//update();
          }
          
          final IDataModelCheck modelCheck = dataModel.getModelCheck();
          final VALIDITY_STATE validityState = 
                      (modelCheck!=null)?modelCheck.getValidityState():null;
          
          {
            //update workstatus description and background
            String message = dataModel.getMessage();            
            workStatus.setDescription( 
                message!=null?message:""  //$NON-NLS-1$
                  );
            final Color modelCheckColor; 
            if(modelCheck!=null)
            {
              if( message == null )
              {
                modelCheckColor = colors.getBackground();
              }
              else
              {
                modelCheckColor = 
                    colors.getColor( validityState.name() );
              }
            }
            else
            {
              Color tempColor = colors.getColor( "_VALID_COLOR_" ); //$NON-NLS-1$
              if(tempColor == null )
              {
                tempColor =
                  colors.createColor( 
                      "_INVALID_COLOR_", new RGB(0,255,0) ); //$NON-NLS-1$
              }
              modelCheckColor = tempColor;
            }
            Control descriptionControl = workStatus.getDescriptionControl();
            descriptionControl.setBackground( modelCheckColor );
            descriptionControl.redraw();
//            workStatus.redraw();
          }
          
          //button
          {
            //enable convert button if data is valid 
            if( validityState != VALIDITY_STATE.INVALID )
            {
              buttonConvertToModel.setEnabled( true );
            }
            else
            {
              buttonConvertToModel.setEnabled( false );
            }
          }
          rootPanel.layout(true, true);
        }
      };
      display.asyncExec( updateRunnable );
    }
    else
    {
      System.out.println(Messages.getString("JunctionContextWidgetFace.17")); //$NON-NLS-1$
    }
    
    
  }
  /**
   * Call to create the workstatus section content controls
   * 
   */
  private final void createWorkStatus( Section workStatusSection )
  {
    final int COLUMN_WIDTH = 200; 
    this.colors = toolkit.getColors();
    RGB systemRedRGB = colors.getSystemColor( SWT.COLOR_RED );
    
    //init colors
    colors.createColor(
            IDataModelCheck.VALIDITY_STATE.VALID.name(),
            colors.getSystemColor( SWT.COLOR_DARK_GREEN));
    colors.createColor(
        IDataModelCheck.VALIDITY_STATE.INVALID.name(),
        colors.getSystemColor( SWT.COLOR_RED ));
    colors.createColor(
        IDataModelCheck.VALIDITY_STATE.ACCEPTABLE.name(),
        colors.getSystemColor( SWT.COLOR_RED ));
    
    this.createColor = colors.createColor( 
        "_INVALID_SELECTION", new RGB(255,0,0) ); //$NON-NLS-1$
    final Font sectionTextFont = workStatusSection.getFont();
//    workStatusSection.setLayout( new FillLayout() );
//    workStatusSection.setDescription( 
//        "TEXT ETEX TETSGSG \n TETTXF \netete"  );
//    workStatusSection.getDescriptionControl().setBackground( createColor );
    
    this.clientComposite = toolkit.createComposite( workStatusSection, SWT.FLAT );
    
    workStatusSection.setClient( clientComposite );
//    clientComposite.setLayout( new GridLayout() );
    FormLayout formLayout = new FormLayout();
    clientComposite.setLayout( formLayout );
    ///1D Kanten
    Label label1DKante = 
              toolkit.createLabel( 
                  clientComposite, 
                  Messages.getString("JunctionContextWidgetFace.19") ); //$NON-NLS-1$
    label1DKante.setFont( sectionTextFont );
    FormData formData = new FormData();
//    formData.top = new FormAttachment(0);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    label1DKante.setLayoutData( formData );
    
    //sep
    
    System.out.println(Messages.getString("JunctionContextWidgetFace.20")+createColor); //$NON-NLS-1$
    
    this.sepLabel1D = 
      createLabel( clientComposite );    
    formData = new FormData();
    formData.top = new FormAttachment(label1DKante);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    formData.height = 2;
    sepLabel1D.setLayoutData( formData );
    
    
    //List von kanten
    this.edgeNr1D = toolkit.createLabel( clientComposite, EGDE_1D_NR_STR_PREFFIX );

    formData = new FormData();
    formData.top = new FormAttachment(sepLabel1D,3);
    formData.left = new FormAttachment(0,3);
    formData.right = new FormAttachment(100,-5);
    formData.height = 20;
    edgeNr1D.setLayoutData( formData );
    
    //2D Kanten    
    Label label2DKante = 
            toolkit.createLabel( 
                clientComposite, 
                Messages.getString("JunctionContextWidgetFace.21") ); //$NON-NLS-1$
    label2DKante.setFont( sectionTextFont );
    formData = new FormData();
    formData.top = new FormAttachment(edgeNr1D,5);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    label2DKante.setLayoutData( formData );
    
    this.sepLabel2D = 
      createLabel( clientComposite);    
    formData = new FormData();
    formData.top = new FormAttachment(label2DKante);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    formData.height = 2;
    sepLabel2D.setLayoutData( formData );
    
    //list kanten 2D    
    this.edgeNr2D = toolkit.createLabel( clientComposite, Messages.getString("JunctionContextWidgetFace.22") ); //$NON-NLS-1$
    formData = new FormData();
    formData.top = new FormAttachment(sepLabel2D);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    edgeNr2D.setLayoutData( formData );

    //kopplung lücke
    Label labelGap = 
            toolkit.createLabel( 
                clientComposite, 
                Messages.getString("JunctionContextWidgetFace.23") ); //$NON-NLS-1$
    labelGap.setFont( sectionTextFont );
    formData = new FormData();
    formData.top = new FormAttachment(edgeNr2D,5);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    labelGap.setLayoutData( formData );
    
    Label sepGap = 
      createLabel( clientComposite );    
    formData = new FormData();
    formData.top = new FormAttachment(labelGap);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    formData.height = 2;
    sepGap.setLayoutData( formData );
    
    
    this.labelDistance1D2D = 
      toolkit.createLabel( clientComposite, DISTANCE_JUNCTION_STR_PREFIX );//createLabel( clientComposite, createColor );    
    formData = new FormData();
    formData.top = new FormAttachment(sepGap,3);
    formData.left = new FormAttachment(0);
    formData.right = new FormAttachment(100);
    labelDistance1D2D.setLayoutData( formData );
    
  }

  /**
   * Dispose this face
   */
  public void disposeControl( )
  {
    preferenceStore.removePropertyChangeListener( storePropertyChangeListener );
    if( rootPanel == null )
    {
      System.out.println( Messages.getString("JunctionContextWidgetFace.24") ); //$NON-NLS-1$
      return;
    }
    if( !rootPanel.isDisposed() )
    {
      criticalDistance.setPropertyChangeListener( null );
      criticalDistance.store();
      rootPanel.dispose();
    }

  }

  
  /**
   * Create an horizotal separator which styles itselv
   * by according to {@link org.kalypso.kalypsomodel1d2d.ui.map.facedata.IDataModelCheck.VALIDITY_STATE}
   * data get with {@link Label#getData(String)} with 
   * {@link #COLOR_DATA_KEY} as key
   * @param parent the parent composite of the separator to be created 
   */
  private Label createLabel( 
                        final Composite parent ) 
  {
    final Label label = new Label(parent,SWT.SEPARATOR|SWT.HORIZONTAL);
    label.addListener(SWT.Paint, new Listener() 
    {
        public void handleEvent(Event e) {
            if (label.isDisposed())
                return;
            String data = (String)label.getData( COLOR_DATA_KEY );
            final Color color;
            if( data == null )
            {
              color = createColor;
              System.out.println(Messages.getString("JunctionContextWidgetFace.25")); //$NON-NLS-1$
            }
            else
            {
              color = colors.getColor( data );
            }
            Rectangle bounds = label.getBounds();
            GC gc = e.gc;
            gc.setForeground(color);
            gc.fillGradientRectangle(
                      0, 0, bounds.width, bounds.height,
                      false);
        }
    });
    return label;
  }
  /**
   * Inits the preference store with the storable
   * state of this face
   */
  private void initStoreDefaults( )
  {

    if( !preferenceStore.contains( CRITICAL_DISTANCE_NAME ) )
    {
      preferenceStore.setDefault( CRITICAL_DISTANCE_NAME, 1 );
      preferenceStore.setValue( CRITICAL_DISTANCE_NAME, 1 );
    }
  }

//  public static java.awt.Color[] getLineColors( )
//  {
//    if( !preferenceStore.contains( CRITICAL_DISTANCE_NAME ) )
//    {
//      preferenceStore.setDefault( CRITICAL_DISTANCE_NAME, 1 );
//      preferenceStore.setValue( CRITICAL_DISTANCE_NAME, 1 );
//    }
//    
//    return new java.awt.Color[] { };
//  }

  /**
   * Gets the critical distance from the preference store
   */
  public static final int getCriticalDistance( )
  {
    if( !preferenceStore.contains( CRITICAL_DISTANCE_NAME ) )
    {
      preferenceStore.setDefault( CRITICAL_DISTANCE_NAME, 1 );
      preferenceStore.setValue( CRITICAL_DISTANCE_NAME, 1 );
    }
    return preferenceStore.getInt( CRITICAL_DISTANCE_NAME );
  }

  
  private void createConfigSection( Section configSection )
  {
    configSection.setLayout( new FillLayout() );

    Composite clientComposite = toolkit.createComposite( configSection, SWT.FLAT );
    configSection.setClient( clientComposite );
    clientComposite.setLayout( new GridLayout() );

    criticalDistance = new IntegerFieldEditor( CRITICAL_DISTANCE_NAME, Messages.getString("JunctionContextWidgetFace.26"), clientComposite ); //$NON-NLS-1$
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
      URL htmlURL = KalypsoModel1D2DPlugin.getDefault().getBundle().getEntry( "/help/grid_widget_small_help.html" ); //$NON-NLS-1$
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
          System.out.println( "Property changed=" + event.getProperty() + " " + event.getNewValue() + " " + source.getClass() ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
        }
      }

    };
  }

//  static private final java.awt.Color makeAWTColor( RGB rgb )
//  {
//
//    return new java.awt.Color( rgb.red, rgb.green, rgb.blue );
//  }

}