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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.lang.reflect.Constructor;

import org.eclipse.jface.action.Action;
import org.eclipse.jface.action.ToolBarManager;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ModifyEvent;
import org.eclipse.swt.events.ModifyListener;
import org.eclipse.swt.events.SelectionEvent;
import org.eclipse.swt.events.SelectionListener;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.layout.FillLayout;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Combo;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.ToolBar;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.forms.widgets.FormToolkit;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DUIImages;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.ui.map.CreateFE2DElementWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.IWidgetWithStrategy;
import org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.wizards.CreateSubCalculationUnitCopyWizard;
import org.kalypso.kalypsomodel1d2d.ui.map.cline.RouteLineElementWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.element1d.CreateFEElement1DWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelChangeListener;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.ogc.gml.map.widgets.PanToWidget;
import org.kalypso.ogc.gml.map.widgets.ZoomInByRectWidget;
import org.kalypso.ogc.gml.widgets.IWidget;

/**
 * @author Madanagopal
 * 
 */
public class CalculationUnitAdministerComponent
{
  class ActivateStrategyWidgetAction<T extends IWidget> extends Action
  {

    private final IWidgetWithStrategy widgetWithStrategy;

    private final Class<T> strategyClass;

    private final ImageDescriptor imageDescriptor;

    private final String name;

    private final String tooltip;

    public ActivateStrategyWidgetAction( final IWidgetWithStrategy widgetWithStrategy, final Class<T> strategyClass, final String name, final String tooltip, final ImageDescriptor imageDescriptor )
    {
      this.widgetWithStrategy = widgetWithStrategy;
      this.strategyClass = strategyClass;
      this.name = name;
      this.tooltip = tooltip;
      this.imageDescriptor = imageDescriptor;
    }

    /**
     * @see org.eclipse.jface.action.Action#run()
     */
    @Override
    public void run( )
    {

      try
      {
        Constructor<T> constructor = strategyClass.getConstructor( new Class[] { String.class, String.class } );
        IWidget widget = constructor.newInstance( new Object[] { name, tooltip } );
        widgetWithStrategy.setStrategy( widget );
        System.out.println( "Strategy set" );
      }
      catch( Throwable e )
      {
        e.printStackTrace();
      }
    }

    /**
     * @see org.eclipse.jface.action.Action#getImageDescriptor()
     */
    @Override
    public ImageDescriptor getImageDescriptor( )
    {
      return imageDescriptor;
    }

    /**
     * @see org.eclipse.jface.action.Action#getText()
     */
    @Override
    public String getText( )
    {
      return name;
    }

    /**
     * @see org.eclipse.jface.action.Action#getToolTipText()
     */
    @Override
    public String getToolTipText( )
    {
      return tooltip;
    }

    /**
     * @see org.eclipse.jface.action.Action#getId()
     */
    @Override
    public String getId( )
    {
      return "ID_" + name;
    }

  }

  private FormToolkit toolkit;

  private Composite parent;

  private CalculationUnitDataModel dataModel;

  private Composite rootComposite;

  private Combo actionsCombo;

  private Combo elementsCombo;

  private Button goButton;

  private Image goImage;

  /** holds button that activate wiget strategies */
// private CoolBar coolbar;
  private ToolBarManager toolbarManager;

  private static final String ACTION_KEY_ADMINISTER = "Verwalten";

  private static final String ACTION_KEY_DRAW = "New Zeichnen";

  private static final String ELEMENTS_KEY_ELEMENTS = "Elemente";

  private static final String ELEMENTS_KEY_SUBUNITS = "Sub-Einheiten";

  private static final String ELEMENTS_KEY_BOUNDARY_UP = "Rand-Linien";

  private static final String ELEMENTS_KEY_BOUNDARY_CONDITIONS = "Rand-Bedingung";

  private KeyBasedDataModelChangeListener settingsKeyListener = new KeyBasedDataModelChangeListener()
  {

    public void dataChanged( final String key, final Object newValue )
    {

      Display display = parent.getDisplay();
      final Runnable runnable = new Runnable()
      {
        public void run( )
        {
          if( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER.equals( key ) )
          {
            if( newValue != null )
            {
              updateThisSection( newValue );
            }
          }
        }

      };
      display.syncExec( runnable );
    }

  };

  private void updateThisSection( Object newValue )
  {
    if( newValue instanceof ICalculationUnit2D )
    {
      actionsCombo.removeAll();
      elementsCombo.removeAll();
      actionsCombo.add( ACTION_KEY_DRAW );
      actionsCombo.add( ACTION_KEY_ADMINISTER );
      elementsCombo.add( ELEMENTS_KEY_ELEMENTS );
      elementsCombo.add( ELEMENTS_KEY_BOUNDARY_UP );
      elementsCombo.add( ELEMENTS_KEY_BOUNDARY_CONDITIONS );
    }
    else if( newValue instanceof ICalculationUnit1D )
    {
      actionsCombo.removeAll();
      elementsCombo.removeAll();
      actionsCombo.add( ACTION_KEY_DRAW );
      actionsCombo.add( ACTION_KEY_ADMINISTER );
      elementsCombo.add( ELEMENTS_KEY_ELEMENTS );
      elementsCombo.add( ELEMENTS_KEY_BOUNDARY_UP );
      elementsCombo.add( ELEMENTS_KEY_BOUNDARY_CONDITIONS );
    }
    else if( newValue instanceof ICalculationUnit1D2D )
    {
      actionsCombo.removeAll();
      elementsCombo.removeAll();
      actionsCombo.add( ACTION_KEY_DRAW );
      actionsCombo.add( ACTION_KEY_ADMINISTER );
      elementsCombo.add( ELEMENTS_KEY_BOUNDARY_CONDITIONS );
// elementsCombo.add(ELEMENTS_KEY_BOUNDARY_CONDITIONS);
    }
    else
    {
      actionsCombo.removeAll();
      elementsCombo.removeAll();
      actionsCombo.add( ACTION_KEY_DRAW );
      actionsCombo.add( ACTION_KEY_ADMINISTER );
      elementsCombo.add( ELEMENTS_KEY_ELEMENTS );
      elementsCombo.add( ELEMENTS_KEY_SUBUNITS );
      elementsCombo.add( ELEMENTS_KEY_BOUNDARY_UP );
      elementsCombo.add( ELEMENTS_KEY_BOUNDARY_CONDITIONS );
    }

  }

  public void createControl( CalculationUnitDataModel dataModel, FormToolkit toolkit, Composite parent )
  {
    this.toolkit = toolkit;
    this.parent = parent;
    this.dataModel = dataModel;
    guiComboSelections( parent );
    dataModel.addKeyBasedDataChangeListener( settingsKeyListener );
  }

  private void guiComboSelections( Composite parentComposite )
  {
    rootComposite = new Composite( parentComposite, SWT.FLAT );
    rootComposite.setLayout( new GridLayout( 3, false ) );
// CoolBarManager cbMng = new CoolBarManager();
// CoolBar coolBar = cbMng.createControl( rootComposite );
// coolBar.setLayout( new FillLayout() );
//
// GridData data1 = new GridData(GridData.FILL_HORIZONTAL|GridData.FILL_VERTICAL);
// data1.horizontalSpan = 3;
// coolBar.setLayoutData( data1 );
// cbMng.add( createSetZoomWidgetAction() );
//
// IToolBarManager item;
// System.out.println("ItemSize="+cbMng.getControl().getItemSizes().length);

    toolbarManager = new ToolBarManager();
    ToolBar toolbar = toolbarManager.createControl( rootComposite );
    toolbar.setLayout( new FillLayout() );
    GridData data1 = new GridData( GridData.FILL_HORIZONTAL | GridData.FILL_VERTICAL );
    data1.horizontalSpan = 3;
    toolbar.setLayoutData( data1 );
    populateToolbar( toolbarManager );
    toolbarManager.update( true );

    actionsCombo = new Combo( rootComposite, SWT.RIGHT | SWT.READ_ONLY | SWT.BORDER );
    // actionsCombo.add( ACTION_KEY_ADMINISTER );
    // actionsCombo.add( ACTION_KEY_DRAW );
    GridData data = new GridData( GridData.FILL_HORIZONTAL );
    actionsCombo.setLayoutData( data );
    actionsCombo.addModifyListener( new ModifyListener()
    {

      public void modifyText( ModifyEvent e )
      {
        if( getSelectedCalcUnit() instanceof ICalculationUnit1D2D )
        {
          if( actionsCombo.getText().equals( ACTION_KEY_DRAW ) )
          {
            elementsCombo.removeAll();
            elementsCombo.add( ELEMENTS_KEY_BOUNDARY_UP );
          }
          else if( actionsCombo.getText().equals( ACTION_KEY_ADMINISTER ) )
          {
            elementsCombo.removeAll();
            elementsCombo.add( ELEMENTS_KEY_SUBUNITS );
            elementsCombo.add( ELEMENTS_KEY_BOUNDARY_UP );
            elementsCombo.add( ELEMENTS_KEY_BOUNDARY_CONDITIONS );
          }
        }
      }
    } );

    elementsCombo = new Combo( rootComposite, SWT.RIGHT | SWT.READ_ONLY | SWT.BORDER );

    data = new GridData( GridData.FILL_HORIZONTAL );
    elementsCombo.setLayoutData( data );

    goButton = new Button( rootComposite, SWT.PUSH );
    goButton.setToolTipText( "Funktion aktivieren" );

    goImage = new Image( rootComposite.getDisplay(), KalypsoModel1D2DPlugin.imageDescriptorFromPlugin( PluginUtilities.id( KalypsoModel1D2DPlugin.getDefault() ), "icons/elcl16/nav_go.gif" ).getImageData() );
    goButton.setImage( goImage );
    final SelectionListener goButtonListener = new SelectionListener()
    {

      public void widgetDefaultSelected( SelectionEvent e )
      {

      }

      public void widgetSelected( SelectionEvent e )
      {
        changeStategy();
      }
    };
    goButton.addSelectionListener( goButtonListener );

  }

  private void populateToolbar( ToolBarManager mng )
  {

    IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );

    mng.add( new ActivateStrategyWidgetAction<ZoomInByRectWidget>( widgetWithStrategy, ZoomInByRectWidget.class, "Zoom in", "Zoom in descr", KalypsoModel1D2DUIImages.IMG_DESC_ZOOM_WITH_RECT ) );

    mng.add( new ActivateStrategyWidgetAction<PanToWidget>( widgetWithStrategy, PanToWidget.class, "Pan to", "Pan to", KalypsoModel1D2DUIImages.IMG_DESC_PAN ) );
    // save action
    Action saveAction = new Action()
    {
      /**
       * @see org.eclipse.jface.action.Action#run()
       */
      @Override
      public void run( )
      {
        Util.saveAllModel();
      }

      /**
       * @see org.eclipse.jface.action.Action#getImageDescriptor()
       */
      @Override
      public ImageDescriptor getImageDescriptor( )
      {
        return KalypsoModel1D2DUIImages.IMG_DESC_SAVE;
      }
    };

    saveAction.setText( "Save all models" );
    saveAction.setToolTipText( "Save All models" );
    mng.add( saveAction );
  }

  /**
   * 
   */
  public void changeStategy( )
  {
    final String selectedType = elementsCombo.getText();
    final String selectedAction = actionsCombo.getText();
    IWidgetWithStrategy widgetWithStrategy = (IWidgetWithStrategy) dataModel.getData( ICommonKeys.WIDGET_WITH_STRATEGY );

    final Object selectedWrapper = dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );

    IWidget strategy = null;

    if( ACTION_KEY_ADMINISTER.equals( selectedAction ) )
    {
      if( ELEMENTS_KEY_BOUNDARY_UP.equals( selectedType ) )
      {
        strategy = new AlterCalUnitBorderWidget( dataModel );
      }
      else if( ELEMENTS_KEY_ELEMENTS.equals( selectedType ) )
      {
        strategy = new AddRemoveElementToCalUnitWidget( dataModel );
      }
      else if( ELEMENTS_KEY_SUBUNITS.equals( selectedType ) )
      {
        final Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
        final CreateSubCalculationUnitCopyWizard calculationSubWizard = new CreateSubCalculationUnitCopyWizard( dataModel );
        final WizardDialog wizardDialog = new WizardDialog( shell, calculationSubWizard );
        wizardDialog.open();
      }
      else if( ELEMENTS_KEY_BOUNDARY_CONDITIONS.equals( selectedType ) )
      {
        strategy = new AddRemoveBoundaryConditionToCalUnitWidget( dataModel );
      }
    }
    else if( ACTION_KEY_DRAW.equals( selectedAction ) )
    {
      if( ELEMENTS_KEY_BOUNDARY_UP.equals( selectedType ) )
      {
        if( selectedWrapper instanceof ICalculationUnit1D )
        {
          strategy = new RouteLineElementWidget<IBoundaryLine1D>( "Route boundary line", "Route boundary line", IBoundaryLine1D.class, Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE1D );
        }
        else
        {
          strategy = new RouteLineElementWidget<IBoundaryLine>( "Route boundary line", "Route boundary line", IBoundaryLine.class, Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE );
        }
      }
      else if( ELEMENTS_KEY_ELEMENTS.equals( selectedType ) )
      {
        if( selectedWrapper instanceof ICalculationUnit1D )
          strategy = new CreateFEElement1DWidget();
        if( selectedWrapper instanceof ICalculationUnit2D )
          strategy = new CreateFE2DElementWidget();
      }
      else
      {
        System.out.println( "Drawing not supported for:" + selectedType );
      }

    }
    widgetWithStrategy.setStrategy( strategy );
  }

  public Object getSelectedCalcUnit( )
  {
    return dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
  }
}
