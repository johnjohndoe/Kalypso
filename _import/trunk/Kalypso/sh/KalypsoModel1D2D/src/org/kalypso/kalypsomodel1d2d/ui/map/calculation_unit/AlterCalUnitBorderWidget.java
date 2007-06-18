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

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.xml.namespace.QName;

import org.eclipse.core.resources.IFolder;
import org.eclipse.jface.wizard.WizardDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.internal.dialogs.ImportExportWizard;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.CalUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IBoundaryLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DContinuityLine;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ILineElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IOperationalModel1D2D;
import org.kalypso.kalypsomodel1d2d.schema.dict.Kalypso1D2DDictConstants;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteBoundaryLineCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteCmdFactory;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryLineToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveBoundaryLineFromCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.CreateNodalBCFlowrelationWidget;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.IBoundaryConditionDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.NodalBCSelectionWizard;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.TimeserieStepDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.WQStepDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.map.flowrel.ZmlChooserStepDescriptor;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.kalypsosimulationmodel.core.KalypsoSimBaseFeatureFactory;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.kalypsosimulationmodel.core.modeling.IOperationalModel;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.GMLWorkspace;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;
import org.kalypsodeegree.model.geometry.GM_Point;

/**
 * @author Patrice Congo
 * 
 */

// @TODO Start implementing the addition of this to the calculationUnit.
@SuppressWarnings( { "unchecked", "hiding", "synthetic-access" })
public class AlterCalUnitBorderWidget extends FENetConceptSelectionWidget
{
  private static final String SEPARATOR_PSEUDO_TEXT = "_separator_pseudo_text_";

  private static final String ICONS_ELCL16_REMOVE_GIF = "icons/elcl16/remove.gif";

  private static final String ICONS_ELCL16_ADD_GIF = "icons/elcl16/add.gif";

  private static final String ICONS_ELCL16_SET_BOUNDARY_GIF = "icons/elcl16/addBoundary.gif";

  private static final String TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT = 
                         "Remove boundary line from unit";// "Remove Up Stream boundary line";

  private static final String TXT_ADD_BOUNDARY_LINE_TO_UNIT = 
                          "Add boundary line to calculation unit";//"Add Up Stream Boundary Line";

//  private static final String TXT_ADD_BOUNDARY_LINE_DOWN_STREAM = "Add Down Stream Boundary Line";

  private static final String TXT_SET_BOUNDARY_CONDITION = "Set Boundary Condition";

  private static final String TXT_REMOVE_BOUNDARY_LINE_FROM_MODEL = "Remove Boundary Line From Model";

  private static final String[][] MENU_ITEM_SPECS = { 
      { TXT_ADD_BOUNDARY_LINE_TO_UNIT, ICONS_ELCL16_ADD_GIF }, 
//      { TXT_ADD_BOUNDARY_LINE_DOWN_STREAM, ICONS_ELCL16_ADD_GIF },
      { TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT, ICONS_ELCL16_REMOVE_GIF },
// {TXT_REMOVE_BOUNDARY_LINE_DOWN_STREAM, ICONS_ELCL16_REMOVE_GIF},
      { TXT_SET_BOUNDARY_CONDITION, ICONS_ELCL16_SET_BOUNDARY_GIF }, 
      { TXT_REMOVE_BOUNDARY_LINE_FROM_MODEL, ICONS_ELCL16_SET_BOUNDARY_GIF } };

  private KeyBasedDataModel dataModel;

  private JPopupMenu popupMenu;

  private List<JMenuItem> items = new ArrayList<JMenuItem>();

  public AlterCalUnitBorderWidget( KeyBasedDataModel dataModel )
  {
    this( new QName[] { Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE }, "Select Elements and add to the current calculation unit", "Select Elements and add to the current calculation unit", dataModel );
  }

  protected AlterCalUnitBorderWidget( QName themeElementsQName, String name, String toolTip, KeyBasedDataModel dataModel )
  {
    this( new QName[] { themeElementsQName }, name, toolTip, dataModel );
  }

  protected AlterCalUnitBorderWidget( QName[] themeElementsQNames, String name, String toolTip, KeyBasedDataModel dataModel )
  {
    super( themeElementsQNames, name, toolTip );
    this.dataModel = dataModel;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#clickPopup(java.awt.Point)
   */
  @Override
  public void clickPopup( Point p )
  {
    MapPanel mapPanel = (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    if( popupMenu == null )
    {
      popupMenu = createMenu();
    }
    updateMenuItem();
    popupMenu.show( mapPanel, p.x, p.y );
  }

  private ImageIcon getImageIcon( String pluginRelativPath )
  {
    ImageIcon imageIcon = new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), pluginRelativPath ) );
    return imageIcon;
  }

  private ActionListener makeActionListener( )
  {
    ActionListener al = new ActionListener()
    {

      public void actionPerformed( ActionEvent e )
      {
        Object source = e.getSource();
        if( !(source instanceof JMenuItem) )
        {
          return;
        }
        doMenuAction( ((JMenuItem) source).getText() );

      }

    };
    return al;
  }

  private void updateMenuItem( )
  {
    for( JMenuItem item : items )
    {
      final String text = item.getText();
      if( TXT_ADD_BOUNDARY_LINE_TO_UNIT.equals( text ) )
      {
        updateAddUpStreamMenu( item );
      }
//      else if( TXT_ADD_BOUNDARY_LINE_DOWN_STREAM.equals( text ) )
//      {
//        updateAddUpStreamMenu( item );
//      }
      else if( TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT.equals( text ) )
      {
        updateRemoveUpStreamMenu( item );
      }
      else if( TXT_SET_BOUNDARY_CONDITION.equals( text ) )
      {
        updateSetBoundaryMenu( item );
      }
      else if( TXT_REMOVE_BOUNDARY_LINE_FROM_MODEL.equals( text ) )
      {
        updateRemoveBoundaryLineMenu( item );
      }
      else
      {

      }
    }
  }

  private void updateRemoveBoundaryLineMenu( JMenuItem item )
  {
    // updateGeneralBadSelection( item );

    item.setEnabled( true );
    final IBoundaryLine selectedBoundaryLine = getSelectedBoundaryLine();
    if( selectedBoundaryLine == null )
    {
      item.setEnabled( false );
    }
  }

  private void updateAddUpStreamMenu( JMenuItem item )
  {
    updateGeneralBadSelection( item );
    if( item.isEnabled() )
    {
      final ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );

      final IBoundaryLine selectedBoundaryLine = getSelectedBoundaryLine();
      if( selectedBoundaryLine != null && calUnit != null )
      {
        if( CalUnitOps.isBoundaryLineOf( selectedBoundaryLine, calUnit ))// || CalUnitOps.isUpStreamBoundaryLine( calUnit, selectedBoundaryLine ) )
        {
          item.setEnabled( false );
        }
      }
    }
  }

  private void updateRemoveUpStreamMenu( JMenuItem item )
  {
    updateGeneralBadSelection( item );
    if( item.isEnabled() )
    {
      ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );

      IBoundaryLine selectedBoundaryLine = getSelectedBoundaryLine();
      if( selectedBoundaryLine == null || calUnit == null )
      {
        item.setEnabled( false );
      }
      else
      {
        if( !CalUnitOps.isBoundaryLineOf( selectedBoundaryLine, calUnit ))// !CalUnitOps.isDownStreamBoundaryLine( calUnit, selectedBoundaryLine ) && !CalUnitOps.isUpStreamBoundaryLine( calUnit, selectedBoundaryLine ) )
        {
          item.setEnabled( false );
        }
      }
    }
  }

  private void updateSetBoundaryMenu( JMenuItem item )
  {
    updateGeneralBadSelection( item );
  }

  private void updateGeneralBadSelection( JMenuItem item )
  {
    Feature[] selectedFeature = getSelectedFeature();
    item.setEnabled( true );

    if( selectedFeature == null )
    {
      item.setEnabled( false );
    }
    else if( selectedFeature.length != 1 )
    {
      item.setEnabled( false );
    }
    else if( dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER ) == null )
    {
      item.setEnabled( false );
    }
  }

    synchronized void doMenuAction( String text )
  {
     final IFeatureWrapper2 boundaryFeature = (IFeatureWrapper2) dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
      
    if( text == null )
    {
      return;
    }
    else if( TXT_ADD_BOUNDARY_LINE_TO_UNIT.equals( text ))// || TXT_ADD_BOUNDARY_LINE_DOWN_STREAM.equals( text ) )
    {
      actionAddBoundaryLineToUnit( text );
    }
    else if( TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT.equals( text ) )
    {
      actionRemoveBoundaryLineFromUnit( text );
    }
    else if( TXT_SET_BOUNDARY_CONDITION.equals( text ) )
    {
      final Display display = (Display) dataModel.getData(ICommonKeys.KEY_SELECTED_DISPLAY);
      final Runnable runnable = new Runnable()
      {
        private CommandableWorkspace workspace;

        public void run( )
        {   
          try
          {
             final IBoundaryConditionDescriptor[] descriptors = 
               CreateNodalBCFlowrelationWidget.createTimeserieDescriptors( getSelectedBoundaryLine(),Util.getScenarioFolder() );
           //@ TODO Add two more parameters.
             if (dataModel.getData( ICommonKeys.KEY_COMMAND_MANAGER) instanceof CommandableWorkspace)
             {
               workspace = (CommandableWorkspace) dataModel.getData( ICommonKeys.KEY_COMMAND_MANAGER);
             }
             
             ///model is to be  get from the calculation unit
             final IFlowRelationshipModel opModel=
                        Util.getModel( IFlowRelationshipModel.class );
             final Feature opModelFeature = opModel.getWrappedFeature();
             final IRelationType parentRelation = 
                       opModel.getWrappedList().getParentFeatureTypeProperty();
             
             GMLWorkspace opWorkspace = opModel.getWrappedFeature().getWorkspace();
             
            NodalBCSelectionWizard wizard = new NodalBCSelectionWizard(
                                              descriptors,
                                              opWorkspace,
                                              opModelFeature,
                                              parentRelation );
            System.out.println("OpWorkspace:"+opWorkspace.getContext());
            GM_Point boundaryPosition = getBoundaryPosition();
            wizard.setBoundaryPosition( boundaryPosition );
            final WizardDialog wizardDialog = new WizardDialog( display.getActiveShell(), wizard );
            wizardDialog.open();
          }
          catch (Exception e) {
            e.printStackTrace();
            throw new RuntimeException(e);
          }
        }

        
      };
      display.syncExec( runnable );     
    }
    
    

    else if( TXT_REMOVE_BOUNDARY_LINE_FROM_MODEL.equals( text ) )
    {
      actionRemoveBoundaryLineFromModel( text );
    }
    else
    {
      System.out.println( "Not supported menu action:" + text );
    }
  }
    
    private GM_Point getBoundaryPosition( )
    {
      IBoundaryLine selectedBoundaryLine = getSelectedBoundaryLine();
      
      return null;
    }
    
//    private IBoundaryConditionDescriptor[] createTimeserieDescriptors( final IFeatureWrapper2 modelElement, final IFolder scenarioFolder )
//    {
//      final TimeserieStepDescriptor wstTimeDescriptor = new TimeserieStepDescriptor( "Wasserstand - Zeitreihe", Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_WATERLEVEL );
//      final TimeserieStepDescriptor qTimeDescriptor = new TimeserieStepDescriptor( "Abfluss - Zeitreihe", Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE );
//      final TimeserieStepDescriptor specQ1TimeDescriptor = new TimeserieStepDescriptor( "Spezifische Abfluss - Zeitreihe", Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE_1D );
//      final TimeserieStepDescriptor specQ2TimeDescriptor = new TimeserieStepDescriptor( "Spezifische Abfluss - Zeitreihe", Kalypso1D2DDictConstants.DICT_COMPONENT_TIME, Kalypso1D2DDictConstants.DICT_COMPONENT_DISCHARGE_2D );
//      final WQStepDescriptor wqDescriptor = new WQStepDescriptor( "W/Q - Beziehung" );
//
//      final IFolder importFolder = scenarioFolder.getProject().getFolder( "imports" ).getFolder( "timeseries" );
//      final ZmlChooserStepDescriptor zmlChooser = new ZmlChooserStepDescriptor( "Importierte Zeitreihe", importFolder );
//
//      // TODO: ask ingenieurs what is right here:
//      if( modelElement instanceof IElement1D )
//        return new IBoundaryConditionDescriptor[] { specQ1TimeDescriptor, wqDescriptor, zmlChooser };
//
//      if( modelElement instanceof IPolyElement )
//        return new IBoundaryConditionDescriptor[] { specQ2TimeDescriptor, zmlChooser };
//
//      if( modelElement instanceof IFE1D2DNode )
//        return new IBoundaryConditionDescriptor[] { wstTimeDescriptor, qTimeDescriptor, zmlChooser };
//
//      if( modelElement instanceof ILineElement)
//        return new IBoundaryConditionDescriptor[] { wstTimeDescriptor, qTimeDescriptor, zmlChooser };
//      
//      return new IBoundaryConditionDescriptor[] {};
//    }

  private void actionRemoveBoundaryLineFromModel( String itemText )
  {
    final IBoundaryLine bLine = getSelectedBoundaryLine();
    final IFEDiscretisationModel1d2d model1d2d = 
              dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
    final MapPanel mapPanel = 
              dataModel.getData( MapPanel.class, ICommonKeys.KEY_MAP_PANEL );
    IDiscrModel1d2dChangeCommand delCmd = new DeleteBoundaryLineCmd( model1d2d, bLine )
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteBoundaryLineCmd#process()
       */
      @Override
      public void process( ) throws Exception
      {
        super.process();
        
        KeyBasedDataModelUtil.resetCurrentEntry( 
                dataModel,
                ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        KeyBasedDataModelUtil.repaintMapPanel( 
                dataModel, 
                ICommonKeys.KEY_MAP_PANEL );
      }
    };
    KeyBasedDataModelUtil.postCommand( dataModel, delCmd );

  }

  private void actionAddBoundaryLineToUnit( String itemText )
  {
    final ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IBoundaryLine bLine = getSelectedBoundaryLine();
    final QName relationType;
    if( itemText.equals( TXT_ADD_BOUNDARY_LINE_TO_UNIT ) )
    {
      relationType = Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_UPSTREAM;
    }
//    else if( itemText.equals( TXT_ADD_BOUNDARY_LINE_DOWN_STREAM ) )
//    {
//      relationType = Kalypso1D2DSchemaConstants.WB1D2D_PROP_BOUNDARY_LINE_DOWNSTREAM;
//    }
    else
    {
      throw new RuntimeException( "Unknown itemText:" + itemText );
    }

    IFEDiscretisationModel1d2d model1d2d = 
          dataModel.getData( 
              IFEDiscretisationModel1d2d.class, 
              ICommonKeys.KEY_DISCRETISATION_MODEL );
    final AddBoundaryLineToCalculationUnitCmd cmd = 
          new AddBoundaryLineToCalculationUnitCmd( 
                  calUnit, bLine, model1d2d, relationType )
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryLineToCalculationUnit#process()
       */
      @Override
      public void process( ) throws Exception
      {
        super.process();
        try
        {
          KeyBasedDataModelUtil.resetCurrentEntry( 
              dataModel,  
              ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
          KeyBasedDataModelUtil.repaintMapPanel(
              dataModel, 
              ICommonKeys.KEY_MAP_PANEL );
        }
        catch (Exception e) 
        {
          e.printStackTrace();
        }
      }
    };
    KeyBasedDataModelUtil.postCommand( dataModel, cmd );
  }

  private void actionRemoveBoundaryLineFromUnit( String itemText )
  {
    final ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IBoundaryLine bLine = getSelectedBoundaryLine();

    final IFEDiscretisationModel1d2d model1d2d = dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
    final RemoveBoundaryLineFromCalculationUnitCmd cmd = new RemoveBoundaryLineFromCalculationUnitCmd( calUnit, bLine, model1d2d )
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryLineToCalculationUnit#process()
       */
      @Override
      public void process( ) throws Exception
      {
        super.process();
        KeyBasedDataModelUtil.resetCurrentEntry( 
                  dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        KeyBasedDataModelUtil.repaintMapPanel( 
                  dataModel, ICommonKeys.KEY_MAP_PANEL );
      }
    };

    KeyBasedDataModelUtil.postCommand( dataModel, cmd );

  }

  synchronized private JPopupMenu createMenu( )
  {

    JPopupMenu menu = new JPopupMenu();
    final ActionListener actionListener = makeActionListener();

    for( String[] spec : MENU_ITEM_SPECS )
    {
      if( spec.length != 2 )
      {
        throw new RuntimeException( "Spec must have length 2, but has:" + spec.length );
      }
      String text = spec[0];

      if( SEPARATOR_PSEUDO_TEXT.equals( text ) )
      {
        menu.addSeparator();
      }
      else
      {
        JMenuItem addElement = new JMenuItem();
        addElement.setText( text );
        addElement.setIcon( getImageIcon( spec[1] ) );
        addElement.addActionListener( actionListener );
        menu.add( addElement );
        items.add( addElement );
      }
    }
    return menu;
  }

  private final IBoundaryLine getSelectedBoundaryLine( )
  {
    IBoundaryLine[] bLines = (IBoundaryLine[]) getWrappedSelectedFeature( IBoundaryLine.class );
    if( bLines == null )
    {
      return null;
    }
    else if( bLines.length == 0 )
    {
      return null;
    }
    else
    {
      return bLines[0];
    }
  }

}
