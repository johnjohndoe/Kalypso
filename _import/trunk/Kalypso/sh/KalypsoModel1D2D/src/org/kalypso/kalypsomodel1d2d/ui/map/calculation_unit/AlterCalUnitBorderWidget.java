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

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.featureinput.AddMetaDataToFeatureDialog;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteBoundaryLineCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.IDiscrModel1d2dChangeCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryLineToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveBoundaryLineFromCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.Feature;
import org.kalypsodeegree.model.feature.binding.IFeatureWrapper2;

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

  private static final String TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT = "Remove boundary line from unit";// "Remove Up Stream
                                                                                                    // boundary line";

  private static final String TXT_ADD_META_DATA = "Add Meta Data";// "Add Metadata for Boundary Line";

  private static final String TXT_ADD_BOUNDARY_LINE_TO_UNIT = "Add boundary line to calculation unit";// "Add Up Stream
                                                                                                      // Boundary Line";

  private static final String TXT_REMOVE_BOUNDARY_LINE_FROM_MODEL = "Remove Boundary Line From Model";

  private static final String[][] MENU_ITEM_SPECS = { { TXT_ADD_META_DATA, ICONS_ELCL16_ADD_GIF }, { TXT_ADD_BOUNDARY_LINE_TO_UNIT, ICONS_ELCL16_ADD_GIF },
      { TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT, ICONS_ELCL16_REMOVE_GIF }, { TXT_REMOVE_BOUNDARY_LINE_FROM_MODEL, ICONS_ELCL16_SET_BOUNDARY_GIF } };

  private final KeyBasedDataModel dataModel;

  private JPopupMenu popupMenu;

  private final List<JMenuItem> items = new ArrayList<JMenuItem>();

  public AlterCalUnitBorderWidget( final KeyBasedDataModel dataModel )
  {
    this( new QName[] { IFELine.QNAME }, "Select Elements and add to the current calculation unit", "Select Elements and add to the current calculation unit", dataModel );
  }

  protected AlterCalUnitBorderWidget( final QName themeElementsQName, final String name, final String toolTip, final KeyBasedDataModel dataModel )
  {
    this( new QName[] { themeElementsQName }, name, toolTip, dataModel );
  }

  protected AlterCalUnitBorderWidget( final QName[] themeElementsQNames, final String name, final String toolTip, final KeyBasedDataModel dataModel )
  {
    super( themeElementsQNames, name, toolTip );
    this.dataModel = dataModel;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#clickPopup(java.awt.Point)
   */
  @Override
  public void clickPopup( final Point p )
  {
    final MapPanel mapPanel = (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    if( popupMenu == null )
    {
      popupMenu = createMenu();
    }
    updateMenuItem();
    popupMenu.show( mapPanel, p.x, p.y );
  }

  private ImageIcon getImageIcon( final String pluginRelativPath )
  {
    final ImageIcon imageIcon = new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), pluginRelativPath ) );
    return imageIcon;
  }

  private ActionListener makeActionListener( )
  {
    final ActionListener al = new ActionListener()
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
    for( final JMenuItem item : items )
    {
      final String text = item.getText();
      if( TXT_ADD_BOUNDARY_LINE_TO_UNIT.equals( text ) )
      {
        updateAddUpStreamMenu( item );
      }

      else if( TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT.equals( text ) )
      {
        updateRemoveUpStreamMenu( item );
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

  private void updateRemoveBoundaryLineMenu( final JMenuItem item )
  {
    item.setEnabled( true );
    final IFELine selectedBoundaryLine = getSelectedBoundaryLine();
    if( selectedBoundaryLine == null )
    {
      item.setEnabled( false );
    }
  }

  private void updateAddUpStreamMenu( final JMenuItem item )
  {
    updateGeneralBadSelection( item );
    if( item.isEnabled() )
    {
      final ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
      final IFELine selectedBoundaryLine = getSelectedBoundaryLine();
      if( selectedBoundaryLine != null && calUnit != null )
      {
        if( CalcUnitOps.isBoundaryLineOf( selectedBoundaryLine, calUnit ) )// || CalUnitOps.isUpStreamBoundaryLine(
                                                                          // calUnit, selectedBoundaryLine ) )
        {
          item.setEnabled( false );
        }
      }
    }
  }

  private void updateRemoveUpStreamMenu( final JMenuItem item )
  {
    updateGeneralBadSelection( item );
    if( item.isEnabled() )
    {
      final ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );

      final IFELine selectedBoundaryLine = getSelectedBoundaryLine();
      if( selectedBoundaryLine == null || calUnit == null )
      {
        item.setEnabled( false );
      }
      else
      {
        if( !CalcUnitOps.isBoundaryLineOf( selectedBoundaryLine, calUnit ) )// !CalUnitOps.isDownStreamBoundaryLine(
                                                                            // calUnit, selectedBoundaryLine ) &&
                                                                            // !CalUnitOps.isUpStreamBoundaryLine(
                                                                            // calUnit, selectedBoundaryLine ) )
        {
          item.setEnabled( false );
        }
      }
    }
  }

  private void updateGeneralBadSelection( final JMenuItem item )
  {
    final Feature[] selectedFeature = getSelectedFeature();
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

  synchronized void doMenuAction( final String text )
  {
    final IFeatureWrapper2 boundaryFeature = (IFeatureWrapper2) dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );

    if( text == null )
    {
      return;
    }
    else if( TXT_ADD_META_DATA.equals( text ) )
    {
      final IFELine bLine = getSelectedBoundaryLine();

      final Display display = (Display) dataModel.getData( ICommonKeys.KEY_SELECTED_DISPLAY );
      final Runnable runnable = new Runnable()
      {
        public void run( )
        {
          Shell shell = display.getActiveShell();
          final AddMetaDataToFeatureDialog setFeatureDialog = new AddMetaDataToFeatureDialog( shell );
          setFeatureDialog.open();
          if( setFeatureDialog.getName() != null )
          {
            bLine.setName( setFeatureDialog.getName() );
          }
          if( setFeatureDialog.getDescription() != null )
          {
            bLine.setDescription( setFeatureDialog.getDescription() );
          }
        }
      };
      display.asyncExec( runnable );
    }
    else if( TXT_ADD_BOUNDARY_LINE_TO_UNIT.equals( text ) )// || TXT_ADD_BOUNDARY_LINE_DOWN_STREAM.equals( text ) )
    {
      actionAddBoundaryLineToUnit( text );
    }
    else if( TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT.equals( text ) )
    {
      actionRemoveBoundaryLineFromUnit( text );
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

  private void actionRemoveBoundaryLineFromModel( final String itemText )
  {
    final IFELine bLine = getSelectedBoundaryLine();
    final IFEDiscretisationModel1d2d model1d2d = dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
    final MapPanel mapPanel = dataModel.getData( MapPanel.class, ICommonKeys.KEY_MAP_PANEL );
    final IDiscrModel1d2dChangeCommand delCmd = new DeleteBoundaryLineCmd( model1d2d, bLine )
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.DeleteBoundaryLineCmd#process()
       */
      @Override
      public void process( ) throws Exception
      {
        super.process();

        KeyBasedDataModelUtil.resetCurrentEntry( dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        KeyBasedDataModelUtil.repaintMapPanel( dataModel, ICommonKeys.KEY_MAP_PANEL );
      }
    };
    KeyBasedDataModelUtil.postCommand( dataModel, delCmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
  }

  private void actionAddBoundaryLineToUnit( final String itemText )
  {
    final ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFELine bLine = getSelectedBoundaryLine();
    if( !itemText.equals( TXT_ADD_BOUNDARY_LINE_TO_UNIT ) )
    {
      throw new RuntimeException( "Unknown itemText:" + itemText );
    }

    final IFEDiscretisationModel1d2d model1d2d = dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
    final AddBoundaryLineToCalculationUnitCmd cmd = new AddBoundaryLineToCalculationUnitCmd( calUnit, bLine, model1d2d )
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
          KeyBasedDataModelUtil.resetCurrentEntry( dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
          KeyBasedDataModelUtil.repaintMapPanel( dataModel, ICommonKeys.KEY_MAP_PANEL );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    };
    KeyBasedDataModelUtil.postCommand( dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
  }

  private void actionRemoveBoundaryLineFromUnit( final String itemText )
  {
    final ICalculationUnit calUnit = dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFELine bLine = getSelectedBoundaryLine();

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
        KeyBasedDataModelUtil.resetCurrentEntry( dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        KeyBasedDataModelUtil.repaintMapPanel( dataModel, ICommonKeys.KEY_MAP_PANEL );
      }
    };

    KeyBasedDataModelUtil.postCommand( dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

  }

  synchronized private JPopupMenu createMenu( )
  {

    final JPopupMenu menu = new JPopupMenu();
    final ActionListener actionListener = makeActionListener();

    for( final String[] spec : MENU_ITEM_SPECS )
    {
      if( spec.length != 2 )
      {
        throw new RuntimeException( "Spec must have length 2, but has:" + spec.length );
      }
      final String text = spec[0];

      if( SEPARATOR_PSEUDO_TEXT.equals( text ) )
      {
        menu.addSeparator();
      }
      else
      {
        final JMenuItem addElement = new JMenuItem();
        addElement.setText( text );
        addElement.setIcon( getImageIcon( spec[1] ) );
        addElement.addActionListener( actionListener );
        menu.add( addElement );
        items.add( addElement );
      }
    }
    return menu;
  }

  private final IFELine getSelectedBoundaryLine( )
  {
    final IFELine[] bLines = getWrappedSelectedFeature( IFELine.class );
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
