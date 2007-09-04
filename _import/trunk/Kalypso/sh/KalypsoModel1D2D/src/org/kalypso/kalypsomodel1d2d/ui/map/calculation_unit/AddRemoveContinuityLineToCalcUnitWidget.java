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

import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.CalcUnitOps;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryLineToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveBoundaryLineFromCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Patrice Congo
 * 
 */

public class AddRemoveContinuityLineToCalcUnitWidget extends FENetConceptSelectionWidget
{
  private static final String SEPARATOR_PSEUDO_TEXT = "_separator_pseudo_text_"; //$NON-NLS-1$

  private static final String ICONS_ELCL16_REMOVE_GIF = "icons/elcl16/remove.gif"; //$NON-NLS-1$

  private static final String ICONS_ELCL16_ADD_GIF = "icons/elcl16/add.gif"; //$NON-NLS-1$

  private static final String TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT = Messages.getString("AddRemoveContinuityLineToCalcUnitWidget.3"); //$NON-NLS-1$

  private static final String TXT_ADD_BOUNDARY_LINE_TO_UNIT = Messages.getString("AddRemoveContinuityLineToCalcUnitWidget.4"); //$NON-NLS-1$

  private static final String[][] MENU_ITEM_SPECS = { { TXT_ADD_BOUNDARY_LINE_TO_UNIT, ICONS_ELCL16_ADD_GIF }, { TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT, ICONS_ELCL16_REMOVE_GIF } };

  private final KeyBasedDataModel m_dataModel;

  private JPopupMenu m_popupMenu;

  private final List<JMenuItem> m_popupMenuItems = new ArrayList<JMenuItem>();

  public AddRemoveContinuityLineToCalcUnitWidget( final KeyBasedDataModel dataModel )
  {
    this( new QName[] { IFELine.QNAME }, Messages.getString("AddRemoveContinuityLineToCalcUnitWidget.5"), Messages.getString("AddRemoveContinuityLineToCalcUnitWidget.6"), dataModel ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  protected AddRemoveContinuityLineToCalcUnitWidget( final QName themeElementsQName, final String name, final String toolTip, final KeyBasedDataModel dataModel )
  {
    this( new QName[] { themeElementsQName }, name, toolTip, dataModel );
  }

  protected AddRemoveContinuityLineToCalcUnitWidget( final QName[] themeElementsQNames, final String name, final String toolTip, final KeyBasedDataModel dataModel )
  {
    super( themeElementsQNames, name, toolTip );
    m_dataModel = dataModel;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#clickPopup(java.awt.Point)
   */
  @Override
  public void clickPopup( final Point p )
  {
    final MapPanel mapPanel = (MapPanel) m_dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    if( m_popupMenu == null )
      m_popupMenu = createMenu();
    updateMenuItem();
    m_popupMenu.show( mapPanel, p.x, p.y );
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
    for( final JMenuItem item : m_popupMenuItems )
    {
      final String text = item.getText();
      if( TXT_ADD_BOUNDARY_LINE_TO_UNIT.equals( text ) )
        updateAddUpStreamMenu( item );
      else if( TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT.equals( text ) )
        updateRemoveUpStreamMenu( item );
    }
  }

  private void updateAddUpStreamMenu( final JMenuItem item )
  {
    updateGeneralBadSelection( item );
    final ICalculationUnit calUnit = m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFELine selectedBoundaryLine = getSelectedBoundaryLine();
    if( selectedBoundaryLine != null && calUnit != null )
    {
      if( CalcUnitOps.isBoundaryLineOf( selectedBoundaryLine, calUnit ) )
        item.setEnabled( false );
      else
        item.setEnabled( true );
    }
  }

  private void updateRemoveUpStreamMenu( final JMenuItem item )
  {
    updateGeneralBadSelection( item );
    if( item.isEnabled() )
    {
      final ICalculationUnit calUnit = m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
      final IFELine selectedBoundaryLine = getSelectedBoundaryLine();
      if( selectedBoundaryLine == null || calUnit == null )
        item.setEnabled( false );
      else if( !CalcUnitOps.isBoundaryLineOf( selectedBoundaryLine, calUnit ) )
        item.setEnabled( false );
    }
  }

  private void updateGeneralBadSelection( final JMenuItem item )
  {
    final Feature[] selectedFeature = getSelectedFeature();
    item.setEnabled( true );
    if( selectedFeature == null )
      item.setEnabled( false );
    else if( selectedFeature.length != 1 )
      item.setEnabled( false );
    else if( m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER ) == null )
      item.setEnabled( false );
  }

  synchronized void doMenuAction( final String text )
  {
    if( text == null )
      return;
    else if( TXT_ADD_BOUNDARY_LINE_TO_UNIT.equals( text ) )
      actionAddBoundaryLineToUnit( text );
    else if( TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT.equals( text ) )
      actionRemoveBoundaryLineFromUnit( text );
  }

  private void actionAddBoundaryLineToUnit( final String itemText )
  {
    final ICalculationUnit calUnit = m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFELine bLine = getSelectedBoundaryLine();
    if( !itemText.equals( TXT_ADD_BOUNDARY_LINE_TO_UNIT ) )
    {
      throw new RuntimeException( "Unknown itemText:" + itemText ); //$NON-NLS-1$
    }

    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
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
          KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
          KeyBasedDataModelUtil.repaintMapPanel( m_dataModel, ICommonKeys.KEY_MAP_PANEL );
        }
        catch( Exception e )
        {
          e.printStackTrace();
        }
      }
    };
    KeyBasedDataModelUtil.postCommand( m_dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
  }

  private void actionRemoveBoundaryLineFromUnit( final String itemText )
  {
    final ICalculationUnit calUnit = m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFELine bLine = getSelectedBoundaryLine();

    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );
    final RemoveBoundaryLineFromCalculationUnitCmd cmd = new RemoveBoundaryLineFromCalculationUnitCmd( calUnit, bLine, model1d2d )
    {
      /**
       * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryLineToCalculationUnit#process()
       */
      @Override
      public void process( ) throws Exception
      {
        super.process();
        KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        KeyBasedDataModelUtil.repaintMapPanel( m_dataModel, ICommonKeys.KEY_MAP_PANEL );
      }
    };

    KeyBasedDataModelUtil.postCommand( m_dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

  }

  synchronized private JPopupMenu createMenu( )
  {
    final JPopupMenu menu = new JPopupMenu();
    final ActionListener actionListener = makeActionListener();

    for( final String[] spec : MENU_ITEM_SPECS )
    {
      if( spec.length != 2 )
        throw new RuntimeException( "Spec must have length 2, but has:" + spec.length ); //$NON-NLS-1$
      final String text = spec[0];
      if( SEPARATOR_PSEUDO_TEXT.equals( text ) )
        menu.addSeparator();
      else
      {
        final JMenuItem addElement = new JMenuItem();
        addElement.setText( text );
        addElement.setIcon( getImageIcon( spec[1] ) );
        addElement.addActionListener( actionListener );
        menu.add( addElement );
        m_popupMenuItems.add( addElement );
      }
    }
    return menu;
  }

  private final IFELine getSelectedBoundaryLine( )
  {
    final IFELine[] bLines = getWrappedSelectedFeature( IFELine.class );
    if( bLines == null )
      return null;
    else if( bLines.length == 0 )
      return null;
    else
      return bLines[0];
  }

}
