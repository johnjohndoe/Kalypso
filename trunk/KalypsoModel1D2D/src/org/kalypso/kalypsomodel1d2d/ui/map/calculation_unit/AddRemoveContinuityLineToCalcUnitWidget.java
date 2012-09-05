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

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.util.ArrayList;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryLineToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveBoundaryLineFromCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.util.UtilMap;
import org.kalypso.ogc.gml.IKalypsoFeatureTheme;
import org.kalypso.ogc.gml.map.IMapPanel;
import org.kalypso.ogc.gml.map.utilities.tooltip.ToolTipRenderer;
import org.kalypso.ogc.gml.map.widgets.AbstractDelegateWidget;
import org.kalypso.ogc.gml.map.widgets.SelectFeatureWidget;
import org.kalypso.ogc.gml.selection.IFeatureSelectionManager;

/**
 * @author Patrice Congo
 * @author Thomas Jung (changes in order to use the common SelectFeatureWidget)
 */

public class AddRemoveContinuityLineToCalcUnitWidget extends AbstractDelegateWidget
{
  private static final String SEPARATOR_PSEUDO_TEXT = "_separator_pseudo_text_"; //$NON-NLS-1$

  private static final String ICONS_ELCL16_REMOVE_GIF = "icons/elcl16/remove.gif"; //$NON-NLS-1$

  private static final String ICONS_ELCL16_ADD_GIF = "icons/elcl16/add.gif"; //$NON-NLS-1$

  private static final String TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveContinuityLineToCalcUnitWidget.1" ); //$NON-NLS-1$

  private static final String TXT_ADD_BOUNDARY_LINE_TO_UNIT = Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveContinuityLineToCalcUnitWidget.2" ); //$NON-NLS-1$

  private static final String[][] MENU_ITEM_SPECS = { { TXT_ADD_BOUNDARY_LINE_TO_UNIT, ICONS_ELCL16_ADD_GIF }, { TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT, ICONS_ELCL16_REMOVE_GIF } };

  protected final KeyBasedDataModel m_dataModel;

  private JPopupMenu m_popupMenu;

  private final List<JMenuItem> m_popupMenuItems = new ArrayList<>();

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final SelectFeatureWidget m_selDelegateWidget;

  private final QName[] m_themeElementQNames = new QName[] { IFELine.QNAME };

  private IKalypsoFeatureTheme[] m_featureThemes;

  public AddRemoveContinuityLineToCalcUnitWidget( final KeyBasedDataModel dataModel )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveContinuityLineToCalcUnitWidget.5" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveContinuityLineToCalcUnitWidget.6" ), new SelectFeatureWidget( "", "", new QName[] { IFELine.QNAME }, IFELine.PROP_GEOMETRY ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveContinuityLineToCalcUnitWidget.9" ) ); //$NON-NLS-1$
    final Color color = new Color( 1f, 1f, 0.6f, 0.70f );
    m_toolTipRenderer.setBackgroundColor( color );

    m_dataModel = dataModel;
    m_selDelegateWidget = (SelectFeatureWidget)getDelegate();
  }

  @Override
  public void clickPopup( final Point p )
  {
    // TODO: we should discuss, if we want this right-click popup behavior. Right now it is only used in the calcunit
    // widgets and no common kalypso style...

    final IMapPanel mapPanel = (IMapPanel)m_dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    if( m_popupMenu == null )
      m_popupMenu = createMenu();
    updateMenuItem();
    m_popupMenu.show( (Component)mapPanel, p.x, p.y );
    super.clickPopup( p );
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( e.getKeyChar() == KeyEvent.VK_ESCAPE )
      reinit();

    // Remove
    else if( e.getKeyChar() == KeyEvent.VK_DELETE )
    {
      final IFELine[] selectedBoundaryLines = CalcUnitHelper.getSelectedBoundaryLine( mapPanel );

      actionRemoveBoundaryLineFromUnit( selectedBoundaryLines );
    }
    // Add
    else if( e.getKeyCode() == KeyEvent.VK_INSERT )
    {
      final IFELine[] selectedBoundaryLines = CalcUnitHelper.getSelectedBoundaryLine( mapPanel );

      actionAddBoundaryLineToUnit( selectedBoundaryLines );
    }
    super.keyPressed( e );
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
      @Override
      public void actionPerformed( final ActionEvent e )
      {
        final Object source = e.getSource();
        if( !(source instanceof JMenuItem) )
        {
          return;
        }
        doMenuAction( ((JMenuItem)source).getText() );
      }
    };
    return al;
  }

  @Override
  public void activate( final ICommandTarget commandPoster, final IMapPanel mapPanel )
  {
    super.activate( commandPoster, mapPanel );
    reinit();
  }

  private void reinit( )
  {
    final IMapPanel mapPanel = getMapPanel();

    m_featureThemes = new IKalypsoFeatureTheme[m_themeElementQNames.length];
    for( int i = 0; i < m_themeElementQNames.length; i++ )
      m_featureThemes[i] = UtilMap.findEditableTheme( mapPanel, m_themeElementQNames[i] );

    m_selDelegateWidget.setThemes( m_featureThemes );

    final IFeatureSelectionManager selectionManager = mapPanel.getSelectionManager();
    selectionManager.clear();
    mapPanel.repaintMap();
  }

  private void updateMenuItem( )
  {
    for( final JMenuItem item : m_popupMenuItems )
    {
      item.setEnabled( true );
    }
  }

  synchronized void doMenuAction( final String text )
  {
    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel == null )
      return;

    if( text == null )
      return;

    else if( TXT_ADD_BOUNDARY_LINE_TO_UNIT.equals( text ) )
      actionAddBoundaryLineToUnit( CalcUnitHelper.getSelectedBoundaryLine( mapPanel ) );
    else if( TXT_REMOVE_BOUNDARY_LINE_FROM_UNIT.equals( text ) )
      actionRemoveBoundaryLineFromUnit( CalcUnitHelper.getSelectedBoundaryLine( mapPanel ) );
  }

  private void actionAddBoundaryLineToUnit( final IFELine[] selectedBoundaryLines )
  {
    final ICalculationUnit calUnit = m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );

    if( selectedBoundaryLines == null )
      return;

    for( final IFELine bLine : selectedBoundaryLines )
    {
      final AddBoundaryLineToCalculationUnitCmd cmd = new AddBoundaryLineToCalculationUnitCmd( calUnit, bLine, model1d2d )
      {
        @Override
        public void process( ) throws Exception
        {
          super.process();
          getMapPanel().getSelectionManager().clear();
          KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        }
      };
      KeyBasedDataModelUtil.postCommand( m_dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
    }
  }

  private void actionRemoveBoundaryLineFromUnit( final IFELine[] selectedBoundaryLines )
  {
    final ICalculationUnit calUnit = m_dataModel.getData( ICalculationUnit.class, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFEDiscretisationModel1d2d model1d2d = m_dataModel.getData( IFEDiscretisationModel1d2d.class, ICommonKeys.KEY_DISCRETISATION_MODEL );

    if( selectedBoundaryLines == null )
      return;

    for( final IFELine bLine : selectedBoundaryLines )
    {
      final RemoveBoundaryLineFromCalculationUnitCmd cmd = new RemoveBoundaryLineFromCalculationUnitCmd( calUnit, bLine, model1d2d )
      {
        @Override
        public void process( ) throws Exception
        {
          super.process();
          getMapPanel().getSelectionManager().clear();
          KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        }
      };

      KeyBasedDataModelUtil.postCommand( m_dataModel, cmd, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
    }
  }

  synchronized private JPopupMenu createMenu( )
  {
    final JPopupMenu menu = new JPopupMenu();
    final ActionListener actionListener = makeActionListener();

    for( final String[] spec : MENU_ITEM_SPECS )
    {
      if( spec.length != 2 )
        throw new RuntimeException( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveContinuityLineToCalcUnitWidget.10" ) + spec.length ); //$NON-NLS-1$
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

  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );

    final IMapPanel mapPanel = getMapPanel();
    if( mapPanel != null )
    {
      final Rectangle bounds = mapPanel.getScreenBounds();
      final String delegateTooltip = getDelegate().getToolTip();

      m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveContinuityLineToCalcUnitWidget.11" ) + delegateTooltip ); //$NON-NLS-1$

      m_toolTipRenderer.paintToolTip( new Point( 5, bounds.height - 5 ), g, bounds );
    }
  }

  @Override
  public void finish( )
  {
    getMapPanel().getSelectionManager().clear();
    super.finish();
  }
}