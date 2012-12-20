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

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.i18n.Messages;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddElementToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveElementFromCalculationUnitCmd;
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
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * @author Thomas Jung (changes in order to use the common SelectFeatureWidget)
 */
public class AddRemoveElementToCalcUnitWidget extends AbstractDelegateWidget
{
  protected final KeyBasedDataModel m_dataModel;

  private final ToolTipRenderer m_toolTipRenderer = new ToolTipRenderer();

  private final SelectFeatureWidget m_selDelegateWidget;

  private IKalypsoFeatureTheme[] m_featureThemes;

  private final QName[] m_themeElementQNames = new QName[] { IPolyElement.QNAME, IElement1D.QNAME };

  private class AddElementToCalculationUnitWithPostCall extends AddElementToCalculationUnitCmd
  {
    public AddElementToCalculationUnitWithPostCall( final ICalculationUnit calculationUnit, final Feature[] elementsToAdd, final IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToAdd, model1d2d );
    }

    @Override
    public void process( ) throws Exception
    {
      super.process();
      KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    }
  }

  private class RemoveElementFromCalculationUnitWithPostCall extends RemoveElementFromCalculationUnitCmd
  {
    public RemoveElementFromCalculationUnitWithPostCall( final ICalculationUnit calculationUnit, final Feature[] elementsToRemove, final IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToRemove, model1d2d );
    }

    @Override
    public void process( ) throws Exception
    {
      super.process();
      KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    }
  }

  public AddRemoveElementToCalcUnitWidget( final KeyBasedDataModel dataModel )
  {
    super( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveElementToCalcUnitWidget.0" ), Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveElementToCalcUnitWidget.1" ), new SelectFeatureWidget( "", "", new QName[] { IPolyElement.QNAME, IElement1D.QNAME }, IFE1D2DElement.PROP_GEOMETRY ) ); //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$ //$NON-NLS-4$

    m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveElementToCalcUnitWidget.4" ) ); //$NON-NLS-1$
    final Color color = new Color( 1f, 1f, 0.6f, 0.70f );
    m_toolTipRenderer.setBackgroundColor( color );

    m_dataModel = dataModel;
    m_selDelegateWidget = (SelectFeatureWidget)getDelegate();
  }

  @Override
  public void keyPressed( final KeyEvent e )
  {
    if( e.getKeyChar() == KeyEvent.VK_ESCAPE )
      reinit();

    // Remove
    else if( e.getKeyChar() == KeyEvent.VK_DELETE )
    {
      removeElements();
    }
    // Add
    else if( e.getKeyCode() == KeyEvent.VK_INSERT )
    {
      addElements();
    }
    super.keyPressed( e );
  }

  protected void addElements( )
  {
    final IMapPanel mapPanel = getMapPanel();

    if( mapPanel == null )
      return;

    final Feature[] selectedFeatures = CalcUnitHelper.getSelectedFeature( mapPanel );
    final Object selectedWrapper = m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d)m_dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
    if( selectedWrapper instanceof ICalculationUnit )
    {
      final ICalculationUnit calUnit = (ICalculationUnit)selectedWrapper;
      final AddElementToCalculationUnitWithPostCall command = new AddElementToCalculationUnitWithPostCall( calUnit, selectedFeatures, model1d2d );
      KeyBasedDataModelUtil.postCommand( m_dataModel, command, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
    }
    getMapPanel().getSelectionManager().clear();
  }

  protected void removeElements( )
  {
    final IMapPanel mapPanel = getMapPanel();

    if( mapPanel == null )
      return;

    final Feature[] selectedFeatures = CalcUnitHelper.getSelectedFeature( mapPanel );
    final Object selectedWrapper = m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    final IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d)m_dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
    if( selectedWrapper instanceof ICalculationUnit )
    {
      final ICalculationUnit calUnit = (ICalculationUnit)selectedWrapper;
      final RemoveElementFromCalculationUnitWithPostCall command = new RemoveElementFromCalculationUnitWithPostCall( calUnit, selectedFeatures, model1d2d );
      KeyBasedDataModelUtil.postCommand( m_dataModel, command, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
    }

    getMapPanel().getSelectionManager().clear();
  }

  @Override
  public void clickPopup( final Point p )
  {
    // TODO: we should discuss, if we want this right-click popup behavior. Right now it is only used in the calcunit
    // widgets and no common kalypso style...

    final IMapPanel mapPanel = (IMapPanel)m_dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    final JPopupMenu popupMenu = new JPopupMenu();

    final JMenuItem addElement = new JMenuItem();
    addElement.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveElementToCalcUnitWidget.5" ) ); //$NON-NLS-1$
    addElement.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/add.gif" ) ) ); //$NON-NLS-1$
    addElement.addActionListener( makeAddElementActionListener() );

    final JMenuItem removeElement = new JMenuItem();
    removeElement.setText( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveElementToCalcUnitWidget.7" ) ); //$NON-NLS-1$
    removeElement.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/remove.gif" ) ) ); //$NON-NLS-1$
    removeElement.addActionListener( makeRemoveElementActionListener() );
    // popupMenu.add( addNameDescription );
    popupMenu.add( addElement );
    popupMenu.add( removeElement );
    popupMenu.show( (Component)mapPanel, p.x, p.y );
  }

  private ActionListener makeAddElementActionListener( )
  {
    final ActionListener al = new ActionListener()
    {

      @Override
      public void actionPerformed( final ActionEvent e )
      {
        addElements();
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

  private ActionListener makeRemoveElementActionListener( )
  {
    final ActionListener al = new ActionListener()
    {
      @Override
      public void actionPerformed( final ActionEvent e )
      {
        removeElements();
      }
    };
    return al;
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

      m_toolTipRenderer.setTooltip( Messages.getString( "org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit.AddRemoveElementToCalcUnitWidget.9" ) + delegateTooltip ); //$NON-NLS-1$

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