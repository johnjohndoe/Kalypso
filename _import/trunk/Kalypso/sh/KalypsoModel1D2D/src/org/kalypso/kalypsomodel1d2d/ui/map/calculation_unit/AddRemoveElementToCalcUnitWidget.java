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

import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddElementToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveElementFromCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Patrice Congo
 * @author Madanagopal
 * 
 */
public class AddRemoveElementToCalcUnitWidget extends FENetConceptSelectionWidget
{
  private final KeyBasedDataModel m_dataModel;

  private class AddElementToCalculationUnitWithPostCall extends AddElementToCalculationUnitCmd
  {

    public AddElementToCalculationUnitWithPostCall( final ICalculationUnit calculationUnit, final IFE1D2DElement[] elementsToAdd, final IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToAdd, model1d2d );
    }

    public AddElementToCalculationUnitWithPostCall( final ICalculationUnit calculationUnit, final Feature[] elementsToAdd, final IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToAdd, model1d2d );
    }

    /**
     * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddElementToCalculationUnit#process()
     */
    @Override
    public void process( ) throws Exception
    {
      super.process();
      KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
      KeyBasedDataModelUtil.repaintMapPanel( m_dataModel, ICommonKeys.KEY_MAP_PANEL );

    }
  }

  private class RemoveElementFromCalculationUnitWithPostCall extends RemoveElementFromCalculationUnitCmd
  {

    public RemoveElementFromCalculationUnitWithPostCall( final ICalculationUnit calculationUnit, final IFE1D2DElement[] elementsToRemove, final IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToRemove, model1d2d );
    }

    public RemoveElementFromCalculationUnitWithPostCall( final ICalculationUnit calculationUnit, final Feature[] elementsToRemove, final IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToRemove, model1d2d );
    }

    /**
     * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddElementToCalculationUnit#process()
     */
    @Override
    public void process( ) throws Exception
    {
      super.process();
      KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
      KeyBasedDataModelUtil.repaintMapPanel( m_dataModel, ICommonKeys.KEY_MAP_PANEL );
    }
  }

  public AddRemoveElementToCalcUnitWidget( final KeyBasedDataModel dataModel )
  {
    this( new QName[] { Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT, Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D, }, Messages.getString("AddRemoveElementToCalcUnitWidget.0"), Messages.getString("AddRemoveElementToCalcUnitWidget.1"), dataModel ); //$NON-NLS-1$ //$NON-NLS-2$

  }

  protected AddRemoveElementToCalcUnitWidget( final QName themeElementsQName, final String name, final String toolTip, final KeyBasedDataModel dataModel )
  {
    this( new QName[] { themeElementsQName }, name, toolTip, dataModel );
  }

  protected AddRemoveElementToCalcUnitWidget( final QName[] themeElementsQNames, final String name, final String toolTip, final KeyBasedDataModel dataModel )
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
    final JPopupMenu popupMenu = new JPopupMenu();

    final JMenuItem addElement = new JMenuItem();
    addElement.setText( Messages.getString("AddRemoveElementToCalcUnitWidget.2") ); //$NON-NLS-1$
    addElement.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/add.gif" ) ) ); //$NON-NLS-1$
    addElement.addActionListener( makeAddElementActionListener() );

    final JMenuItem removeElement = new JMenuItem();
    removeElement.setText( Messages.getString("AddRemoveElementToCalcUnitWidget.6") ); //$NON-NLS-1$
    removeElement.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/remove.gif" ) ) ); //$NON-NLS-1$
    removeElement.addActionListener( makeRemoveElementActionListener() );
    // popupMenu.add( addNameDescription );
    popupMenu.add( addElement );
    popupMenu.add( removeElement );
    popupMenu.show( mapPanel, p.x, p.y );
  }

  private ActionListener makeAddElementActionListener( )
  {
    final ActionListener al = new ActionListener()
    {

      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        final Object selectedWrapper = m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) m_dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        if( selectedWrapper instanceof ICalculationUnit )
        {
          ICalculationUnit calUnit = (ICalculationUnit) selectedWrapper;
          AddElementToCalculationUnitWithPostCall command = new AddElementToCalculationUnitWithPostCall( calUnit, selectedFeatures, model1d2d );
          KeyBasedDataModelUtil.postCommand( m_dataModel, command, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );
        }
      }
    };
    return al;
  }

  private ActionListener makeRemoveElementActionListener( )
  {
    final ActionListener al = new ActionListener()
    {
      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        Object selectedWrapper = m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) m_dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        if( selectedWrapper instanceof ICalculationUnit )
        {
          ICalculationUnit calUnit = (ICalculationUnit) selectedWrapper;
          RemoveElementFromCalculationUnitWithPostCall command = new RemoveElementFromCalculationUnitWithPostCall( calUnit, selectedFeatures, model1d2d );
          KeyBasedDataModelUtil.postCommand( m_dataModel, command, ICommonKeys.KEY_COMMAND_MANAGER_DISC_MODEL );

        }
      }
    };
    return al;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( final Graphics g )
  {
    super.paint( g );
  }
}
