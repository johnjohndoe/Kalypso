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
package org.kalypso.kalypsomodel1d2d.ui.map.calculation_unit;

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFELine;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryConditionToCalcUnitCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveBoundaryConditionFromCalcUnitCommand;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 */
public class AddRemoveBoundaryConditionToCalcUnitWidget extends FENetConceptSelectionWidget
{
  private KeyBasedDataModel m_dataModel;

  public AddRemoveBoundaryConditionToCalcUnitWidget( final KeyBasedDataModel dataModel )
  {
    this( new QName[] { IFELine.QNAME, IBoundaryCondition.QNAME }, Messages.getString( "AddRemoveBoundaryConditionToCalcUnitWidget.0" ), Messages.getString( "AddRemoveBoundaryConditionToCalcUnitWidget.1" ), dataModel ); //$NON-NLS-1$ //$NON-NLS-2$
  }

  public AddRemoveBoundaryConditionToCalcUnitWidget( final QName[] names, final String name, final String toolTip, final KeyBasedDataModel dataModel )
  {
    super( names, name, toolTip );
    m_dataModel = dataModel;
  }

  @Override
  public void clickPopup( final Point p )
  {
    final MapPanel mapPanel = (MapPanel) m_dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    final JPopupMenu popupMenu = new JPopupMenu();

    final JMenuItem addBoundaryCondition = new JMenuItem();
    addBoundaryCondition.setText( Messages.getString( "AddRemoveBoundaryConditionToCalcUnitWidget.2" ) ); //$NON-NLS-1$
    addBoundaryCondition.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/add.gif" ) ) ); //$NON-NLS-1$
    addBoundaryCondition.addActionListener( makeAddBoundaryConditionListener() );

    final JMenuItem removeBoundaryCondition = new JMenuItem();
    removeBoundaryCondition.setText( Messages.getString( "AddRemoveBoundaryConditionToCalcUnitWidget.4" ) ); //$NON-NLS-1$
    removeBoundaryCondition.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/remove.gif" ) ) ); //$NON-NLS-1$
    removeBoundaryCondition.addActionListener( makeRemoveBoundaryConditionListener() );

    popupMenu.add( addBoundaryCondition );
    popupMenu.add( removeBoundaryCondition );

    final Feature[] selectedFeatures = getSelectedFeature();
    if( selectedFeatures != null && selectedFeatures.length > 0 )
    {
      boolean calcUnitContainsAllSelectedBCs = true;
      boolean calcUnitContainsAnyOfSelectedBCs = false;
      final ICalculationUnit calcUnit = (ICalculationUnit) m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
      final String calcUnitID = calcUnit.getGmlID();
      for( final Feature feature : selectedFeatures )
      {
        final IBoundaryCondition bc = (IBoundaryCondition) feature.getAdapter( IBoundaryCondition.class );
        if( bc == null )
          continue;
        calcUnitContainsAllSelectedBCs &= bc.getParentCalculationUnitIDs().contains( calcUnitID );
        calcUnitContainsAnyOfSelectedBCs |= bc.getParentCalculationUnitIDs().contains( calcUnitID );
      }
      addBoundaryCondition.setEnabled( !calcUnitContainsAnyOfSelectedBCs );
      removeBoundaryCondition.setEnabled( calcUnitContainsAllSelectedBCs );
    }
    else
    {
      addBoundaryCondition.setEnabled( false );
      removeBoundaryCondition.setEnabled( false );
    }

    popupMenu.show( mapPanel, p.x, p.y );
  }

  private ActionListener makeRemoveBoundaryConditionListener( )
  {
    final ActionListener al = new ActionListener()
    {
      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        final ICalculationUnit calUnit = (ICalculationUnit) m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        final IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) m_dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        if( selectedFeatures.length == 0 )
          return;
        for( final Feature feature : selectedFeatures )
        {
          final IBoundaryCondition bc = (IBoundaryCondition) feature.getAdapter( IBoundaryCondition.class );
          if( bc == null )
            return;
          final RemoveBoundaryConditionFromCalcUnitCommand command = new RemoveBoundaryConditionFromCalcUnitCommand( bc, calUnit, model1d2d )
          {
            /**
             * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryConditionToCalculationUnitCmd#process()
             */
            @Override
            public void process( ) throws Exception
            {
              super.process();
              KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
            }
          };

          if( command != null )
            KeyBasedDataModelUtil.postCommand( m_dataModel, command, ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE );
        }
        getMapPanel().getSelectionManager().clear();
      }
    };
    return al;
  }

  private ActionListener makeAddBoundaryConditionListener( )
  {
    final ActionListener al = new ActionListener()
    {
      @SuppressWarnings("unchecked")//$NON-NLS-1$
      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        if( selectedFeatures == null || selectedFeatures.length == 0 )
          return;
        final ICalculationUnit calcUnit = (ICalculationUnit) m_dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );

        for( final Feature feature : selectedFeatures )
        {
          final IBoundaryCondition bc = (IBoundaryCondition) feature.getAdapter( IBoundaryCondition.class );
          if( bc == null )
            continue;
          final AddBoundaryConditionToCalcUnitCommand command = new AddBoundaryConditionToCalcUnitCommand( calcUnit, bc )
          {
            /**
             * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryConditionToCalculationUnitCmd#process()
             */
            @Override
            public void process( ) throws Exception
            {
              super.process();
              KeyBasedDataModelUtil.resetCurrentEntry( m_dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
            }
          };

          if( command != null )
            KeyBasedDataModelUtil.postCommand( m_dataModel, command, ICommonKeys.KEY_BOUNDARY_CONDITION_CMD_WORKSPACE );
        }
        getMapPanel().getSelectionManager().clear();
      }

    };
    return al;
  }
}
