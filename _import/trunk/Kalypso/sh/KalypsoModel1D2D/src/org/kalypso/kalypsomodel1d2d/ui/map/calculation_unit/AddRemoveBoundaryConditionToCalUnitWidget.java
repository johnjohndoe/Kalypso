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

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.xml.namespace.QName;

import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.LinksOps;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryConditionToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveBoundaryConditionFromCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Madanagopal
 *
 */
public class AddRemoveBoundaryConditionToCalUnitWidget extends FENetConceptSelectionWidget
{
  private KeyBasedDataModel dataModel;
  
  
  public AddRemoveBoundaryConditionToCalUnitWidget( KeyBasedDataModel dataModel )
  {
    this(new QName[]{
        Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE,
        IBoundaryCondition.QNAME,
    },
    " Removes Boundary Condition ",
    " Removes Boundary Condition", 
    dataModel );
    this.dataModel = dataModel;    
  }
  
  public AddRemoveBoundaryConditionToCalUnitWidget(
      QName[] names, String name, String toolTip, KeyBasedDataModel dataModel )
  {
    super(names,name,toolTip);
  }

  @Override
  public void clickPopup(Point p){
    MapPanel mapPanel = (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    JPopupMenu popupMenu = new JPopupMenu();
    
    JMenuItem addBoundaryCondition = new JMenuItem();
    addBoundaryCondition.setText( "Add Boundary Condition" );
    addBoundaryCondition.setIcon( new ImageIcon(PluginUtilities.findResource(
                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
                                  "icons/elcl16/add.gif" )));
    addBoundaryCondition.addActionListener( makeAddBoundaryConditionListener() );
    
    JMenuItem editBoundaryCondition = new JMenuItem();
    editBoundaryCondition.setText( "Edit Boundary Condition" );
    editBoundaryCondition.setIcon( new ImageIcon(PluginUtilities.findResource(
                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
                                  "icons/elcl16/edit.gif" )));
    editBoundaryCondition.addActionListener( makeEditBoundaryConditionListener() );
    
    JMenuItem removeBoundaryCondition = new JMenuItem();
    removeBoundaryCondition.setText( "Remove Boundary Condition" );
    removeBoundaryCondition.setIcon( new ImageIcon(PluginUtilities.findResource(
                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
                                  "icons/elcl16/remove.gif" )));
    removeBoundaryCondition.addActionListener( makeRemoveBoundaryConditionListener() );
    
    popupMenu.add( addBoundaryCondition);
    popupMenu.add( editBoundaryCondition);
    popupMenu.add( removeBoundaryCondition);
    popupMenu.show( mapPanel, p.x, p.y );  
    
  }

  private ActionListener makeEditBoundaryConditionListener( )
  {     
    return null;

  }

  private ActionListener makeRemoveBoundaryConditionListener( )
  {
    ActionListener al = new ActionListener()
    {  
      @SuppressWarnings("unchecked")
      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        Object selectedWrapper = dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        IFEDiscretisationModel1d2d model1d2d = 
          (IFEDiscretisationModel1d2d) dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        if( selectedWrapper instanceof ICalculationUnit )
        {
          ICalculationUnit calUnit = (ICalculationUnit)selectedWrapper;
          RemoveBoundaryConditionFromCalculationUnitCmd command = null;
          for(Feature feature:selectedFeatures)
          {           
            IFE1D2DElement ele = (IFE1D2DElement) feature.getAdapter( IFE1D2DElement.class );
            LinksOps.delRelationshipElementAndComplexElement( ele, calUnit );
            
            if( calUnit instanceof ICalculationUnit1D &&
                ele instanceof IBoundaryCondition )
            {
              command = new RemoveBoundaryConditionFromCalculationUnitCmd(
                        (IBoundaryCondition)ele,
                        (ICalculationUnit1D)calUnit,                   
                        model1d2d);
            }
            else if( calUnit instanceof ICalculationUnit2D &&
                ele instanceof IBoundaryCondition )
            {
              command = new RemoveBoundaryConditionFromCalculationUnitCmd(
                        (IBoundaryCondition)ele,
                        (ICalculationUnit2D)calUnit,                   
                        model1d2d);
            }
            else if( calUnit instanceof ICalculationUnit1D2D &&
                ele instanceof IBoundaryCondition)
            {
              command = new RemoveBoundaryConditionFromCalculationUnitCmd(
                        (IBoundaryCondition)ele,
                        (ICalculationUnit1D2D)calUnit,                   
                        model1d2d);
            }            
          }           
          if( command != null )
          {
            KeyBasedDataModelUtil.postCommand( dataModel, command );
          }
        }
      }      
    };
    return al;
  }
  

  private ActionListener makeAddBoundaryConditionListener( )
  {
    ActionListener al = new ActionListener()
    {
      
      @SuppressWarnings("unchecked")
      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        final Object selectedWrapper = dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        IFEDiscretisationModel1d2d model1d2d = 
          (IFEDiscretisationModel1d2d) dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        if( selectedWrapper instanceof IBoundaryCondition )
        {
          IBoundaryCondition calUnit = (IBoundaryCondition) selectedWrapper;
          
          for(Feature feature:selectedFeatures)
          {
            AddBoundaryConditionToCalculationUnitCmd command = null;
            IFE1D2DElement ele = (IFE1D2DElement) feature.getAdapter( IFE1D2DElement.class );
            if( calUnit instanceof ICalculationUnit1D &&
                ele instanceof IBoundaryCondition )
            {
              command = new AddBoundaryConditionToCalculationUnitCmd(
                  (ICalculationUnit1D)calUnit, 
                  (IBoundaryCondition)ele,
                  model1d2d,
                  IBoundaryCondition.QNAME);
              
            }
            else if( calUnit instanceof ICalculationUnit2D &&
                ele instanceof IBoundaryCondition )
            {
              command = new AddBoundaryConditionToCalculationUnitCmd(
                  (ICalculationUnit2D)calUnit, 
                  (IBoundaryCondition)ele,
                  model1d2d,
                  IBoundaryCondition.QNAME);
              
            }
            else if( calUnit instanceof ICalculationUnit1D2D &&
                ele instanceof IBoundaryCondition )
            {
              command = new AddBoundaryConditionToCalculationUnitCmd(
                  (ICalculationUnit1D2D)calUnit, 
                  (IBoundaryCondition)ele,
                  model1d2d,
                  IBoundaryCondition.QNAME);              
            }
            else
            {
              System.out.println("Bad constellation:"+feature+ " "+calUnit);
            }
            if( command != null )
            {
              KeyBasedDataModelUtil.postCommand( dataModel, command );
            }            
          }       
        }
      }
    };
    return al;

  }
  

}
