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

import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.gmlschema.property.relation.IRelationType;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.ops.TypeInfo;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.flowrel.IBoundaryCondition;
import org.kalypso.kalypsomodel1d2d.schema.binding.model.IPseudoOPerationalModel;
import org.kalypso.kalypsomodel1d2d.ui.featureinput.AddMetaDataToFeatureDialog;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryConditionToCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.RemoveBoundaryConditionFromCalculationUnitCmd;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModelUtil;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.kalypsosimulationmodel.core.Util;
import org.kalypso.kalypsosimulationmodel.core.flowrel.IFlowRelationshipModel;
import org.kalypso.ogc.gml.command.DeleteFeatureCommand;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypso.ogc.gml.mapmodel.CommandableWorkspace;
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
    this( new QName[] { Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE, IBoundaryCondition.QNAME }, " Removes Boundary Condition ", " Removes Boundary Condition", dataModel );
    this.dataModel = dataModel;
  }

  public AddRemoveBoundaryConditionToCalUnitWidget( QName[] names, String name, String toolTip, KeyBasedDataModel dataModel )
  {
    super( names, name, toolTip );
  }

  @Override
  public void clickPopup( Point p )
  {
    MapPanel mapPanel = (MapPanel) dataModel.getData( ICommonKeys.KEY_MAP_PANEL );
    JPopupMenu popupMenu = new JPopupMenu();

    JMenuItem addNameDescription = new JMenuItem();
    addNameDescription.setText( "Add Metadata" );
    addNameDescription.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/add.gif" ) ) );
    addNameDescription.addActionListener( addNameDescriptionActionListener() );

    JMenuItem addBoundaryCondition = new JMenuItem();
    addBoundaryCondition.setText( "Add Boundary Condition" );
    addBoundaryCondition.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/add.gif" ) ) );
    addBoundaryCondition.addActionListener( makeAddBoundaryConditionListener() );

    JMenuItem editBoundaryCondition = new JMenuItem();
    editBoundaryCondition.setText( "Edit Boundary Condition" );
    editBoundaryCondition.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/edit.gif" ) ) );
    editBoundaryCondition.addActionListener( makeEditBoundaryConditionListener() );

    JMenuItem removeBoundaryCondition = new JMenuItem();
    removeBoundaryCondition.setText( "Remove Boundary Condition from calculation unit" );
    removeBoundaryCondition.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/remove.gif" ) ) );
    removeBoundaryCondition.addActionListener( makeRemoveBoundaryConditionListener() );

    JMenuItem deleteBoundaryCondition = new JMenuItem();
    deleteBoundaryCondition.setText( "Delete Boundary Condition from model" );
    deleteBoundaryCondition.setIcon( new ImageIcon( PluginUtilities.findResource( KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(), "icons/elcl16/remove.gif" ) ) );
    deleteBoundaryCondition.addActionListener( makeDeleteBoundaryConditionFromModelListener() );

    popupMenu.add( addNameDescription );
    popupMenu.add( addBoundaryCondition );
    popupMenu.add( editBoundaryCondition );
    popupMenu.add( removeBoundaryCondition );
    popupMenu.add( deleteBoundaryCondition );
    popupMenu.show( mapPanel, p.x, p.y );

  }

  private ActionListener addNameDescriptionActionListener( )
  {
    ActionListener al = new ActionListener()
    {
      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        for( Feature feature : selectedFeatures )
        {
          final IBoundaryCondition bc = (IBoundaryCondition) feature.getAdapter( IBoundaryCondition.class );
          if( bc == null )
          {
            System.out.println( "could not adapt to boundary " );
            return;
          }

          final Display display = (Display) dataModel.getData( ICommonKeys.KEY_SELECTED_DISPLAY );
          Runnable runnable = new Runnable()
          {

            public void run( )
            {
              Shell shell = display.getActiveShell();
              final AddMetaDataToFeatureDialog setFeatureDialog = new AddMetaDataToFeatureDialog( shell );

              setFeatureDialog.open();

              if( setFeatureDialog.getName() != null )
              {
                bc.setName( setFeatureDialog.getName() );

              }

              if( setFeatureDialog.getDescription() != null )
              {
                bc.setDescription( setFeatureDialog.getDescription() );

              }
            }
          };
          display.asyncExec( runnable );
        }
      }
    };
    return al;
  }

  private ActionListener makeDeleteBoundaryConditionFromModelListener( )
  {
    ActionListener al = new ActionListener()
    {
      @SuppressWarnings("unchecked")
      public void actionPerformed( ActionEvent e )
      {
        final CommandableWorkspace cmdWorkspace = Util.getCommandableWorkspace( IPseudoOPerationalModel.class );// KeyBasedDataModelUtil.getBCWorkSpace(
        // dataModel
        // );
        final Feature parentFeature = cmdWorkspace.getRootFeature();
        final IFlowRelationshipModel bcHolder = (IFlowRelationshipModel) parentFeature.getAdapter( IFlowRelationshipModel.class );
        final IRelationType relationType = bcHolder.getWrappedList().getParentFeatureTypeProperty();

        final Feature[] selectedFeatures = getSelectedFeature();

        for( Feature feature : selectedFeatures )
        {
          if( TypeInfo.isBoundaryCondition( feature ) )
          {
            DeleteFeatureCommand delCmd = new DeleteFeatureCommand( cmdWorkspace, parentFeature, relationType, feature )
            {
              /**
               * @see org.kalypso.ogc.gml.command.DeleteFeatureCommand#process()
               */
              @Override
              public void process( ) throws Exception
              {
                super.process();
                KeyBasedDataModelUtil.resetCurrentEntry( dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
                KeyBasedDataModelUtil.repaintMapPanel( dataModel, ICommonKeys.KEY_MAP_PANEL );
              }
            };
            KeyBasedDataModelUtil.postCommand( dataModel, delCmd );
          }
        }
      }
    };
    return al;
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
        final ICalculationUnit calUnit = (ICalculationUnit) dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        if( selectedFeatures.length == 0 )
        {
          System.out.println( "Please select at least one boundary condition" );
          return;
        }
        for( Feature feature : selectedFeatures )
        {
          final IBoundaryCondition bc = (IBoundaryCondition) feature.getAdapter( IBoundaryCondition.class );
          if( bc == null )
          {
            System.out.println( "could not adapt to boundary " );
            return;
          }

          final RemoveBoundaryConditionFromCalculationUnitCmd command = new RemoveBoundaryConditionFromCalculationUnitCmd( bc, calUnit, model1d2d, getGrabDistance() )
          {
            /**
             * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryConditionToCalculationUnitCmd#process()
             */
            @Override
            public void process( ) throws Exception
            {
              super.process();
              KeyBasedDataModelUtil.resetCurrentEntry( dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
            }
          };

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
        final ICalculationUnit calUnit = (ICalculationUnit) dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        IFEDiscretisationModel1d2d model1d2d = (IFEDiscretisationModel1d2d) dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );

        if( selectedFeatures.length == 0 )
        {
          System.out.println( "Please select at least one boundary condition" );
          return;
        }

        for( Feature feature : selectedFeatures )
        {
          final IBoundaryCondition bc = (IBoundaryCondition) feature.getAdapter( IBoundaryCondition.class );
          if( bc == null )
          {
            System.out.println( "could not adapt to boundary " );
            return;
          }

          final AddBoundaryConditionToCalculationUnitCmd command = new AddBoundaryConditionToCalculationUnitCmd( calUnit, bc, model1d2d, getGrabDistance() )
          {
            /**
             * @see org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddBoundaryConditionToCalculationUnitCmd#process()
             */
            @Override
            public void process( ) throws Exception
            {
              super.process();
              KeyBasedDataModelUtil.resetCurrentEntry( dataModel, ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
            }
          };

          if( command != null )
          {
            KeyBasedDataModelUtil.postCommand( dataModel, command );
          }
        }
      }

    };
    return al;
  }
}
