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
import java.awt.Polygon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.ImageIcon;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.xml.namespace.QName;

import org.kalypso.commons.command.ICommandTarget;
import org.kalypso.contribs.eclipse.core.runtime.PluginUtilities;
import org.kalypso.kalypsomodel1d2d.KalypsoModel1D2DPlugin;
import org.kalypso.kalypsomodel1d2d.schema.Kalypso1D2DSchemaConstants;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit1D2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.ICalculationUnit2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement1D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IElement2D;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DElement;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFE1D2DNode;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IFEDiscretisationModel1d2d;
import org.kalypso.kalypsomodel1d2d.schema.binding.discr.IPolyElement;
import org.kalypso.kalypsomodel1d2d.ui.map.DrawElements;
import org.kalypso.kalypsomodel1d2d.ui.map.cmds.calcunit.AddElementToCalculationUnit;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.ICommonKeys;
import org.kalypso.kalypsomodel1d2d.ui.map.facedata.KeyBasedDataModel;
import org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget;
import org.kalypso.ogc.gml.map.MapPanel;
import org.kalypsodeegree.graphics.transformation.GeoTransform;
import org.kalypsodeegree.model.feature.Feature;

/**
 * @author Patrice Congo
 * @author Madanagopal
 *
 */
public class AddElementToCalUnitWidget extends FENetConceptSelectionWidget
{
  private KeyBasedDataModel dataModel;
  
  private class AddElementToCalculationUnitWithPostCall extends AddElementToCalculationUnit
  {

    public AddElementToCalculationUnitWithPostCall( ICalculationUnit calculationUnit, IFE1D2DElement[] elementsToAdd, IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToAdd, model1d2d );
    }

    public AddElementToCalculationUnitWithPostCall( ICalculationUnit1D calculationUnit, IElement1D[] elementsToAdd, IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToAdd, model1d2d );
    }

    public AddElementToCalculationUnitWithPostCall( ICalculationUnit1D2D calculationUnit, IFE1D2DElement[] elementsToAdd, IFEDiscretisationModel1d2d model1d2d )
    {
      super( calculationUnit, elementsToAdd, model1d2d );
    }

    public AddElementToCalculationUnitWithPostCall( ICalculationUnit2D<IElement2D> calculationUnit, IElement2D[] elementsToAdd, IFEDiscretisationModel1d2d model1d2d )
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
      dataModel.setData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER, dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER ));
    }
  }

  public AddElementToCalUnitWidget( KeyBasedDataModel dataModel )
  {
      this(
          new QName[]{
              Kalypso1D2DSchemaConstants.WB1D2D_F_POLY_ELEMENT ,
              Kalypso1D2DSchemaConstants.WB1D2D_F_ELEMENT1D,
          },
          "Select Elements and add to the current calculation unit",
          "Select Elements and add to the current calculation unit", 
          dataModel );
      
  }
  
  protected AddElementToCalUnitWidget( 
              QName themeElementsQName, 
              String name, String toolTip,
              KeyBasedDataModel dataModel )
  {
    this( new QName[]{themeElementsQName}, name, toolTip,dataModel);
  }

  protected AddElementToCalUnitWidget( 
                  QName[] themeElementsQNames, 
                  String name, 
                  String toolTip,
                  KeyBasedDataModel dataModel)
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
    JPopupMenu popupMenu = new JPopupMenu();
    
    JMenuItem addElement = new JMenuItem();
    addElement.setText( "Add Element" );
    addElement.setIcon( new ImageIcon(PluginUtilities.findResource(
                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
                                  "icons/elcl16/add.gif" )));
    addElement.addActionListener( makeAddElementActionListener() );
   
//    JMenuItem removeElement = new JMenuItem();
//    removeElement.setText("Remove Element");
//    removeElement.setIcon( new ImageIcon(PluginUtilities.findResource(
//                                  KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
//                                  "icons/elcl16/remove.gif" )));
//    removeElement.addActionListener( 
//          makeRemoveElementActionListener());
    
    popupMenu.add( addElement);
//    popupMenu.add( removeElement);
//    popupMenu.addSeparator();
    
    popupMenu.show( mapPanel, p.x, p.y );    
  }
  
//  private ActionListener makeRemoveElementActionListener()
//  {
//    ActionListener al = new ActionListener()
//    {
//
//      public void actionPerformed( ActionEvent e )
//      {
//        final Feature[] selectedFeatures = getSelectedFeature();
//        Object selectedWrapper = dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
//        if( selectedWrapper instanceof ICalculationUnit )
//        {
//          ICalculationUnit calUnit = (ICalculationUnit)selectedWrapper;          
//        }
//      }
//      
//    };
//    return al;
//  }
  
  private ActionListener makeAddElementActionListener()
  {
    ActionListener al = new ActionListener()
    {

      public void actionPerformed( ActionEvent e )
      {
        final Feature[] selectedFeatures = getSelectedFeature();
        final Object selectedWrapper = dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
        IFEDiscretisationModel1d2d model1d2d = 
          (IFEDiscretisationModel1d2d) dataModel.getData( ICommonKeys.KEY_DISCRETISATION_MODEL );
        if( selectedWrapper instanceof ICalculationUnit )
        {
          ICalculationUnit calUnit = (ICalculationUnit)selectedWrapper;
          
          for(Feature feature:selectedFeatures)
          {
            AddElementToCalculationUnit command = null;
            IFE1D2DElement ele = (IFE1D2DElement) feature.getAdapter( IFE1D2DElement.class );
            if( calUnit instanceof ICalculationUnit1D &&
                ele instanceof IElement1D )
            {
              command = new AddElementToCalculationUnitWithPostCall(
                        (ICalculationUnit1D)calUnit, 
                        new IElement1D[]{(IElement1D)ele},
                        model1d2d);
              
            }
            else if( calUnit instanceof ICalculationUnit2D &&
                ele instanceof IElement2D )
            {
              command = new AddElementToCalculationUnitWithPostCall(
                        (ICalculationUnit2D)calUnit, 
                        new IElement2D[]{(IElement2D)ele},
                        model1d2d);
              
            }
            else if( calUnit instanceof ICalculationUnit1D2D &&
                      ele instanceof IFE1D2DElement )
            {
              command = new AddElementToCalculationUnitWithPostCall(
                        (ICalculationUnit1D2D)calUnit, 
                        new IFE1D2DElement[]{(IFE1D2DElement)ele},
                        model1d2d);
              
            }
            else
            {
              System.out.println("Bad constellation:"+feature+ " "+calUnit);
            }
            if( command != null )
            {
              ICommandTarget cmdTarget =
                (ICommandTarget)dataModel.getData( ICommonKeys.KEY_COMMAND_TARGET );
              
              cmdTarget.postCommand( command, null  );
            }
            

          }       
          
        }
      }
      
    };
    return al;
  }

  /**
   * @see org.kalypso.kalypsomodel1d2d.ui.map.select.FENetConceptSelectionWidget#paint(java.awt.Graphics)
   */
  @Override
  public void paint( Graphics g )
  {
    super.paint( g );
//    MapPanel mapPanel == null;
    
    DrawElements drawElements = new DrawElements();
    
    MapPanel mapPanel = getMapPanel();
    Object selectedWraper = 
      dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    if( selectedWraper instanceof ICalculationUnit2D )
    {
      drawElements.paint( 
          g, 
          mapPanel.getProjection(), 
          (ICalculationUnit2D)selectedWraper );
    }
  }
  
  
}
