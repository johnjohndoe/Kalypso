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
import java.util.ArrayList;
import java.util.List;

import javax.swing.Action;
import javax.swing.ImageIcon;
import javax.swing.JMenu;
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
 *
 */
public class AlterCalUnitBorderWidget extends FENetConceptSelectionWidget
{
  private static final String SEPARATOR_PSEUDO_TEXT = "_separator_pseudo_text_";
  private static final String ICONS_ELCL16_REMOVE_GIF = "icons/elcl16/remove.gif";
  private static final String ICONS_ELCL16_ADD_GIF = "icons/elcl16/add.gif";
  private static final String TXT_REMOVE_BOUNDARY_LINE_UP_STREAM = "Remove boundary line";
  private static final String TXT_ADD_BOUNDARY_LINE_UP_STREAM = "Add Boundary Line";

  private static final String[][] MENU_ITEM_SPECS = {
      {TXT_ADD_BOUNDARY_LINE_UP_STREAM, ICONS_ELCL16_ADD_GIF},
      {TXT_REMOVE_BOUNDARY_LINE_UP_STREAM, ICONS_ELCL16_REMOVE_GIF}};
  
  
  private KeyBasedDataModel dataModel;
  private JPopupMenu popupMenu;
  private List<JMenuItem> items = new ArrayList<JMenuItem>();
  

  public AlterCalUnitBorderWidget( KeyBasedDataModel dataModel )
  {
      this(
          new QName[]{
              Kalypso1D2DSchemaConstants.WB1D2D_F_BOUNDARY_LINE
          },
          "Select Elements and add to the current calculation unit",
          "Select Elements and add to the current calculation unit", 
          dataModel );      
  }
  
  protected AlterCalUnitBorderWidget( 
              QName themeElementsQName, 
              String name, String toolTip,
              KeyBasedDataModel dataModel )
  {
    this( new QName[]{themeElementsQName}, name, toolTip,dataModel);
  }

  protected AlterCalUnitBorderWidget( 
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
    if( popupMenu==null )
    {
      popupMenu = createMenu();
    }
    updateMenuItem();
    popupMenu.show( mapPanel, p.x, p.y );    
  }
  
  private ImageIcon getImageIcon(String pluginRelativPath )
  {
    ImageIcon imageIcon = new ImageIcon(PluginUtilities.findResource(
        KalypsoModel1D2DPlugin.getDefault().getBundle().getSymbolicName(),
        pluginRelativPath ) );
    return imageIcon;
  }
  
  private ActionListener makeActionListener()
  {
    ActionListener al = new ActionListener()
    {

      public void actionPerformed( ActionEvent e )
      {
        Object source = e.getSource();
        if(!( source instanceof JMenuItem ) )
        {
          return;
        };
        doMenuAction( ((JMenuItem)source).getText() );
        
      }
      
    };
    return al;
  }
  
  private void updateMenuItem()
  {
    for(JMenuItem item:items )
    {
      final String text = item.getText();
      if(TXT_ADD_BOUNDARY_LINE_UP_STREAM.equals( text ))
      {
        updateAddUpStreamMenu(item);
      }
      else if(TXT_REMOVE_BOUNDARY_LINE_UP_STREAM.equals( text ))
      {
        updateRemoveUpStreamMenu( item );
      }
      else
      {
        
      }
    }
  }
  
  private void updateAddUpStreamMenu( JMenuItem item )
  {
    updateGeneraBadSelection( item );
  }
  
  private void updateRemoveUpStreamMenu( JMenuItem item )
  {
    updateGeneraBadSelection( item );
  }
  
  private void updateGeneraBadSelection( JMenuItem item )
  {
    Feature[] selectedFeature = getSelectedFeature();
    item.setEnabled( true );
    
    if( selectedFeature== null )
    {
      item.setEnabled( false );
    }
    if( selectedFeature.length != 1 )
    {
      item.setEnabled( false );
    }
  }

  private void doMenuAction( String text )
  {
    if( text == null )
    {
      return;
    }
    else if( TXT_ADD_BOUNDARY_LINE_UP_STREAM.equals( text ) )
    {
      
    }
    else
    {
      System.out.println("Not supported menu action:"+text);
    }
    final Feature[] selectedFeatures = getSelectedFeature();
    Object selectedWrapper = dataModel.getData( ICommonKeys.KEY_SELECTED_FEATURE_WRAPPER );
    if( selectedWrapper instanceof ICalculationUnit )
    {
      ICalculationUnit calUnit = (ICalculationUnit)selectedWrapper;          
    }
  }
  
  synchronized private JPopupMenu createMenu()
  {
    
    JPopupMenu menu = new JPopupMenu();
    final ActionListener actionListener = makeActionListener();
    
    
    for( String[] spec : MENU_ITEM_SPECS )
    {
      if( spec.length != 2 )
      {
        throw new RuntimeException(
            "Spec must have length 2, but has:"+spec.length );
      }
      String text = spec[0];
      
      if( SEPARATOR_PSEUDO_TEXT.equals(text) )
      {
        menu.addSeparator();
      }
      else
      {
        JMenuItem addElement = new JMenuItem();
        addElement.setText( text );
        addElement.setIcon( getImageIcon( spec[1] ));
        addElement.addActionListener( actionListener );
        menu.add( addElement );
        items.add( addElement );
      }
    }
    
    
    return menu;
  }
 
}
