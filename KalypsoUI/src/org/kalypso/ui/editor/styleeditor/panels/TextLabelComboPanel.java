/*--------------- Kalypso-Header --------------------------------------------------------------------

 This file is part of kalypso.
 Copyright (C) 2004, 2005 by:

 Technical University Hamburg-Harburg (TUHH)
 Institute of River and coastal engineering
 Denickestr. 22
 21073 Hamburg, Germany
 http://www.tuhh.de/wb

 and
 
 Bjoernsen Consulting Engineers (BCE)
 Maria Trost 3
 56070 Koblenz, Germany
 http://www.bjoernsen.de

 This library is free software; you can redistribute it and/or
 modify it under the terms of the GNU Lesser General Public
 License as published by the Free Software Foundation; either
 version 2.1 of the License, or (at your option) any later version.

 This library is distributed in the hope that it will be useful,
 but WITHOUT ANY WARRANTY; without even the implied warranty of
 MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 Lesser General Public License for more details.

 You should have received a copy of the GNU Lesser General Public
 License along with this library; if not, write to the Free Software
 Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

 Contact:

 E-Mail:
 belger@bjoernsen.de
 schlienger@bjoernsen.de
 v.doemming@tuhh.de
  
---------------------------------------------------------------------------------------------------*/
/*
 * Created on 15.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor.panels;

import java.util.ArrayList;

import org.deegree.model.feature.FeatureType;
import org.deegree.model.feature.FeatureTypeProperty;
import org.eclipse.swt.widgets.Composite;

/**
 * @author F.Lindemann
 *  
 */
public class TextLabelComboPanel extends ComboPanel
{

  public TextLabelComboPanel( Composite parent, String label, FeatureType featureType, String value )
  {
    super( parent, label );
    // read possible items to get the label text from
    ArrayList labelStringItems = new ArrayList();
    FeatureTypeProperty[] ftp = featureType.getProperties();
    for( int i = 0; i < ftp.length; i++ )
      if( !ftp[i].getType().startsWith( "org.deegree.model.geometry." ) )
        labelStringItems.add( ftp[i].getName() );
    items = new String[labelStringItems.size()];
    for( int j = 0; j < items.length; j++ )
      items[j] = (String)labelStringItems.get( j );
    init();
    comboBox.setText( "..." );
    if( value != null )
    {
      for( int m = 0; m < items.length; m++ )
        if( items[m].equals( value ) )
        {
          comboBox.select( m );
          break;
        }
    }
  }

  public String getSelectedFeatureTypeProperty()
  {
    return items[getSelection()];
  }

  // sets the comboBox to a default state
  public void reset()
  {
    comboBox.setText( "..." );
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#getSelection()
   */
  public int getSelection()
  {
    return selection_index;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.kalypso.ui.editor.styleeditor.panels.StrokeComboPanel#setSelection(int)
   */
  public void setSelection( int index )
  {
    selection_index = index;
  }
}