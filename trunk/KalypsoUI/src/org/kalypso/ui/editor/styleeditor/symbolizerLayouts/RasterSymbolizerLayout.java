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
package org.kalypso.ui.editor.styleeditor.symbolizerLayouts;

import org.deegree.graphics.sld.RasterSymbolizer;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.model.feature.event.ModellEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.MessageBundle;
import org.kalypso.ui.editor.styleeditor.panels.ModeSelectionComboPanel;
import org.kalypso.ui.editor.styleeditor.panels.PanelEvent;
import org.kalypso.ui.editor.styleeditor.panels.PanelListener;

/**
 * @author F.Lindemann
 *  
 */

public class RasterSymbolizerLayout extends AbstractSymbolizerLayout
{

  public RasterSymbolizerLayout( Composite m_composite, Symbolizer m_symbolizer,
      KalypsoUserStyle m_userStyle )
  {
    super( m_composite, m_symbolizer, m_userStyle );
  }

  public void draw()
  {
    final RasterSymbolizer rasterSymbolizer = (RasterSymbolizer)symbolizer;

    GridLayout compositeLayout = new GridLayout();
    compositeLayout.marginHeight = 2;

    // ***** Label Group
    /*
     * Group labelGroup = new Group( composite, SWT.NULL ); GridData
     * labelGroupData = new GridData(); labelGroupData.widthHint = 210;
     * labelGroupData.heightHint = 246; labelGroup.setLayoutData( labelGroupData );
     * labelGroup.setLayout( compositeLayout ); labelGroup.layout();
     * 
     * Label label = new Label( labelGroup, SWT.NULL ); label.setText(
     * MessageBundle.STYLE_EDITOR_ERROR_NO_IMPLEMENTATION );
     */

    // ***** ColorMap Group
    Group colorMapGroup = new Group( composite, SWT.NULL );
    GridData colorMapGroupData = new GridData();
    colorMapGroupData.widthHint = 210;
    colorMapGroupData.heightHint = 246;
    colorMapGroup.setLayoutData( colorMapGroupData );
    colorMapGroup.setLayout( compositeLayout );
    colorMapGroup.layout();
    colorMapGroup.setText( MessageBundle.STYLE_EDITOR_COLORMAP );

    // ***** ComboBox Mode Panel

    ModeSelectionComboPanel modeComboPanel = new ModeSelectionComboPanel( colorMapGroup, "Mode:",
        rasterSymbolizer.getMode() );
    modeComboPanel.addPanelListener( new PanelListener()
    {
      public void valueChanged( PanelEvent event )
      {
        int mode = ( (ModeSelectionComboPanel)event.getSource() ).getSelection();
        rasterSymbolizer.setMode( mode );
        userStyle.fireModellEvent( new ModellEvent( userStyle, ModellEvent.STYLE_CHANGE ) );
      }
    } );

    // ***** Table
  }
}