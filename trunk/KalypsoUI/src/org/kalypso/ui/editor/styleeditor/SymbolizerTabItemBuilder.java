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
 * Created on 12.07.2004
 *  
 */
package org.kalypso.ui.editor.styleeditor;

import org.deegree.filterencoding.FilterEvaluationException;
import org.deegree.graphics.sld.LineSymbolizer;
import org.deegree.graphics.sld.PointSymbolizer;
import org.deegree.graphics.sld.PolygonSymbolizer;
import org.deegree.graphics.sld.RasterSymbolizer;
import org.deegree.graphics.sld.Symbolizer;
import org.deegree.graphics.sld.TextSymbolizer;
import org.deegree.model.feature.FeatureType;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.AbstractSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.LineSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.PointSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.PolygonSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.RasterSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.SymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.TextSymbolizerLayout;

/**
 * @author F.Lindemann
 *  
 */
public class SymbolizerTabItemBuilder
{

  public SymbolizerTabItemBuilder( TabFolder tabFolder, Symbolizer symbolizer,
      KalypsoUserStyle userStyle, FeatureType featureType )
  {
    TabItem tabItem = new TabItem( tabFolder, SWT.NULL );

    Composite composite = new Composite( tabFolder, SWT.NULL );
    GridLayout compositeLayout = new GridLayout();
    composite.setLayout( compositeLayout );
    compositeLayout.marginWidth = 0;
    compositeLayout.marginHeight = 0;
    composite.layout();
    tabItem.setControl( composite );

    AbstractSymbolizerLayout symbolizerLayout = null;

    if( symbolizer == null )
    {
      symbolizerLayout = new SymbolizerLayout( composite );
    }
    else if( symbolizer instanceof PolygonSymbolizer )
    {
      tabItem.setText( "Polygon" );
      symbolizerLayout = new PolygonSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof PointSymbolizer )
    {
      tabItem.setText( "Point" );
      symbolizerLayout = new PointSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof LineSymbolizer )
    {
      tabItem.setText( "Line" );
      symbolizerLayout = new LineSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof RasterSymbolizer )
    {
      tabItem.setText( "Raster" );
      symbolizerLayout = new RasterSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof TextSymbolizer )
    {
      tabItem.setText( "Text" );
      symbolizerLayout = new TextSymbolizerLayout( composite, symbolizer, userStyle, featureType);
    }

    if( symbolizerLayout != null )
    {
      try
      {
        symbolizerLayout.draw();
      }
      catch( FilterEvaluationException e )
      {
        e.printStackTrace();
      }
    }

  }
}