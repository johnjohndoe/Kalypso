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

import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.TabFolder;
import org.eclipse.swt.widgets.TabItem;
import org.kalypso.gmlschema.feature.IFeatureType;
import org.kalypso.ogc.gml.KalypsoUserStyle;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.AbstractSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.LineSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.PointSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.PolygonSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.RasterSymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.SymbolizerLayout;
import org.kalypso.ui.editor.styleeditor.symbolizerLayouts.TextSymbolizerLayout;
import org.kalypsodeegree.filterencoding.FilterEvaluationException;
import org.kalypsodeegree.graphics.sld.LineSymbolizer;
import org.kalypsodeegree.graphics.sld.PointSymbolizer;
import org.kalypsodeegree.graphics.sld.PolygonSymbolizer;
import org.kalypsodeegree.graphics.sld.RasterSymbolizer;
import org.kalypsodeegree.graphics.sld.Symbolizer;
import org.kalypsodeegree.graphics.sld.TextSymbolizer;

/**
 * @author F.Lindemann
 *  
 */
public class SymbolizerTabItemBuilder
{

  public SymbolizerTabItemBuilder( TabFolder tabFolder, Symbolizer symbolizer, KalypsoUserStyle userStyle,
      IFeatureType featureType )
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
      tabItem.setText( "Polygon" ); //$NON-NLS-1$
      symbolizerLayout = new PolygonSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof PointSymbolizer )
    {
      tabItem.setText( "Point" ); //$NON-NLS-1$
      symbolizerLayout = new PointSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof LineSymbolizer )
    {
      tabItem.setText( "Line" ); //$NON-NLS-1$
      symbolizerLayout = new LineSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof RasterSymbolizer )
    {
      tabItem.setText( "Raster" ); //$NON-NLS-1$
      symbolizerLayout = new RasterSymbolizerLayout( composite, symbolizer, userStyle );
    }
    else if( symbolizer instanceof TextSymbolizer )
    {
      tabItem.setText( "Text" ); //$NON-NLS-1$
      symbolizerLayout = new TextSymbolizerLayout( composite, symbolizer, userStyle, featureType );
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