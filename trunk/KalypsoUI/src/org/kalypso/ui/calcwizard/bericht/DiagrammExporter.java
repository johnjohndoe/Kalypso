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
package org.kalypso.ui.calcwizard.bericht;

import java.awt.Color;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.net.URL;
import java.net.URLConnection;
import java.util.Properties;

import org.deegree.model.feature.Feature;
import org.kalypso.java.io.ReaderUtilities;
import org.kalypso.ogc.sensor.diagview.DiagView;
import org.kalypso.ogc.sensor.diagview.DiagViewUtils;
import org.kalypso.ogc.sensor.diagview.jfreechart.ExportableChart;
import org.kalypso.ogc.sensor.diagview.jfreechart.ObservationChart;
import org.kalypso.template.obsdiagview.ObsdiagviewType;
import org.kalypso.ui.calcwizard.Arguments;
import org.kalypso.util.url.UrlResolver;

/**
 * @author belger
 */
public class DiagrammExporter extends AbstractBerichtExporter
{
  private static final String EXT = ".jpg";

  /**
   * @see org.kalypso.ui.calcwizard.bericht.IBerichtExporter#export(org.deegree.model.feature.Feature, java.io.OutputStream)
   */
  public void export( final Feature feature, final OutputStream os ) throws Exception
  {
    // parse arguments:
    final Arguments arguments = getArguments();

    // - templatefile
    final String templateurl = arguments.getProperty( "template", null );
    
    final int width = Integer.parseInt( arguments.getProperty( "width" , "800" ) );
    final int height = Integer.parseInt( arguments.getProperty( "height" , "600" ) );
    
    // - replacetokens / featureprops
    final Arguments tokens = arguments.getArguments( "tokens" );
    final Properties replacetokens = ExporterHelper.createReplaceTokens( feature, tokens );
    
    final URL url = new UrlResolver().resolveURL( getContext(), templateurl );
    final URLConnection connection = url.openConnection();
    final Reader reader = new InputStreamReader( connection.getInputStream(), "UTF-8" );
    final Reader reader2 = ReaderUtilities.createTokenReplaceReader( reader, replacetokens, '%', '%' );
    
    final ObsdiagviewType xml = DiagViewUtils.loadDiagramTemplateXML( reader2 );
    
    final DiagView tpl = new DiagView();
    DiagViewUtils.applyXMLTemplate( tpl, xml, getContext() );
    tpl.waitUntilLoaded( 100, 100 );

    final ObservationChart chart = new ObservationChart( tpl );
    chart.setBackgroundPaint( Color.WHITE );
    
    new ExportableChart( chart, EXT, width, height ).exportDocument( os );
    
    chart.dispose();
    tpl.dispose();
  }

  /**
   * @see org.kalypso.ui.calcwizard.bericht.IBerichtExporter#getExtension()
   */
  public String getExtension( )
  {
    return EXT;
  }
}
