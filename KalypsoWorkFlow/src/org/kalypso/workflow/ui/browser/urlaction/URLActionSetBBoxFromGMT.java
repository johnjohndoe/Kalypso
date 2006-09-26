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
package org.kalypso.workflow.ui.browser.urlaction;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
import java.net.URL;

import javax.xml.bind.Marshaller;
import javax.xml.bind.Unmarshaller;

import org.eclipse.core.resources.IFile;
import org.kalypso.contribs.eclipse.core.resources.ResourceUtilities;
import org.kalypso.ogc.gml.GisTemplateHelper;
import org.kalypso.template.gismapview.Gismapview;
import org.kalypso.template.types.ExtentType;
import org.kalypso.workflow.ui.browser.AbstractURLAction;
import org.kalypso.workflow.ui.browser.ICommandURL;
import org.kalypsodeegree.model.geometry.GM_Envelope;
import org.kalypsodeegree.model.geometry.GM_Surface;
import org.kalypsodeegree_impl.model.cs.ConvenienceCSFactoryFull;
import org.xml.sax.InputSource;

/**
 * @author kuepfer
 */

public class URLActionSetBBoxFromGMT extends AbstractURLAction
{

  // private final static String COMMAND_NAME = "setBBoxInGMT";

  /**
   * The url of the Gismapview to get the bbox from
   * 
   * @see Gismapview
   */
  private static final String PARAM_URL_GMT_FROM = "urlGMTfrom";

  /**
   * The url of the Gismapview to set a new bbox to
   * 
   * @see Gismapview
   */

  private static final String PARAM_URL_GMT_TO = "urlGMTto";

  /**
   * @see org.kalypso.workflow.ui.browser.IURLAction#run(org.kalypso.workflow.ui.browser.ICommandURL)
   */
  public boolean run( ICommandURL commandURL )
  {
    final String gmtURLfrom = commandURL.getParameter( PARAM_URL_GMT_FROM );
    final String gmtURLto = commandURL.getParameter( PARAM_URL_GMT_TO );
    try
    {
      final URL urlFrom = getWorkFlowContext().resolveURL( gmtURLfrom );
      final URL urlTo = getWorkFlowContext().resolveURL( gmtURLto );
      final IFile gmtFromFile = ResourceUtilities.findFileFromURL( urlFrom );
      final IFile gmtToFile = ResourceUtilities.findFileFromURL( urlTo );
      final Unmarshaller unmarshaller = GisTemplateHelper.JC_GISMAP.createUnmarshaller();
      // open existing gmt to load bbox from
      final InputSource isfromGMT = new InputSource( new InputStreamReader( gmtFromFile.getContents(), gmtFromFile.getCharset() ) );
      final Gismapview mapviewfrom = (Gismapview) unmarshaller.unmarshal( isfromGMT );
      // load existing gmt to write new bbox to
      final InputSource isToGMT = new InputSource( new InputStreamReader( gmtToFile.getContents(), gmtToFile.getCharset() ) );
      final Gismapview mapviewto = (Gismapview) unmarshaller.unmarshal( isToGMT );
      final Gismapview transformedGisMapView = transformExtent( mapviewfrom, mapviewto );
      // write change to file
      final Marshaller marshaller = GisTemplateHelper.JC_GISMAP.createMarshaller();
      marshaller.setProperty( Marshaller.JAXB_FORMATTED_OUTPUT, Boolean.TRUE );
      final ByteArrayOutputStream bos = new ByteArrayOutputStream();
      marshaller.marshal( transformedGisMapView, bos );
      final ByteArrayInputStream bis = new ByteArrayInputStream( bos.toByteArray() );
      bos.close();
      if( gmtToFile.exists() )
        gmtToFile.setContents( bis, false, true, null );
      else
        return false;
    }
    catch( Exception e )
    {
      e.printStackTrace();
      return false;
    }
    return true;
  }

  private Gismapview transformExtent( Gismapview mapviewfrom, Gismapview mapviewto ) throws Exception
  {
    final ExtentType bboxFrom = mapviewfrom.getExtent();
    final String sourceSrsAsString = bboxFrom.getSrs();
    final ConvenienceCSFactoryFull csFac = new ConvenienceCSFactoryFull();
    final ExtentType bboxTo = mapviewto.getExtent();
    final String targetSrs = bboxTo.getSrs();
    if( targetSrs.equals( sourceSrsAsString ) )
    {
      bboxTo.setBottom( bboxFrom.getBottom() );
      bboxTo.setTop( bboxFrom.getTop() );
      bboxTo.setLeft( bboxFrom.getLeft() );
      bboxTo.setRight( bboxFrom.getRight() );
    }
    else if( csFac.isKnownCS( sourceSrsAsString ) && csFac.isKnownCS( targetSrs ) )
    {
      final GM_Surface boxAsSurface = GisTemplateHelper.getBoxAsSurface( mapviewfrom, csFac.getCSByName( targetSrs ) );
      // since the helper returns the bbox as a surface we know that the getEnvelope() method returns the real bbox
      final GM_Envelope envelope = boxAsSurface.getEnvelope();
      bboxTo.setBottom( envelope.getMin().getY() );
      bboxTo.setTop( envelope.getMax().getY() );
      bboxTo.setLeft( envelope.getMin().getX() );
      bboxTo.setRight( envelope.getMax().getX() );
    }
    return mapviewto;
  }

}
