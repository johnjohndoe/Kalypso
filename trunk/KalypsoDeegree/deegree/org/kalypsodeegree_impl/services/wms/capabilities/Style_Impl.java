/*----------------    FILE HEADER  ------------------------------------------

 This file is part of deegree.
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon Fitzke/Fretter/Poth GbR
 http://www.lat-lon.de

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

 Andreas Poth
 lat/lon Fitzke/Fretter/Poth GbR
 Meckenheimer Allee 176
 53115 Bonn
 Germany
 E-Mail: poth@lat-lon.de

 Jens Fitzke
 Department of Geography
 University of Bonn
 Meckenheimer Allee 166
 53115 Bonn
 Germany
 E-Mail: jens.fitzke@uni-bonn.de

 
 ---------------------------------------------------------------------------*/
package org.deegree_impl.services.wms.capabilities;

import java.io.InputStreamReader;
import java.net.URL;
import java.util.HashMap;

import org.deegree.graphics.sld.NamedLayer;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserLayer;
import org.deegree.graphics.sld.UserStyle;
import org.deegree.services.wms.capabilities.LegendURL;
import org.deegree.services.wms.capabilities.Style;
import org.deegree.services.wms.capabilities.StyleSheetURL;
import org.deegree.services.wms.capabilities.StyleURL;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLParsingException;
import org.deegree.xml.XMLTools;
import org.deegree_impl.graphics.sld.SLDFactory;

/**
 * Zero or more Styles may be advertised for a Layer or collection of layers
 * using <Style>elements, each of which shall have <Name>and <Title>elements.
 * The style's Name is used in the Map request STYLES parameter. The Title is a
 * human-readable string. If only a single style is available, that style is
 * known as the "default" style and need not be advertised by the server.
 * <p>
 * </p>
 * A Style may contain several other elements in the Capabilities XML DTD. In
 * particular, <Abstract>provides a narrative description while <LegendURL>
 * contains the location of an image of a map legend appropriate to the
 * enclosing Style. A <Format>element in LegendURL indicates the MIME type of
 * the logo image, and the attributes width and height state the size of the
 * image in pixels.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version $Revision$
 */
public class Style_Impl implements Style, Marshallable
{
  static HashMap styles = null;

  static
  {
    styles = new HashMap( 100 );
    styles.put( "default", null );
  }

  private String abstract_ = null;

  private String name = null;

  private String title = null;

  private StyleSheetURL styleSheetURL = null;

  private StyleURL styleURL = null;

  private URL styleResource = null;

  private LegendURL[] legendURLList = null;

  /**
   * constructor initializing the class with the <Style>
   */
  Style_Impl( String name, String title, String abstract_, LegendURL[] legendURLs,
      StyleSheetURL styleSheetURL, StyleURL styleURL, URL styleResource )
      throws XMLParsingException
  {

    if( name.endsWith( "null" ) )
    {
      this.name = name.substring( 0, name.length() - 5 );
    }
    else
    {
      this.name = name;
    }
    if( title.endsWith( "null" ) )
    {
      this.title = title.substring( 0, name.length() - 5 );
    }
    else
    {
      this.title = title;
    }
    this.abstract_ = abstract_;
    this.styleResource = styleResource;
    this.styleSheetURL = styleSheetURL;
    this.legendURLList = legendURLs;
    this.styleURL = styleURL;
    // only cache styles that belong to the local WMS
    // for Remote WMS, styleResource is always null
    if( styleResource != null )
    {
      synchronized( styles )
      {
        if( !styles.containsKey( name ) )
        {
          loadStyles( styleResource );
        }
      }
    }
  }

  /**
   * loads style definitions from the submitted URL and writes them to the
   * styles HashMap. The styleResource must provide style definitions in a SLD
   * conform XML document.
   * <p>
   * 
   * @param styleResource
   *          resource of the style defintions
   * @exception XMLParsingException
   *              thrown if the resource doesn't hold the style definitions in a
   *              SLD conform document
   */
  private static synchronized void loadStyles( URL styleResource ) throws XMLParsingException
  {
    try
    {
      InputStreamReader isr = new InputStreamReader( styleResource.openStream() );
      StyledLayerDescriptor sld = SLDFactory.createSLD( isr );
      // get styles from named layers
      NamedLayer[] nl = sld.getNamedLayers();

      for( int i = 0; i < nl.length; i++ )
      {
        org.deegree.graphics.sld.Style[] sldStyles = nl[i].getStyles();

        for( int j = 0; j < sldStyles.length; j++ )
        {
          if( sldStyles[j] instanceof UserStyle )
          {
            UserStyle us = (UserStyle)sldStyles[j];
            styles.put( us.getName(), us );
          }
        }
      }

      // get styles from user layers
      UserLayer[] ul = sld.getUserLayers();

      for( int i = 0; i < ul.length; i++ )
      {
        org.deegree.graphics.sld.Style[] sldStyles = ul[i].getStyles();

        for( int j = 0; j < sldStyles.length; j++ )
        {
          if( sldStyles[j] instanceof UserStyle )
          {
            UserStyle us = (UserStyle)sldStyles[j];
            styles.put( us.getName(), us );
          }
        }
      }

      isr.close();
    }
    catch( Exception e )
    {
      e.printStackTrace();
      throw new XMLParsingException( e.toString() );
    }
  }

  /**
   * returns the name - machine code - of the style
   */
  public String getName()
  {
    return name;
  }

  /**
   * returns the title (human readable) of the style
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * returns a short narrative description of the style
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * returns an array of LegendURL objects that defines the location, size and
   * format of the legend graphics
   */
  public LegendURL[] getLegendURL()
  {
    return legendURLList;
  }

  /**
   * StyleSheeetURL provides symbology information for each style of a layer.
   */
  public StyleSheetURL getStyleSheetURL()
  {
    return styleSheetURL;
  }

  /**
   * A Style element lists the name by which a style is requested and a
   * human-readable title for pick lists, optionally (and ideally) provides a
   * human-readable description, and optionally gives a style URL.
   */
  public StyleURL getStyleURL()
  {
    return styleURL;
  }

  /**
   * returns the URL where to access the XML (SLD) document definingthe style
   * 
   * @return
   */
  public URL getStyleResource()
  {
    return styleResource;
  }

  /**
   * returns the content of the style as SLD style object
   * 
   * @return instance of org.deegree.graphics.sld.Style
   *  
   */
  public UserStyle getStyleContent()
  {
    return (UserStyle)styles.get( name );
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<Style>" ).append( "<Name>" ).append( XMLTools.validateCDATA( name ) ).append(
        "</Name>" ).append( "<Title>" ).append( XMLTools.validateCDATA( title ) ).append(
        "</Title>" );

    if( styleResource != null )
    {
      sb.append( "<StyleResource>" ).append( styleResource.toString() ).append( "</StyleResource>" );
    }

    if( abstract_ != null )
    {
      sb.append( "<Abstract>" ).append( XMLTools.validateCDATA( abstract_ ) )
          .append( "</Abstract>" );
    }

    if( legendURLList != null )
    {
      for( int i = 0; i < legendURLList.length; i++ )
      {
        sb.append( ( (Marshallable)legendURLList[i] ).exportAsXML() );
      }
    }

    if( styleSheetURL != null )
    {
      sb.append( ( (Marshallable)styleSheetURL ).exportAsXML() );
    }

    if( styleURL != null )
    {
      sb.append( ( (Marshallable)styleURL ).exportAsXML() );
    }

    sb.append( "</Style>" );

    return sb.toString();
  }

  /**
   * 
   * 
   * @return
   */
  public String toString()
  {
    String ret = null;
    ret = "name = " + name + "\n";
    ret += ( "title = " + title + "\n" );
    ret += ( "abstract_ = " + abstract_ + "\n" );
    ret += ( "styleSheetURL = " + styleSheetURL + "\n" );
    ret += ( "styleURL = " + styleURL + "\n" );
    ret += ( "legendURLList = " + legendURLList + "\n" );
    return ret;
  }
}