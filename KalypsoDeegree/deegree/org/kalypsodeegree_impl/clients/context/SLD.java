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
package org.deegree_impl.clients.context;

import java.net.URL;

import org.deegree.graphics.sld.FeatureTypeStyle;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.xml.Marshallable;
import org.deegree_impl.tools.NetWorker;

/**
 * this class encapsulates the descriptions of a SLD element as defined by the
 * OGC Web Map Context specification
 * 
 * The <SLD>element must contain required <Name>and optional <Title>elements
 * which identify the particular element of a Styled Layer Descriptor to be used
 * for this style. The <SLD>element must then contain one of three alternative
 * sources of description of a layer style: <p/>1. an <OnlineResource>element
 * describing a link to the specified SLD document. <p/><OnlineResource
 * xmlns:xlink="http://www.w3.org/TR/xlink" xlink:type="simple"
 * xlink:href=”http://example.org/this/is/an/example/link/to/the/sld"> <p/>This
 * reference may be to a separately referenced SLD document or to an inline
 * <StyledLayerDescriptor>in the same context document (which may define the
 * styles for multiple layers within the Web Map Context) <p/><p/>2.
 * <StyledLayerDescriptor>element containing inline the namedStyle or userStyle
 * named in the enclosing <Style>element <p/><p/>3. <FeatureTypeStyle>element
 * containing inline the specific feature styling instructions for the enclosing
 * <Style>element
 * 
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth </a>
 */
public class SLD implements Marshallable
{
  private FeatureTypeStyle featureTypeStyle = null;

  private String name = null;

  private String title = null;

  private StyledLayerDescriptor styledLayerDescriptor = null;

  private URL onlineResource = null;

  /**
   * Creates a new SLD object.
   * 
   * @param name
   *          name of the SLD
   * @param title
   *          title of the SLD
   */
  private SLD( String name, String title )
  {
    setName( name );
    setTitle( title );
  }

  /**
   * Creates a new SLD object.
   * 
   * @param name
   *          name of the SLD
   * @param title
   *          title of the SLD
   * @param styledLayerDescriptor
   *          complete StyledLayerDescriptor
   * 
   * @throws ContextException
   */
  public SLD( String name, String title, StyledLayerDescriptor styledLayerDescriptor )
      throws ContextException
  {
    this( name, title );
    setStyledLayerDescriptor( styledLayerDescriptor );
  }

  /**
   * Creates a new SLD object.
   * 
   * @param name
   *          name of the SLD
   * @param title
   *          title of the SLD
   * @param onlineResource
   *          online resource where to access the StyledLayerDescriptor
   * 
   * @throws ContextException
   */
  public SLD( String name, String title, URL onlineResource ) throws ContextException
  {
    this( name, title );
    setOnlineResource( onlineResource );
  }

  /**
   * Creates a new SLD object.
   * 
   * @param name
   *          name of the SLD
   * @param title
   *          title of the SLD
   * @param featureTypeStyle
   *          one concrete FeatureTypeStyle as part of a StyledLayerDescriptor
   * 
   * @throws ContextException
   */
  public SLD( String name, String title, FeatureTypeStyle featureTypeStyle )
      throws ContextException
  {
    this( name, title );
    setFeatureTypeStyle( featureTypeStyle );
  }

  /**
   * name of the SLD
   * 
   * @return
   */
  public String getName()
  {
    return name;
  }

  /**
   * title of the SLD
   * 
   * @return
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * describing a link to the specified SLD document.
   * 
   * @return
   */
  public URL getOnlineResource()
  {
    return onlineResource;
  }

  /**
   * containing inline the specific feature styling instructions for the
   * enclosing <Style>element
   * 
   * @return
   */
  public FeatureTypeStyle getFeatureTypeStyle()
  {
    return featureTypeStyle;
  }

  /**
   * inline the namedStyle or userStyle named in the enclosing <Style>element
   * 
   * @return
   */
  public StyledLayerDescriptor getStyledLayerDescriptor()
  {
    return styledLayerDescriptor;
  }

  /**
   * @see org.deegree_impl.clients.context.SLD#getName()
   * 
   * @param name
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * @see org.deegree_impl.clients.context.Server#getTitle()
   * 
   * @param title
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * @see org.deegree_impl.clients.context.Server#getOnlineResource()
   * 
   * @param onlineResource
   * 
   * @throws ContextException
   */
  public void setOnlineResource( URL onlineResource ) throws ContextException
  {
    if( onlineResource == null )
    {
      throw new ContextException( "onlineResource isn't allowed to be null" );
    }

    this.onlineResource = onlineResource;
  }

  /**
   * @see org.deegree_impl.clients.context.SLD#getFeatureTypeStyle()
   * 
   * @param featureTypeStyle
   * 
   * @throws ContextException
   */
  public void setFeatureTypeStyle( FeatureTypeStyle featureTypeStyle ) throws ContextException
  {
    if( featureTypeStyle == null )
    {
      throw new ContextException( "featureTypeStyle isn't allowed to be null" );
    }

    this.featureTypeStyle = featureTypeStyle;
  }

  /**
   * @see org.deegree_impl.clients.context.SLD#getStyledLayerDescriptor()
   * 
   * @param styledLayerDescriptor
   * 
   * @throws ContextException
   */
  public void setStyledLayerDescriptor( StyledLayerDescriptor styledLayerDescriptor )
      throws ContextException
  {
    if( styledLayerDescriptor == null )
    {
      throw new ContextException( "onlineResource isn't allowed to be null" );
    }

    this.styledLayerDescriptor = styledLayerDescriptor;
  }

  /*
   * (non-Javadoc)
   * 
   * @see org.deegree.xml.Marshallable#exportAsXML()
   */
  public String exportAsXML()
  {
    StringBuffer sb = null;
    if( styledLayerDescriptor != null || featureTypeStyle != null )
    {
      sb = new StringBuffer( 5000 );
    }
    else
    {
      sb = new StringBuffer( 500 );
    }

    sb.append( "<SLD>" );
    if( name != null )
    {
      sb.append( "<Name>" ).append( name ).append( "</Name>" );
    }
    if( title != null )
    {
      sb.append( "<Title>" ).append( title ).append( "</Title>" );
    }
    if( styledLayerDescriptor != null )
    {
      sb.append( ( (Marshallable)styledLayerDescriptor ).exportAsXML() );
    }
    else if( featureTypeStyle != null )
    {
      sb.append( ( (Marshallable)featureTypeStyle ).exportAsXML() );
    }
    else if( onlineResource != null )
    {
      sb.append( "<OnlineResource  xlink:type='simple' xlink:href='" ).append(
          NetWorker.url2String( onlineResource ) ).append( "'/>" );
    }

    sb.append( "</SLD>" );
    return sb.toString();

  }

}