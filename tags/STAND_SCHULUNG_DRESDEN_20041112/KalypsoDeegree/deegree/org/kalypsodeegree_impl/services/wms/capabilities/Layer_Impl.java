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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;

import org.deegree.graphics.sld.UserStyle;
import org.deegree.model.geometry.GM_Envelope;
import org.deegree.services.capabilities.MetadataURL;
import org.deegree.services.wms.capabilities.Attribution;
import org.deegree.services.wms.capabilities.AuthorityURL;
import org.deegree.services.wms.capabilities.DataSource;
import org.deegree.services.wms.capabilities.DataURL;
import org.deegree.services.wms.capabilities.Dimension;
import org.deegree.services.wms.capabilities.Extent;
import org.deegree.services.wms.capabilities.FeatureListURL;
import org.deegree.services.wms.capabilities.Identifier;
import org.deegree.services.wms.capabilities.Layer;
import org.deegree.services.wms.capabilities.LayerBoundingBox;
import org.deegree.services.wms.capabilities.ScaleHint;
import org.deegree.services.wms.capabilities.Style;
import org.deegree.xml.Marshallable;
import org.deegree.xml.XMLTools;

/**
 * Each available map is advertised by a <Layer>element in the Capabilities XML.
 * A single parent Layer encloses any number of additional layers, which may be
 * hierarchically nested as desired. Some properties defined in a parent layer
 * are inherited by the children it encloses. These inherited properties may be
 * either redefined or added to by the child.
 * <p>
 * </p>
 * A Map Server shall include at least one <Layer>element for each map layer
 * offered. If desired, layers may be repeated in different categories when
 * relevant. No controlled vocabulary has been defined, so at present Layer and
 * Style Names, Titles and Keywords are arbitrary.
 * <p>
 * </p>
 * The <Layer>element can enclose child elements providing metadata about the
 * Layer.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @author <a href="mailto:mschneider@lat-lon.de">Markus Schneider </a>
 * @version 2002-03-01
 */
public class Layer_Impl implements Layer, Marshallable
{
  private ArrayList authorityURL = null;

  private ArrayList boundingBox = null;

  private ArrayList dataSource = null;

  private ArrayList dataURL = null;

  private ArrayList dimension = null;

  private ArrayList extent = null;

  private ArrayList featureListURL = null;

  private ArrayList identifier = null;

  private ArrayList keywordList = null;

  private ArrayList layer = null;

  private ArrayList metadataURL = null;

  private ArrayList srs = null;

  private Attribution attribution = null;

  private GM_Envelope latLonBoundingBox = null;

  private HashMap styles = null;

  private Layer parent = null;

  private ScaleHint scaleHint = null;

  private String abstract_ = null;

  private String name = null;

  private String title = null;

  private boolean noSubsets = false;

  private boolean opaque = false;

  private boolean queryable = false;

  private int cascaded = -1;

  private int fixedHeight = -1;

  private int fixedWidth = -1;

  /**
   * default constructor
   */
  private Layer_Impl()
  {
    keywordList = new ArrayList( 20 );
    srs = new ArrayList( 200 );
    boundingBox = new ArrayList();
    dimension = new ArrayList();
    extent = new ArrayList();
    authorityURL = new ArrayList();
    identifier = new ArrayList();
    metadataURL = new ArrayList();
    dataURL = new ArrayList();
    featureListURL = new ArrayList();
    styles = new HashMap();
    layer = new ArrayList( 50 );
    dataSource = new ArrayList();
  }

  /**
   * constructor initializing the class with the <Layer>
   */
  Layer_Impl( boolean queryable, int cascaded, boolean opaque, boolean noSubsets, int fixedWidth,
      int fixedHeight, String name, String title, String abstract_, GM_Envelope latLonBoundingBox,
      Attribution attribution, ScaleHint scaleHint, String[] keywordList, String[] srs,
      LayerBoundingBox[] boundingBoxes, Dimension[] dimensions, Extent[] extents,
      AuthorityURL[] authorityURLs, Identifier[] identifiers, MetadataURL[] metadataURLs,
      DataURL[] dataURLs, FeatureListURL[] featureListURLs, Style[] styles, Layer[] layers,
      DataSource[] dataSource, Layer parent )
  {
    this();
    this.queryable = queryable;
    this.cascaded = cascaded;
    this.opaque = opaque;
    this.noSubsets = noSubsets;
    this.fixedWidth = fixedWidth;
    this.fixedHeight = fixedHeight;
    setName( name );
    setTitle( title );
    setAbstract( abstract_ );
    setLatLonBoundingBox( latLonBoundingBox );
    setAttribution( attribution );
    setScaleHint( scaleHint );
    setKeywordList( keywordList );
    setSrs( srs );
    setBoundingBox( boundingBoxes );
    setDimension( dimensions );
    setExtent( extents );
    setAuthorityURL( authorityURLs );
    setIdentifier( identifiers );
    setMetadataURL( metadataURLs );
    setDataURL( dataURLs );
    setFeatureListURL( featureListURLs );
    setStyles( styles );
    setLayer( layers );
    setDataSource( dataSource );
    setParent( parent );
  }

  /**
   * If, and only if, a layer has a <Name>, then it is a map layer that can be
   * requested by using that Name in the LAYERS parameter of a GetMap request.
   * If the layer has a Title but no Name, then that layer is only a category
   * title for all the layers nested within. A Map Server that advertises a
   * Layer containing a Name element shall be able to accept that Name as the
   * value of LAYERS argument in a GetMap request and return the corresponding
   * map. A Client shall not attempt to request a layer that has a Title but no
   * Name.
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the name of the layer
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * A <Title>is required for all layers; it is a human-readable string for
   * presentation in a menu. The Title is not inherited by child Layers.
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * sets the title for the layer
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * Abstract is a narrative description of the map layer. The Abstract elements
   * are not inherited by child Layers.
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * sets the a narrative description of the map layer
   */
  public void setAbstract( String abstract_ )
  {
    this.abstract_ = abstract_;
  }

  /**
   * KeywordList contains zero or more Keywords to aid in catalog searches. The
   * KeywordList elements are not inherited by child Layers.
   */
  public String[] getKeywordList()
  {
    return (String[])keywordList.toArray( new String[keywordList.size()] );
  }

  /**
   * adds the keywordList
   */
  public void addKeyword( String keyword )
  {
    this.keywordList.add( keyword );
  }

  /**
   * sets the keywordList
   */
  public void setKeywordList( String[] keywordList )
  {
    this.keywordList.clear();

    if( keywordList != null )
    {
      for( int i = 0; i < keywordList.length; i++ )
      {
        this.keywordList.add( keywordList[i] );
      }
    }
  }

  /**
   * Every Layer is available in one or more spatial reference systems Every
   * Layer shall have at least one <SRS>element that is either stated explicitly
   * or inherited from a parent Layer . The root <Layer>element shall include a
   * sequence of zero or more SRS elements listing all SRSes that are common to
   * all subsidiary layers. Use a single SRS element with empty content (like
   * so: "<SRS></SRS>") if there is no common SRS. Layers may optionally add
   * to the global SRS list, or to the list inherited from a parent layer. Any
   * duplication shall be ignored by clients. When a Layer is available in
   * several Spatial Reference Systems, there are two ways to encode the list of
   * SRS values. The first of these is new in this version of the specification,
   * the second is deprecated but still included for backwards compatibility.
   * <p>
   * </p>
   * 1. Optional, recommended: Multiple single-valued <SRS>elements: a list of
   * SRS values is represented as a sequence of <SRS>elements, each of which
   * contains only a single SRS name. Example: <SRS>EPSG:1234 </SRS>
   * <SRS>EPSG:5678 </SRS>.
   * <p>
   * </p>
   * 2. Deprecated: Single list-valued <SRS>element: a list of SRS values is
   * represented asa whitespace-separated list of SRS names contained within a
   * single <SRS>element. Example: <SRS>EPSG:1234 EPSG:5678 </SRS>.
   */
  public String[] getSrs()
  {
    String[] pSrs = null;

    if( parent != null )
    {
      pSrs = parent.getSrs();
    }
    else
    {
      pSrs = new String[0];
    }

    String[] crs = new String[pSrs.length + srs.size()];

    for( int i = 0; i < pSrs.length; i++ )
    {
      crs[i] = pSrs[i];
    }

    for( int i = 0; i < srs.size(); i++ )
    {
      crs[i + pSrs.length] = (String)srs.get( i );
    }

    return crs;
  }

  /**
   * return s true if the submitted srs (name) is supported by the layer
   */
  public boolean isSrsSupported( String srs )
  {
    String[] sr = getSrs();
    for( int i = 0; i < sr.length; i++ )
    {
      if( sr[i].equals( srs ) )
      {
        return true;
      }
    }
    return false;
  }

  /**
   * adds the spatial reference system (srs)
   */
  public void addSrs( String srs )
  {
    this.srs.add( srs );
  }

  /**
   * sets the srs
   */
  public void setSrs( String[] srs )
  {
    this.srs.clear();

    if( srs != null )
    {
      for( int i = 0; i < srs.length; i++ )
      {
        this.srs.add( srs[i] );
      }
    }
  }

  /**
   * Every Layer shall have exactly one <LatLonBoundingBox>element that is
   * either stated explicitly or inherited from a parent Layer.
   * LatLonBoundingBox states the minimum bounding rectangle of the map data in
   * the EPSG:4326 geographic coordinate system. The LatLonBoundingBox
   * attributes minx, miny, maxx, maxy indicate the edges of an enclosing
   * rectangle in decimal degrees. LatLonBoundingBox shall be supplied
   * regardless of what SRS the map server may support, but it may be
   * approximate if EPSG:4326 is not supported. Its purpose is to facilitate
   * geographic searches without requiring coordinate transformations by the
   * search engine.
   */
  public GM_Envelope getLatLonBoundingBox()
  {
    if( ( latLonBoundingBox == null ) && ( parent != null ) )
    {
      return parent.getLatLonBoundingBox();
    }
    else
    {
      return latLonBoundingBox;
    }
  }

  /**
   * sets the LatLonBoundingBox element that is either stated explicitly or
   * inherited from a parent Layer.
   */
  public void setLatLonBoundingBox( GM_Envelope latLonBoundingBox )
  {
    this.latLonBoundingBox = latLonBoundingBox;
  }

  /**
   * Layers may have zero or more <BoundingBox>elements that are either stated
   * explicitly or inherited from a parent Layer. Each BoundingBox states the
   * bounding rectangle of the map data in a particular spatial reference
   * system; the attribute SRS indicates which SRS applies. If the data area is
   * shaped irregularly then the BoundingBox gives the minimum enclosing
   * rectangle. The attributes minx, miny, maxx, maxy indicate the edges of the
   * bounding box in units of the specified SRS. Optional resx and resy
   * attributes indicate the spatial resolution of the data in those same units.
   * <p>
   * </p>
   * A Layer may have multiple BoundingBox element, but each one shall state a
   * different SRS. A Layer inherits any BoundingBox values defined by its
   * parents. A BoundingBox inherited from the parent Layer for a particular SRS
   * is replaced by any declaration for the same SRS in the child Layer. A
   * BoundingBox in the child for a new SRS not already declared by the parent
   * is added to the list of bounding boxes for the child Layer. A single Layer
   * element shall not contain more than one BoundingBox for the same SRS.
   */
  public LayerBoundingBox[] getBoundingBox()
  {
    HashMap list = new HashMap( 100 );

    if( parent != null )
    {
      LayerBoundingBox[] plb = parent.getBoundingBox();

      for( int i = 0; i < plb.length; i++ )
      {
        list.put( plb[i].getSRS(), plb[i] );
      }
    }

    for( int i = 0; i < boundingBox.size(); i++ )
    {
      LayerBoundingBox lb = (LayerBoundingBox)boundingBox.get( i );
      list.put( lb.getSRS(), lb );
    }

    LayerBoundingBox[] lbs = new LayerBoundingBox[list.size()];
    return (LayerBoundingBox[])list.values().toArray( lbs );
  }

  /**
   * adds the <BoundingBox>
   */
  public void addBoundingBox( GM_Envelope boundingBox )
  {
    this.boundingBox.add( boundingBox );
  }

  /**
   * sets the boundingBox
   */
  public void setBoundingBox( LayerBoundingBox[] boundingBox )
  {
    this.boundingBox.clear();

    if( boundingBox != null )
    {
      for( int i = 0; i < boundingBox.length; i++ )
      {
        this.boundingBox.add( boundingBox[i] );
      }
    }
  }

  /**
   * Dimension declarations are inherited from parent Layers. Any new Dimension
   * declarations in the child are added to the list inherited from the parent.
   * A child shall not redefine a Dimension with the same name attribute as one
   * that was inherited.
   */
  public Dimension[] getDimension()
  {
    HashMap list = new HashMap();

    if( parent != null )
    {
      Dimension[] pDim = parent.getDimension();

      for( int i = 0; i < pDim.length; i++ )
      {
        list.put( pDim[i].getName(), pDim[i] );
      }
    }

    for( int i = 0; i < dimension.size(); i++ )
    {
      Dimension dim = (Dimension)dimension.get( i );

      if( list.get( dim.getName() ) == null )
      {
        list.put( dim.getName(), dim );
      }
    }

    return (Dimension[])list.values().toArray( new Dimension[list.size()] );
  }

  /**
   * adds the dimension
   */
  public void addDimension( Dimension dimension )
  {
    this.dimension.add( dimension );
  }

  /**
   * sets the dimension
   */
  public void setDimension( Dimension[] dimension )
  {
    this.dimension.clear();

    if( dimension != null )
    {
      for( int i = 0; i < dimension.length; i++ )
      {
        this.dimension.add( dimension[i] );
      }
    }
  }

  /**
   * Extent declarations are inherited from parent Layers. Any Extent
   * declarations in the child with the same name attribute as one inherited
   * from the parent replaces the value declared by the parent. A Layer shall
   * not declare an Extent unless a Dimension with the same name has been
   * declared or inherited earlier in the Capabilities XML.
   */
  public Extent[] getExtent()
  {
    HashMap list = new HashMap();

    if( parent != null )
    {
      Extent[] pEx = parent.getExtent();

      for( int i = 0; i < pEx.length; i++ )
      {
        list.put( pEx[i].getName(), pEx[i] );
      }
    }

    for( int i = 0; i < dimension.size(); i++ )
    {
      Extent ex = (Extent)extent.get( i );
      list.put( ex.getName(), ex );
    }

    return (Extent[])list.values().toArray( new Extent[list.size()] );
  }

  /**
   * adds the extent declarations
   */
  public void addExtent( Extent extent )
  {
    this.extent.add( extent );
  }

  /**
   * sets the extent
   */
  public void setExtent( Extent[] extent )
  {
    this.extent.clear();

    if( extent != null )
    {
      for( int i = 0; i < extent.length; i++ )
      {
        this.extent.add( extent[i] );
      }
    }
  }

  /**
   * The optional <Attribution>element provides a way to identify the source of
   * the map data used in a Layer or collection of Layers. Attribution encloses
   * several optional elements: <OnlineResource>states the data provider's URL;
   * <Title>is a human-readable string naming the data provider; <LogoURL>is the
   * URL of a logo image. Client applications may choose to display one or more
   * of these items. A <Format>element in LogoURL indicates the MIME type of the
   * logo image, and the attributes width and height state the size of the image
   * in pixels.
   */
  public Attribution getAttribution()
  {
    if( ( parent != null ) && ( attribution == null ) )
    {
      return parent.getAttribution();
    }
    else
    {
      return attribution;
    }
  }

  /**
   * sets the optional <Attribution>element
   */
  public void setAttribution( Attribution attribution )
  {
    this.attribution = attribution;
  }

  /**
   * The authority attribute of the Identifier element corresponds to the name
   * attribute of a separate <AuthorityURL>element. AuthorityURL encloses an
   * <OnlineResource>element which states the URL of a document defining the
   * meaning of the Identifier values.
   */
  public AuthorityURL[] getAuthorityURL()
  {
    HashMap list = new HashMap();

    if( parent != null )
    {
      AuthorityURL[] pAu = parent.getAuthorityURL();

      for( int i = 0; i < pAu.length; i++ )
      {
        list.put( pAu[i].getName(), pAu[i] );
      }
    }

    for( int i = 0; i < authorityURL.size(); i++ )
    {
      AuthorityURL au = (AuthorityURL)authorityURL.get( i );

      if( list.get( au.getName() ) == null )
      {
        list.put( au.getName(), au );
      }
    }

    AuthorityURL[] aus = new AuthorityURL[list.size()];
    return (AuthorityURL[])list.values().toArray( aus );
  }

  /**
   * adds the authority attribute of the Identifier element
   */
  public void addAuthorityURL( AuthorityURL authorityURL )
  {
    this.authorityURL.add( authorityURL );
  }

  /**
   * sets the authority attribute of the Identifier element
   */
  public void setAuthorityURL( AuthorityURL[] authorityURL )
  {
    this.authorityURL.clear();

    if( authorityURL != null )
    {
      for( int i = 0; i < authorityURL.length; i++ )
      {
        this.authorityURL.add( authorityURL[i] );
      }
    }
  }

  /**
   * A Map Server may use zero or more <Identifier>elements to list ID numbers
   * or labels defined by a particular Authority. The text content of the
   * Identifier element is the ID value.
   */
  public Identifier[] getIdentifier()
  {
    HashMap list = new HashMap();

    if( parent != null )
    {
      Identifier[] pIden = parent.getIdentifier();

      for( int i = 0; i < pIden.length; i++ )
      {
        list.put( pIden[i].getAuthority(), pIden[i] );
      }
    }

    for( int i = 0; i < identifier.size(); i++ )
    {
      Identifier iden = (Identifier)identifier.get( i );

      if( list.get( iden.getAuthority() ) == null )
      {
        list.put( iden.getAuthority(), iden );
      }
    }

    Identifier[] ids = new Identifier[list.size()];
    return (Identifier[])list.values().toArray( ids );
  }

  /**
   * adds the <Identifier>
   */
  public void addIdentifier( Identifier identifier )
  {
    this.identifier.add( identifier );
  }

  /**
   * sets the <Identifier>
   */
  public void setIdentifier( Identifier[] identifier )
  {
    this.identifier.clear();

    if( identifier != null )
    {
      for( int i = 0; i < identifier.length; i++ )
      {
        this.identifier.add( identifier[i] );
      }
    }
  }

  /**
   * A Map Server should use one or more <MetadataURL>elements to offer
   * detailed, standardized metadata about the data underneath a particular
   * layer. The type attribute indicates the standard to which the metadata
   * complies. Two types are defined at present: the value 'TC211' refers to
   * [ISO 19115]; the value 'FGDC' refers to [FGDC-STD-001-1988]. The
   * MetadataURL element shall not be used to reference metadata in a
   * non-standardized metadata format; see DataURL instead. The enclosed
   * <Format>element indicates the file format MIME type of the metadata record.
   */
  public MetadataURL[] getMetadataURL()
  {
    return (MetadataURL[])metadataURL.toArray( new MetadataURL[metadataURL.size()] );
  }

  /**
   * adds the metadataURL
   */
  public void addMetadataURL( MetadataURL metadataURL )
  {
    this.metadataURL.add( metadataURL );
  }

  /**
   * sets the metadataURL
   */
  public void setMetadataURL( MetadataURL[] metadataURL )
  {
    this.metadataURL.clear();

    if( metadataURL != null )
    {
      for( int i = 0; i < metadataURL.length; i++ )
      {
        this.metadataURL.add( metadataURL[i] );
      }
    }
  }

  /**
   * A Map Server may use DataURL to offer more information about the data
   * represented by a particular layer. While the semantics are not
   * well-defined, as long as the results of an HTTP GET request against the
   * DataURL are properly MIME-typed, Viewer Clients and Cascading Map Servers
   * can make use of this. Use <MetadataURL>instead for a precisely defined
   * reference to standardized metadata records.
   */
  public DataURL[] getDataURL()
  {
    return (DataURL[])dataURL.toArray( new DataURL[dataURL.size()] );
  }

  /**
   * adds the dataURL
   */
  public void addDataURL( DataURL dataURL )
  {
    this.dataURL.add( dataURL );
  }

  /**
   * sets the dataURL
   */
  public void setDataURL( DataURL[] dataURL )
  {
    this.dataURL.clear();

    if( dataURL != null )
    {
      for( int i = 0; i < dataURL.length; i++ )
      {
        this.dataURL.add( dataURL[i] );
      }
    }
  }

  /**
   * A Map Server may use a <FeatureListURL>element to point to a list of the
   * features represented in a Layer.
   */
  public FeatureListURL[] getFeatureListURL()
  {
    return (FeatureListURL[])featureListURL.toArray( new FeatureListURL[featureListURL.size()] );
  }

  /**
   * adds the <FeatureListURL>
   */
  public void addFeatureListURL( FeatureListURL featureListURL )
  {
    this.featureListURL.add( featureListURL );
  }

  /**
   * sets the <FeatureListURL>
   */
  public void setFeatureListURL( FeatureListURL[] featureListURL )
  {
    this.featureListURL.clear();

    if( featureListURL != null )
    {
      for( int i = 0; i < featureListURL.length; i++ )
      {
        this.featureListURL.add( featureListURL[i] );
      }
    }
  }

  /**
   * returns a list of style that can be used form rendering the layer.
   */
  public Style[] getStyles()
  {
    HashMap list = new HashMap();

    if( parent != null )
    {
      Style[] pStyle = parent.getStyles();

      for( int i = 0; i < pStyle.length; i++ )
      {
        list.put( pStyle[i].getName(), pStyle[i] );
      }
    }

    Iterator iterator = styles.values().iterator();

    while( iterator.hasNext() )
    {
      Style style = (Style)iterator.next();

      if( list.get( style.getName() ) == null )
      {
        list.put( style.getName(), style );
      }
    }

    return (Style[])list.values().toArray( new Style[list.size()] );
  }

  /**
   * adds a list of style that can be used form rendering the layer.
   */
  public void addStyles( Style style )
  {
    this.styles.put( style.getName(), style );
  }

  /**
   * sets a list of style that can be used form rendering the layer.
   */
  public void setStyles( Style[] styles )
  {
    this.styles.clear();

    if( styles != null )
    {
      for( int i = 0; i < styles.length; i++ )
      {
        this.styles.put( styles[i].getName(), styles[i] );
      }
    }
  }

  /**
   * returns the <tt>UserStyle</tt> (SLD) representation of the style
   * identified by the submitted name.
   * 
   * @param name
   *          of the requested style
   * @return SLD - UserStyle
   *  
   */
  public UserStyle getStyle( String name )
  {

    Style style = (Style)styles.get( name );
    UserStyle us = null;

    if( style == null )
    {
      if( parent != null )
      {
        us = parent.getStyle( name );
      }
    }
    else
    {
      us = style.getStyleContent();
    }

    return us;
  }

  /**
   * returns the <tt>Style</tt> identified by the submitted name.
   * 
   * @param name
   *          of the requested style
   * @return Style
   *  
   */
  public Style getStyleResource( String name )
  {

    Style style = (Style)styles.get( name );

    if( style == null )
    {
      if( parent != null )
      {
        style = parent.getStyleResource( name );
      }
    }

    return style;
  }

  /**
   * Layers may include a <ScaleHint>element that suggests minimum and maximum
   * scales for which it is appropriate to display this layer. Because WMS
   * output is destined for output devices of arbitrary size and resolution, the
   * usual definition of scale as the ratio of map size to real-world size is
   * not appropriate here. The following definition of Scale Hint is
   * recommended. Consider a hypothetical map with a given Bounding Box, width
   * and height. The central pixel of that map (or the pixel just to the
   * northwest of center) will have some size, which can be expressed as the
   * ground distance in meters of the southwest to northeast diagonal of that
   * pixel. The two values in ScaleHint are the minimum and maximum recommended
   * values of that diagonal. It is recognized that this definition is not
   * geodetically precise, but at the same time the hope is that by including it
   * conventions will develop that can be later specified more clearly.
   */
  public ScaleHint getScaleHint()
  {
    if( ( parent != null ) && ( scaleHint == null ) )
    {
      return parent.getScaleHint();
    }
    else
    {
      return scaleHint;
    }
  }

  /**
   * sets the <ScaleHint>
   */
  public void setScaleHint( ScaleHint scaleHint )
  {
    this.scaleHint = scaleHint;
  }

  /**
   * returns a list of layers the are enclosed by this layer.
   */
  public Layer[] getLayer()
  {
    return (Layer[])layer.toArray( new Layer[layer.size()] );
  }

  /**
   * removes a Layer identified by its name from the parent Layer. A reference
   * to the removed layer will be returned. If no Layer matching the passed name
   * can be found nothing happens and <tt>null</tt> will be returned.
   * 
   * @return removerd Layer
   */
  public Layer removeLayer( String name )
  {
    for( int i = 0; i < layer.size(); i++ )
    {
      Layer ly = (Layer)layer.get( i );
      if( ly.getName().equals( name ) )
      {
        layer.remove( i );
        return ly;
      }
    }
    return null;
  }

  /**
   * adds a list of layers the are enclosed by this layer.
   */
  public void addLayer( Layer layer )
  {
    this.layer.add( layer );
  }

  /**
   * sets a list of layers the are enclosed by this layer.
   */
  public void setLayer( Layer[] layer )
  {
    this.layer.clear();

    if( layer != null )
    {
      for( int i = 0; i < layer.length; i++ )
      {
        this.layer.add( layer[i] );
      }
    }
  }

  /**
   * source where the WMS can find the data of a layer.
   */
  public DataSource[] getDataSource()
  {
    return (DataSource[])dataSource.toArray( new DataSource[dataSource.size()] );
  }

  /**
   * source where the WMS can find the data of a layer that matches the
   * submitted scale. If no <tt>DataSource</tt> is defined that matches the
   * scale,</tt> null</tt> will be returned;
   * 
   * @return <tt>DataSource</tt> that fits the submitted scale
   *  
   */
  public DataSource getDataSource( double scale )
  {
    DataSource ds = null;

    for( int i = 0; i < dataSource.size(); i++ )
    {
      DataSource tmp = (DataSource)dataSource.get( i );
      if( ( tmp.getScaleHint() != null ) && ( tmp.getScaleHint().getMin() <= scale )
          && ( tmp.getScaleHint().getMax() > scale ) )
      {
        ds = tmp;
        break;
      }
    }

    return ds;
  }

  /**
   * source where the WMS can find the data of a layer.
   */
  public void setDataSource( DataSource[] dataSource )
  {
    this.dataSource.clear();

    if( dataSource != null )
    {
      for( int i = 0; i < dataSource.length; i++ )
      {
        this.dataSource.add( dataSource[i] );
      }
    }
  }

  /**
   * source where the WMS can find the data of a layer.
   */
  public void addDataSource( DataSource dataSource )
  {
    this.dataSource.add( dataSource );
  }

  /**
   * returns the parent layer of this layer. If the method returns <tt>null</tt>
   * the current layer is the root layer. In addition with the <tt>getLayer</tt>
   * method this enables a program to traverse the layer tree in both
   * directions.
   */
  public Layer getParent()
  {
    return parent;
  }

  /**
   * sets the parent layer of this layer.
   */
  public void setParent( Layer parent )
  {
    this.parent = parent;
  }

  /**
   * returns '0' if the layer is provided directly form the deegree WMS. other
   * it returns the number of cascaded WMS servers the is passed through
   *  
   */
  public int getCascaded()
  {
    return cascaded;
  }

  /**
   * returns '0' if the WMS can resize map to arbitrary height. nonzero: map has
   * a fixed height that cannot be changed by the WMS.
   *  
   */
  public int getFixedHeight()
  {
    return fixedHeight;
  }

  /**
   * returns '0' if the WMS can resize map to arbitrary width. nonzero: map has
   * a fixed width that cannot be changed by the WMS.
   *  
   */
  public int getFixedWidth()
  {
    return fixedWidth;
  }

  /**
   * returns false if the WMS can map a subset of the full bounding box.
   *  
   */
  public boolean hasNoSubsets()
  {
    return noSubsets;
  }

  /**
   * returns false if map data represents vector features that probably do not
   * completely fill space.
   *  
   */
  public boolean isOpaque()
  {
    return opaque;
  }

  /**
   * returns true if the layer is queryable. That means it can be targeted by a
   * GetFeatureInfo request.
   *  
   */
  public boolean isQueryable()
  {
    return queryable;
  }

  /**
   * Returns an XML representation of this object.
   */
  public String exportAsXML()
  {
    StringBuffer sb = new StringBuffer();

    sb.append( "<Layer" );

    if( queryable )
    {
      sb.append( " queryable=\"1\"" );
    }
    else
    {
      sb.append( " queryable=\"0\"" );
    }

    if( cascaded != -1 )
    {
      sb.append( " cascaded=\"" ).append( cascaded ).append( "\"" );
    }

    if( opaque )
    {
      sb.append( " opaque=\"1\"" );
    }
    else
    {
      sb.append( " opaque=\"0\"" );
    }

    if( noSubsets )
    {
      sb.append( "  noSubsets=\"1\"" );
    }
    else
    {
      sb.append( "  noSubsets=\"0\"" );
    }

    if( fixedWidth != -1 )
    {
      sb.append( " fixedWidth=\"" ).append( fixedWidth ).append( "\"" );
    }

    if( fixedHeight != -1 )
    {
      sb.append( " fixedHeight=\"" ).append( fixedHeight ).append( "\"" );
    }

    sb.append( ">" );

    if( name != null && !( name.trim().equals( "" ) ) )
    {
      sb.append( "<Name>" ).append( XMLTools.validateCDATA( name ) ).append( "</Name>" );
    }

    sb.append( "<Title>" ).append( XMLTools.validateCDATA( title ) ).append( "</Title>" );

    if( abstract_ != null )
    {
      sb.append( "<Abstract>" ).append( XMLTools.validateCDATA( abstract_ ) )
          .append( "</Abstract>" );
    }

    if( ( keywordList != null ) && ( keywordList.size() > 0 ) )
    {
      sb.append( "<KeywordList>" );

      Iterator it = keywordList.iterator();

      while( it.hasNext() )
      {
        sb.append( "<Keyword>" ).append( XMLTools.validateCDATA( (String)it.next() ) ).append(
            "</Keyword>" );
      }

      sb.append( "</KeywordList>" );
    }

    if( srs != null )
    {
      Iterator it = srs.iterator();

      while( it.hasNext() )
      {
        sb.append( "<SRS>" ).append( XMLTools.validateCDATA( (String)it.next() ) )
            .append( "</SRS>" );
      }
    }

    if( latLonBoundingBox != null )
    {
      sb.append( "<LatLonBoundingBox minx=\"" ).append( latLonBoundingBox.getMin().getX() ).append(
          "\" miny=\"" ).append( latLonBoundingBox.getMin().getY() ).append( "\" maxx=\"" ).append(
          latLonBoundingBox.getMax().getX() ).append( "\" maxy=\"" ).append(
          latLonBoundingBox.getMax().getY() ).append( "\"/>" );
    }

    if( boundingBox != null )
    {
      Iterator it = boundingBox.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( dimension != null )
    {
      Iterator it = dimension.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( extent != null )
    {
      Iterator it = extent.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( attribution != null )
    {
      sb.append( ( (Marshallable)attribution ).exportAsXML() );
    }

    if( authorityURL != null )
    {
      Iterator it = authorityURL.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( identifier != null )
    {
      Iterator it = identifier.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( metadataURL != null )
    {
      Iterator it = metadataURL.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( dataURL != null )
    {
      Iterator it = dataURL.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( featureListURL != null )
    {
      Iterator it = featureListURL.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    if( styles != null )
    {
      Iterator it = styles.values().iterator();

      while( it.hasNext() )
      {
        Style style = (Style)it.next();

        if( !( style.getName().equalsIgnoreCase( "default" ) )
        /*
         * && !( style.getName().startsWith( "default:" ) )
         */
        )
        {
          sb.append( ( (Marshallable)style ).exportAsXML() );
        }
      }
    }

    if( scaleHint != null )
    {
      sb.append( ( (Marshallable)scaleHint ).exportAsXML() );
    }

    if( dataSource != null )
    {
      if( name != null )
      {
        Iterator it = dataSource.iterator();
        while( it.hasNext() )
        {
          sb.append( ( (Marshallable)it.next() ).exportAsXML() );
        }
      }
    }

    if( layer != null )
    {
      Iterator it = layer.iterator();

      while( it.hasNext() )
      {
        sb.append( ( (Marshallable)it.next() ).exportAsXML() );
      }
    }

    sb.append( "</Layer>" );

    return sb.toString();
  }

}