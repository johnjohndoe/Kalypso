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

import org.deegree.ogcbasic.BaseURL;
import org.deegree.xml.Marshallable;

import org.deegree_impl.tools.NetWorker;


/**
 * encapsulates about a layer described/contained by a Web Map Context
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class Layer implements Marshallable {
    private BaseURL dataURL = null;
    private BaseURL metadataURL = null;
    private FormatList formatList = null;
    private Marshallable extension = null;
    private Server server = null;
    private String abstract_ = null;
    private String name = null;
    private String title = null;
    private StyleList styleList = null;
    private String[] srs = null;
    private boolean hidden = false;
    private boolean queryable = false;

    /**
     * Creates a new ContextLayer object.
     *
     * @param server service from which the named Layer may be requested
     * @param name name of the selected layer (
     * @param title title of the selected layer (
     * @param abstract_ abstract of the selected layer (
     * @param srs list of available SRS for the enclosing layer.
     * @param dataURL contains a link to an online resource where data corresponding 
     *                 to the layer can be found.
     * @param metadataURL contains a link to an online resource where descriptive 
     *                    metadata corresponding to the layer can be found.
     * @param formatList parent element containing the list of available image 
     *                   format for this layer.
     * @param styleList parent element containing the list of available styles 
     *                  for this layer.
     * @param extension container tag in which arbitrary vendor specific information 
     *                  can be included
     *
     * @throws ContextException 
     */
    public Layer( Server server, String name, String title, String abstract_, String[] srs, 
                  BaseURL dataURL, BaseURL metadataURL, FormatList formatList, StyleList styleList, 
                  boolean queryable, boolean hidden, Marshallable extension )
          throws ContextException {
        setName( name );
        setTitle( title );
        setAbstract( abstract_ );
        setSrs( srs );
        setDataURL( dataURL );
        setMetadataURL( metadataURL );
        setFormatList( formatList );
        setStyleList( styleList );
        setExtension( extension );
        setServer( server );
        setQueryable( queryable );
        setHidden( hidden );
    }

    /**
     * The element defining the service from which the named Layer may be requested
     *
     * @return 
     */
    public Server getServer() {
        return server;
    }

    /**
     * The name of the selected layer (extracted from Capabilities by the Context 
     * document creator). 
     *
     * @return 
     */
    public String getName() {
        return name;
    }

    /**
     * The title of the selected layer (extracted from Capabilities by the Context 
     * document creator).
     *
     * @return 
     */
    public String getTitle() {
        return title;
    }

    /**
     * The abstract of the selected layer (extracted from Capabilities by the Context 
     * document creator).
     *
     * @return 
     */
    public String getAbstract() {
        return abstract_;
    }

    /**
     * A list of available SRS for the enclosing layer. One of the listed SRS’s 
     * must be the SRS mentioned in the ViewerContext/General/BoundingBox@SRS 
     * element.
     *
     * @return 
     */
    public String[] getSrs() {
        return srs;
    }

    /**
     * This element contains a link to an online resource where data corresponding 
     * to the layer can be found.
     *
     * @return 
     */
    public BaseURL getDataURL() {
        return dataURL;
    }

    /**
     * This element contains a link to an online resource where descriptive metadata 
     * corresponding to the layer can be found.
     *
     * @return 
     */
    public BaseURL getMetadataURL() {
        return metadataURL;
    }

    /**
     * The parent element containing the list of available image format for this 
     * layer. Image formats should be expressed with MIME types as described in 
     * WMS 1.1.1 Specification.<p/>
     * A FormatList shall include at least one Format
     *
     * @return 
     */
    public FormatList getFormatList() {
        return formatList;
    }

    /**
     * The parent element containing the list of available styles for this layer.
     * A StyleList shall include at least one Style
     *
     * @return 
     */
    public StyleList getStyleList() {
        return styleList;
    }

    /**
     * The Extension element is a container tag in which arbitrary vendor specific 
     * information can be included without compromising the ability of other 
     * clients to enforce schema validation.
     *
     * @return 
     */
    public Marshallable getExtension() {
        return extension;
    }

    /**
     * returns true if the layer can be queried with a GetFeatureInfo request
     *
     * @return 
     */
    public boolean isQueryable() {
        return queryable;
    }

    /**
     * returns true if the layer is not visible in the current view
     *
     * @return 
     */
    public boolean isHidden() {
        return hidden;
    }

    /**
     *
     * @param server 
     *
     * @throws ContextException 
     */
    public void setServer( Server server ) throws ContextException {
        if ( server == null ) {
            throw new ContextException( "server isn't allowed to be null" );
        }

        this.server = server;
    }

    /**
     *
     * @param name 
     *
     * @throws ContextException 
     */
    public void setName( String name ) throws ContextException {
        if ( name == null ) {
            throw new ContextException( "name isn't allowed to be null" );
        }

        this.name = name;
    }

    /**
     *
     * @param title 
     *
     * @throws ContextException 
     */
    public void setTitle( String title ) throws ContextException {
        if ( title == null ) {
            throw new ContextException( "title isn't allowed to be null" );
        }

        this.title = title;
    }

    /**
     *
     * @param abstract_ 
     */
    public void setAbstract( String abstract_ ) {
        this.abstract_ = abstract_;
    }

    /**
     *
     * @param srs 
     */
    public void setSrs( String[] srs ) {
        this.srs = srs;
    }

    /**
     *
     * @param dataURL 
     */
    public void setDataURL( BaseURL dataURL ) {
        this.dataURL = dataURL;
    }

    /**
     *
     * @param metadataURL 
     */
    public void setMetadataURL( BaseURL metadataURL ) {
        this.metadataURL = metadataURL;
    }

    /**
     *
     * @param formatList 
     */
    public void setFormatList( FormatList formatList ) {
        this.formatList = formatList;
    }

    /**
     *
     * @param styleList 
     */
    public void setStyleList( StyleList styleList ) {
        this.styleList = styleList;
    }

    /**
     *
     *
     * @param queryable 
     */
    public void setQueryable( boolean queryable ) {
        this.queryable = queryable;
    }

    /**
     *
     *
     * @param hidden 
     */
    public void setHidden( boolean hidden ) {
        this.hidden = hidden;
    }

    /**
     *
     * @param extension 
     */
    public void setExtension( Marshallable extension ) {
        this.extension = extension;
    }

    /**
     * returns a XML-representation of the <tt>Layer</tt>
     *
     * @return 
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer( 10000 );
        sb.append( "<Layer queryable='" );
        if ( queryable ) {
            sb.append( "1' " );
        } else {
            sb.append( "0' " );
        }
        sb.append( "hidden='" );
        if ( hidden ) {
            sb.append( "1' " );
        } else {
            sb.append( "0' " );
        }
        sb.append( '>' );
        sb.append( ( (Marshallable)server ).exportAsXML() );
        sb.append( "<Name>" ).append( name ).append( "</Name>" );
        sb.append( "<Title>" ).append( title ).append( "</Title>" );

        if ( abstract_ != null ) {
            sb.append( "<Abstract>" ).append( abstract_ ).append( "</Abstract>" );
        }

        if ( dataURL != null ) {
            sb.append( "<DataURL width='" ).append( " format='" ).append( dataURL.getFormat() )
              .append( "'>" );
            sb.append( "<OnlineResource  xlink:type='simple' xlink:href='" )
              .append( NetWorker.url2String( dataURL.getOnlineResource() ) ).append( "'/>" );
            sb.append( "</DataURL>" );
        }

        if ( metadataURL != null ) {
            sb.append( "<MetadataURL width='" ).append( " format='" )
              .append( metadataURL.getFormat() ).append( "'>" );
            sb.append( "<OnlineResource  xlink:type='simple' xlink:href='" )
              .append( NetWorker.url2String( metadataURL.getOnlineResource() ) ).append( "'/>" );
            sb.append( "</MetadataURL>" );
        }

        if ( formatList != null ) {
            sb.append( ( (Marshallable)formatList ).exportAsXML() );
        }

        if ( styleList != null ) {
            sb.append( ( (Marshallable)styleList ).exportAsXML() );
        }

        if ( extension != null ) {
            sb.append( extension.exportAsXML() );
        }

        sb.append( "</Layer>" );
        return sb.toString();
    }
}