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

import org.deegree_impl.ogcbasic.ImageURL;
import org.deegree_impl.tools.NetWorker;
import org.deegree.xml.Marshallable;


/**
 * this class encapsulates the style description as defined by the OGC Web
 * Map Context specification
 *
 * @version $Revision$
 * @author <a href="mailto:poth@lat-lon.de">Andreas Poth</a>
 */
public class Style implements Marshallable {
    private ImageURL legendURL = null;
    private SLD sld = null;
    private String abstract_ = null;
    private String name = null;
    private String title = null;
    private boolean current = false;

    /**
     * Creates a new Style object. 
     *
     * @param name The name of the style
     * @param title The human-readable title of the style
     * @param abstract_ A narrative description of the current style
     * @param legendURL location of an image of a map legend describing the 
     *                  current style
     * @param current true the current style is selected.
     *
     * @throws ContextException 
     */
    public Style( String name, String title, String abstract_, ImageURL legendURL, 
                  boolean current ) throws ContextException {
        setName( name );
        setTitle( title );
        setAbstract( abstract_ );
        setLegendURL( legendURL );
        setCurrent( current );
    }
    
    /**
     * Creates a new Style object. 
     *
     * @param sld define the style(s) of the layer with a <SLD> element.
     * @param current true the current style is selected.
     *
     * @throws ContextException 
     */
    public Style( SLD sld, boolean current ) throws ContextException {
        setSld( sld );
        setCurrent( current );
    }

    /**
     * The name of the style (extracted from Capabilities by the Context document 
     * creator).
     *
     * @return 
     */
    public String getName() {
        return name;
    }

    /**
     * The human-readable title of the style (extracted from Capabilities by the 
     * Context document creator).
     *
     * @return 
     */
    public String getTitle() {
        return title;
    }

    /**
     * A narrative description of the current style (extracted from Capabilities 
     * by the Context document creator).
     *
     * @return 
     */
    public String getAbstract() {
        return abstract_;
    }

    /**
     * The location of an image of a map legend describing the current style 
     * (extracted from Capabilities by the Context document creator).
     *
     * @return 
     */
    public ImageURL getLegendURL() {
        return legendURL;
    }

    /**
     * Each <Style> element may alternatively define the style(s) of the layer 
     * with a <SLD> element.
     *
     * @return 
     */
    public SLD getSld() {
        return sld;
    }

    /**
     * returns true the current style is selected.
     *
     * @return 
     */
    public boolean isCurrent() {
        return current;
    }

    /**
     * @see org.deegree_impl.clients.context.Style#getName()
     *
     * @param name 
     */
    public void setName( String name ) throws ContextException {
        if ( ( name == null ) && ( sld == null ) ) {
            throw new ContextException( "either name or sld must be different to null" );
        }

        this.name = name;
    }

    /**
     * @see org.deegree_impl.clients.context.Style#getTitle()
     *
     * @param title 
     */
    public void setTitle( String title ) throws ContextException {
        if ( ( title == null ) && ( sld == null ) ) {
            throw new ContextException( "either title or sld must be different to null" );
        }

        this.title = title;
    }

    /**
     * @see org.deegree_impl.clients.context.Style#getAbstract()
     *
     * @param abstract_ 
     */
    public void setAbstract( String abstract_ ) {
        this.abstract_ = abstract_;
    }

    /**
     * @see org.deegree_impl.clients.context.Style#getLegendURL()
     *
     * @param  legendURL
     */
    public void setLegendURL( ImageURL legendURL ) {
        this.legendURL = legendURL;
    }

    /**
     * @see org.deegree_impl.clients.context.Style#getSld()
     *
     * @param sld 
     */
    public void setSld( SLD sld ) throws ContextException {
        if ( ( sld == null ) && ( title == null || name == null ) ) {
            throw new ContextException( "either sld or name and tile must be different to null" );
        }

        this.sld = sld;
    }

    /**
     * @see org.deegree_impl.clients.context.Style#isCurrent()
     *
     * @param current 
     */
    public void setCurrent( boolean current ) {
        this.current = current;
    }
    
    /* (non-Javadoc)
     * @see org.deegree.xml.Marshallable#exportAsXML()
     */
    public String exportAsXML() {
        StringBuffer sb = new StringBuffer( 2000 );
        sb.append( "<Style>" );
        if ( sld != null ) {
            sb.append( ((Marshallable)sld).exportAsXML() );
        } else {
            sb.append( "<Name>" ).append( name ).append( "</Name>" );
            sb.append( "<Title>" ).append( title ).append( "</Title>" );
            if ( abstract_ != null ) {
                sb.append( "<Abstrac>" ).append( abstract_ ).append( "</Abstrac>" );
            }
            if ( legendURL != null ) {
                sb.append( "<LegendURL width='" ).append( legendURL.getWidth() )
                .append( "' height='" ).append( legendURL.getHeight() )
                .append( "' format='" ).append( legendURL.getFormat() ).append( "'>" );
                sb.append( "<OnlineResource  xlink:type='simple' xlink:href='" )
                .append( NetWorker.url2String( legendURL.getOnlineResource() ) )
                .append( "'/>" );
                sb.append( "</LegendURL>" );
            }
        }
        sb.append( "</Style>" );
        return sb.toString();
    }
    
}