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
package org.deegree_impl.graphics.sld;

import java.util.ArrayList;

import org.deegree.graphics.sld.Layer;
import org.deegree.graphics.sld.NamedLayer;
import org.deegree.graphics.sld.StyledLayerDescriptor;
import org.deegree.graphics.sld.UserLayer;
import org.deegree.xml.Marshallable;

import org.deegree_impl.tools.Debug;


/**
 * StyledLayerDescriptor: This is a sequence of styled layers, represented at
 * the first level by Layer and UserLayer elements. A "version" attribute
 * has been added to allow the formatting of static-file
 * <p>----------------------------------------------------------------------</p>
 *
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp</a>
 * @version $Revision$ $Date$
 */
public class StyledLayerDescriptor_Impl implements StyledLayerDescriptor, Marshallable {
    private ArrayList layers = null;    
    private String version = null;
    private String abstract_ = null;
    private String name = null;
    private String title = null;
    
    /**
     * @param name
     * @param title
     * @param version
     * @param abstract_
     * @param layers
     */
    StyledLayerDescriptor_Impl(String name, String title, String version, 
    						   String abstract_, Layer[] layers) {
    	this.layers = new ArrayList(layers.length);        
    	setLayers( layers );
    	setVersion( version );
    	setAbstract(abstract_);
    	setName(name);
    	setTitle(title);
    }

    /**
     * constructor initializing the class with the <StyledLayerDescriptor>
     */
    StyledLayerDescriptor_Impl( Layer[] layers, String version ) {
        this.layers = new ArrayList(layers.length);        
        setLayers( layers );
        setVersion( version );
    }

    /**
     * 
     * @return the Layers as Array
     */
    public Layer[] getLayers() {
        return (Layer[])layers.toArray( new Layer[layers.size()] );
    }

    /**
     * Sets Layers
     * @param layers the Layers as Array
     */
    public void setLayers( Layer[] layers ) {
        this.layers.clear();

        if ( layers != null ) {
            for ( int i = 0; i < layers.length; i++ ) {
                this.layers.add( layers[i] );
            }
        }
    }

    /**
     * adds the <Layer>
     * @param layer a Layer to add
     */
    public void addLayer( Layer layer ) {
        layers.add( layers );
    }

    /**
     * removes the <Layer>
     * @param layer a Layer to remove
     */
    public void removeLayer( Layer layer ) {
        if ( layers.indexOf( layer ) != -1 ) {
            layers.remove( layers.indexOf( layer ) );
        }
    }

    /**
     * A UserLayer can contain one or more UserStyles. A UserLayer may direct the
     * WMS to a specified WFS source of feature data. Multiple feature types can
     * be included in a UserLayer, since this is semantically equivalent to a
     * Layer. All feature types of a UserLayer come from the same WFS. The
     * WFS can be named explicitly with the "wfs" attribute or it can be implied
     * by context.
     * @return the UserLayers as Array
     */
    public UserLayer[] getUserLayers() {
        ArrayList list = new ArrayList( layers.size() );
        for (int i = 0; i < layers.size(); i++) {
            if ( layers.get( i ) instanceof UserLayer ) {
                list.add( layers.get(i) );
            }
        }
        return (UserLayer[])list.toArray( new UserLayer[ list.size() ] );
    }

   /**
     * A NamedLayer uses the "name" attribute to identify a layer known to the
     * WMS and can contain zero or more styles, either NamedStyles or UserStyles.
     * In the absence of any styles the default style for the layer is used.
     * @return the NamedLayers as Array
     */
    public NamedLayer[] getNamedLayers() {
        ArrayList list = new ArrayList( layers.size() );
        for (int i = 0; i < layers.size(); i++) {
            if ( layers.get( i ) instanceof NamedLayer ) {
                list.add( layers.get(i) );
            }
        }
        return (NamedLayer[])list.toArray( new NamedLayer[ list.size() ] );
    }


    /**
     * The version attribute gives the SLD version of an SLD document, to
     * facilitate backward compatibility with static documents stored in various
     * different versions of the SLD spec. The string has the format x.y.z, the
     * same as in other OpenGIS Web Server specs. For example, an SLD document
     * stored according to this spec would have the version string 0.7.2.
     * @return the version of the SLD as String
     */
    public String getVersion() {
        return version;
    }

    /**
     * sets the <Version>
     * @param version the version of the SLD
     */
    public void setVersion( String version ) {
        this.version = version;
    }

  	/**
	 * @return Returns the abstract_.
	 */
	public String getAbstract() {
		return abstract_;
	}

	/**
	 * @param abstract_ The abstract_ to set.
	 */
	public void setAbstract(String abstract_) {
		this.abstract_ = abstract_;
	}

	/**
	 * @return Returns the name.
	 */
	public String getName() {
		return name;
	}

	/**
	 * @param name The name to set.
	 */
	public void setName(String name) {
		this.name = name;
	}

	/**
	 * @return Returns the title.
	 */
	public String getTitle() {
		return title;
	}

	/**
	 * @param title The title to set.
	 */
	public void setTitle(String title) {
		this.title = title;
	}
	
	/**
	 * exports the content of the Font as XML formated String
	 *
	 * @return xml representation of the Font
	 */
	public String exportAsXML() {
		Debug.debugMethodBegin();

		StringBuffer sb = new StringBuffer( 10000 );
		sb.append( "<StyledLayerDescriptor version='" + version + "' " );
		sb.append( "xmlns='http://www.opengis.net/sld' " );
		sb.append( "xmlns:gml='http://www.opengis.net/gml' " );
		sb.append( "xmlns:ogc='http://www.opengis.net/ogc' " );
		sb.append( "xmlns:xlink='http://www.w3.org/1999/xlink' " );
		sb.append( "xmlns:xsi='http://www.w3.org/2001/XMLSchema-instance'>" );

		for ( int i = 0; i < layers.size(); i++ ) {
			sb.append( ( (Marshallable)layers.get( i ) ).exportAsXML() );
		}

		sb.append( "</StyledLayerDescriptor>" );

		Debug.debugMethodEnd();
		return sb.toString();
	}
	

}