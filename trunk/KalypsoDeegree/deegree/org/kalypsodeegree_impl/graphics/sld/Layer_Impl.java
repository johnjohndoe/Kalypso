/*--------------- Kalypso-Deegree-Header ------------------------------------------------------------

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
 
 
 history:
  
 Files in this package are originally taken from deegree and modified here
 to fit in kalypso. As goals of kalypso differ from that one in deegree
 interface-compatibility to deegree is wanted but not retained always. 
     
 If you intend to use this software in other ways than in kalypso 
 (e.g. OGC-web services), you should consider the latest version of deegree,
 see http://www.deegree.org .

 all modifications are licensed as deegree, 
 original copyright:
 
 Copyright (C) 2001 by:
 EXSE, Department of Geography, University of Bonn
 http://www.giub.uni-bonn.de/exse/
 lat/lon GmbH
 http://www.lat-lon.de
 
---------------------------------------------------------------------------------------------------*/
package org.deegree_impl.graphics.sld;

import java.util.ArrayList;

import org.deegree.graphics.sld.Layer;
import org.deegree.graphics.sld.LayerFeatureConstraints;
import org.deegree.graphics.sld.Style;

/**
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class Layer_Impl implements Layer
{
  protected LayerFeatureConstraints layerFeatureConstraints = null;

  protected ArrayList styles = null;

  protected String name = null;

  /**
   * constructor initializing the class with the <NamedLayer>
   */
  Layer_Impl( String name, LayerFeatureConstraints layerFeatureConstraints, Style[] styles )
  {
    this.styles = new ArrayList();
    setName( name );
    setLayerFeatureConstraints( layerFeatureConstraints );
    setStyles( styles );
  }

  /**
   * The Name element identifies the well-known name of the layer being
   * referenced, and is required. All possible well-known names are usually
   * identified in the capabilities document for a server.
   * 
   * @return the name of the layer
   */
  public String getName()
  {
    return name;
  }

  /**
   * sets the <Name>
   * 
   * @param name
   *          the name of the layer
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * The LayerFeatureConstraints element is optional in a NamedLayer and allows
   * the user to specify constraints on what features of what feature types are
   * to be selected by the named-layer reference. It is essentially a filter
   * that allows the selection of fewer features than are present in the named
   * layer.
   * 
   * @return the LayerFeatureConstraints
   */
  public LayerFeatureConstraints getLayerFeatureConstraints()
  {
    return layerFeatureConstraints;
  }

  /**
   * sets the <LayerFeatureConstraints>
   * 
   * @param layerFeatureConstraints
   *          the LayerFeatureConstraints
   */
  public void setLayerFeatureConstraints( LayerFeatureConstraints layerFeatureConstraints )
  {
    this.layerFeatureConstraints = layerFeatureConstraints;
  }

  /**
   * Returns the styles associated to the Layer. This may be UserStyles or
   * NamedStyles
   * <p>
   * </p>
   * A UserStyle is at the same semantic level as a NamedStyle used in the
   * context of a WMS. In a sense, a named style can be thought of as a
   * reference to a hidden UserStyle that is stored inside of a map server.
   * 
   * @return the Styles of the Layer as ArrayList
   */
  public Style[] getStyles()
  {
    return (Style[])styles.toArray( new Style[styles.size()] );
  }

  /**
   * Adds styles to the Layer.
   * 
   * @param styles
   *          the styles for the layer as Array
   */
  public void setStyles( Style[] styles )
  {
    this.styles.clear();

    if( styles != null )
    {
      for( int i = 0; i < styles.length; i++ )
      {
        this.styles.add( styles[i] );
      }
    }
  }

  /**
   * @see org.deegree_impl.graphics.sld.Layer_Impl#getStyles()
   * @param style
   *          a style to add
   */
  public void addStyle( Style style )
  {
    styles.add( style );
  }

  /**
   * @see org.deegree_impl.graphics.sld.Layer_Impl#getStyles()
   * @param style
   *          a style to remove
   */
  public void removeStyle( Style style )
  {
    styles.remove( styles.indexOf( style ) );
  }

  /**
   * returns a STring-Representation of the layer
   * 
   * @return the layer as String
   */
  public String toString()
  {
    String ret = getClass().getName() + "\n";
    ret = "name = " + name + "\n";
    ret += ( "layerFeatureConstraints = " + layerFeatureConstraints + "\n" );
    ret += ( "styles = " + styles + "\n" );

    return ret;
  }

}