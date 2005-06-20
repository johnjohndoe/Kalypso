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
package org.kalypsodeegree_impl.graphics.sld;

import java.util.ArrayList;

import org.kalypsodeegree.graphics.sld.FeatureTypeStyle;
import org.kalypsodeegree.graphics.sld.UserStyle;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * A user-defined allows map styling to be defined externally from a system and to be passed around in an interoperable
 * format.
 * <p>
 * </p>
 * A UserStyle is at the same semantic level as a NamedStyle used in the context of a WMS. In a sense, a named style can
 * be thought of as a reference to a hidden UserStyle that is stored inside of a map server.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class UserStyle_Impl extends Style_Impl implements UserStyle, Marshallable
{
  private ArrayList featureTypeStyles = null;

  private String abstract_ = null;

  private String title = null;

  private boolean default_ = false;

  /**
   * constructor initializing the class with the <UserStyle>
   */
  public UserStyle_Impl( String name, String title, String abstract_, boolean default_,
      FeatureTypeStyle[] featureTypeStyles )
  {
    super( name );

    this.featureTypeStyles = new ArrayList();

    setTitle( title );
    setAbstract( abstract_ );
    setDefault( default_ );
    setFeatureTypeStyles( featureTypeStyles );
  }

  /**
   * The Title is a human-readable short description for the style that might be displayed in a GUI pick list.
   * 
   * @return the title of the User-Style
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * sets the <Title>
   * 
   * @param title
   *          the title of the User-Style
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * the Abstract is a more exact description that may be a few paragraphs long.
   * 
   * @return the abstract of the User-Style
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * sets the <Abstract>
   * 
   * @param abstract_
   *          the abstract of the User-Style
   */
  public void setAbstract( String abstract_ )
  {
    this.abstract_ = abstract_;
  }

  /**
   * The IsDefault element identifies whether a style is the default style of a layer, for use in SLD library mode when
   * rendering or for storing inside of a map server. The default value is <tt>false</tt>.
   * 
   * @return true if the style ist the default style
   */
  public boolean isDefault()
  {
    return default_;
  }

  /**
   * sets the <Default>
   * 
   * @param default_
   */
  public void setDefault( boolean default_ )
  {
    this.default_ = default_;
  }

  /**
   * A UserStyle can contain one or more FeatureTypeStyles which allow the rendering of features of specific types.
   * <p>
   * </p>
   * The FeatureTypeStyle defines the styling that is to be applied to a single feature type of a layer.
   * <p>
   * </p>
   * The FeatureTypeStyle element identifies that explicit separation in SLD between the handling of layers and the
   * handling of features of specific feature types. The layer concept is unique to WMS and SLD, but features are used
   * more generally, such as in WFS and GML, so this explicit separation is important.
   * 
   * @return the FeatureTypeStyles of a User-Style
   */
  public FeatureTypeStyle[] getFeatureTypeStyles()
  {
    FeatureTypeStyle[] ft = new FeatureTypeStyle[featureTypeStyles.size()];

    return (FeatureTypeStyle[])featureTypeStyles.toArray( ft );
  }

  /**
   * sets the <FeatureTypeStyle>
   * 
   * @param featureTypeStyles
   *          the FeatureTypeStyles of a User-Style
   */
  public void setFeatureTypeStyles( FeatureTypeStyle[] featureTypeStyles )
  {
    this.featureTypeStyles.clear();

    if( featureTypeStyles != null )
    {
      for( int i = 0; i < featureTypeStyles.length; i++ )
      {
        addFeatureTypeStyle( featureTypeStyles[i] );
      }
    }
  }

  /**
   * Adds a <FeatureTypeStyle>
   * 
   * @param featureTypeStyle
   *          a FeatureTypeStyle to add
   */
  public void addFeatureTypeStyle( FeatureTypeStyle featureTypeStyle )
  {
    featureTypeStyles.add( featureTypeStyle );
  }

  /**
   * Removes a <FeatureTypeStyle>
   */
  public void removeFeatureTypeStyle( FeatureTypeStyle featureTypeStyle )
  {
    if( featureTypeStyles.indexOf( featureTypeStyle ) != -1 )
    {
      featureTypeStyles.remove( featureTypeStyles.indexOf( featureTypeStyle ) );
    }
  }

  /**
   * exports the content of the UserStyle as XML formated String
   * 
   * @return xml representation of the UserStyle
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 100 );
    sb.append( "<UserStyle>" );
    if( name != null && !name.equals( "" ) )
    {
      sb.append( "<Name>" ).append( name ).append( "</Name>" );
    }
    if( title != null && !title.equals( "" ) )
    {
      sb.append( "<Title>" ).append( title ).append( "</Title>" );
    }
    if( abstract_ != null && !abstract_.equals( "" ) )
    {
      sb.append( "<Abstract>" ).append( abstract_ ).append( "</Abstract>" );
    }
    if( default_ )
    {
      sb.append( "<IsDefault>" ).append( 1 ).append( "</IsDefault>" );
    }
    for( int i = 0; i < featureTypeStyles.size(); i++ )
    {
      sb.append( ( (Marshallable)featureTypeStyles.get( i ) ).exportAsXML() );
    }
    sb.append( "</UserStyle>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }

}