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
import org.kalypsodeegree.graphics.sld.Rule;
import org.kalypsodeegree.xml.Marshallable;
import org.kalypsodeegree_impl.tools.Debug;

/**
 * The FeatureTypeStyle defines the styling that is to be applied to a single feature type of a layer). This element may
 * also be externally re-used outside of the scope of WMSes and layers.
 * <p>
 * </p>
 * The FeatureTypeStyle element identifies that explicit separation in SLD between the handling of layers and the
 * handling of features of specific feature types. The layer concept is unique to WMS and SLD, but features are used
 * more generally, such as in WFS and GML, so this explicit separation is important.
 * <p>
 * ----------------------------------------------------------------------
 * </p>
 * 
 * @author <a href="mailto:k.lupp@web.de">Katharina Lupp </a>
 * @version $Revision$ $Date$
 */
public class FeatureTypeStyle_Impl implements FeatureTypeStyle, Marshallable
{
  private ArrayList rules = null;

  private ArrayList semanticTypeIdentifier = null;

  private String abstract_ = null;

  private String featureTypeName = null;

  private String name = null;

  private String title = null;

  /**
   * default constructor
   */
  public FeatureTypeStyle_Impl()
  {
    semanticTypeIdentifier = new ArrayList();
    rules = new ArrayList();
  }

  /**
   * constructor initializing the class with the <FeatureTypeStyle>
   */
  FeatureTypeStyle_Impl( String name, String title, String abstract_, String featureTypeName,
      String[] semanticTypeIdentifier, Rule[] rules )
  {
    this();
    setName( name );
    setTitle( title );
    setAbstract( abstract_ );
    setFeatureTypeName( featureTypeName );
    setSemanticTypeIdentifier( semanticTypeIdentifier );
    setRules( rules );
  }

  /**
   * The Name element does not have an explicit use at present, though it conceivably might be used to reference a
   * feature style in some feature-style library.
   * 
   * @return name
   */
  public String getName()
  {
    return name;
  }

  /**
   * The Name element does not have an explicit use at present, though it conceivably might be used to reference a
   * feature style in some feature-style library. Sets the <Name>o
   * 
   * @param name
   *          the name
   */
  public void setName( String name )
  {
    this.name = name;
  }

  /**
   * human-readable information about the style
   * 
   * @return the title of the FeatureTypeStyle
   */
  public String getTitle()
  {
    return title;
  }

  /**
   * sets the <Title>
   * 
   * @param title
   *          the title of the FeatureTypeStyle
   */
  public void setTitle( String title )
  {
    this.title = title;
  }

  /**
   * human-readable information about the style
   * 
   * @return an abstract of the FeatureTypeStyle
   */
  public String getAbstract()
  {
    return abstract_;
  }

  /**
   * sets <Abstract>
   * 
   * @param abstract_
   *          an abstract of the FeatureTypeStyle
   */
  public void setAbstract( String abstract_ )
  {
    this.abstract_ = abstract_;
  }

  /**
   * returns the name of the affected feature type
   * 
   * @return the name of the FeatureTypeStyle as String
   */
  public String getFeatureTypeName()
  {
    return featureTypeName;
  }

  /**
   * sets the name of the affected feature type
   * 
   * @param featureTypeName
   *          the name of the FeatureTypeStyle
   */
  public void setFeatureTypeName( String featureTypeName )
  {
    this.featureTypeName = featureTypeName;
  }

  /**
   * The SemanticTypeIdentifier is experimental and is intended to be used to identify what the feature style is
   * suitable to be used for using community- controlled name(s). For example, a single style may be suitable to use
   * with many different feature types. The syntax of the SemanticTypeIdentifier string is undefined, but the strings
   * generic:line, generic:polygon, generic:point, generic:text, generic:raster, and generic:any are reserved to
   * indicate that a FeatureTypeStyle may be used with any feature type with the corresponding default geometry type
   * (i.e., no feature properties are referenced in the feature-type style).
   * 
   * @return the SemanticTypeIdentifiers from the FeatureTypeStyle as String-Array
   */
  public String[] getSemanticTypeIdentifier()
  {
    return (String[])semanticTypeIdentifier.toArray( new String[semanticTypeIdentifier.size()] );
  }

  /**
   * Sets the SemanticTypeIdentifiers.
   * 
   * @param semanticTypeIdentifiers
   *          SemanticTypeIdentifiers for the FeatureTypeStyle
   */
  public void setSemanticTypeIdentifier( String[] semanticTypeIdentifiers )
  {
    semanticTypeIdentifier.clear();

    if( semanticTypeIdentifiers != null )
    {
      for( int i = 0; i < semanticTypeIdentifiers.length; i++ )
      {
        semanticTypeIdentifier.add( semanticTypeIdentifiers[i] );
      }
    }
  }

  /**
   * adds the <SemanticTypeIdentifier>
   * 
   * @param semanticTypeIdentifier
   *          SemanticTypeIdentifier to add
   */
  public void addSemanticTypeIdentifier( String semanticTypeIdentifier )
  {
    this.semanticTypeIdentifier.add( semanticTypeIdentifier );
  }

  /**
   * Removes an <SemanticTypeIdentifier>.
   * 
   * @param semanticTypeIdentifier
   *          SemanticTypeIdentifier to remove
   */
  public void removeSemanticTypeIdentifier( String semanticTypeIdentifier )
  {
    this.semanticTypeIdentifier.remove( this.semanticTypeIdentifier.indexOf( semanticTypeIdentifier ) );
  }

  /**
   * Rules are used to group rendering instructions by feature-property conditions and map scales. Rule definitions are
   * placed immediately inside of feature-style definitions.
   * 
   * @return the rules of the FeatureTypeStyle as Array
   */
  public Rule[] getRules()
  {
    return (Rule[])rules.toArray( new Rule[rules.size()] );
  }

  /**
   * sets the <Rules>
   * 
   * @param rules
   *          the rules of the FeatureTypeStyle as Array
   */
  public void setRules( Rule[] rules )
  {
    this.rules.clear();

    if( rules != null )
    {
      for( int i = 0; i < rules.length; i++ )
      {
        this.rules.add( rules[i] );
      }
    }
  }

  /**
   * adds the <Rules>
   * 
   * @param rule
   *          a rule
   */
  public void addRule( Rule rule )
  {
    rules.add( rule );
  }

  /**
   * removes a rule
   * 
   * @param rule
   *          a rule
   */
  public void removeRule( Rule rule )
  {
    rules.remove( rules.indexOf( rule ) );
  }

  /**
   * exports the content of the FeatureTypeStyle as XML formated String
   * 
   * @return xml representation of the FeatureTypeStyle
   */
  public String exportAsXML()
  {
    Debug.debugMethodBegin();

    StringBuffer sb = new StringBuffer( 1000 );
    sb.append( "<FeatureTypeStyle>" );
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
    if( featureTypeName != null && !featureTypeName.equals( "" ) )
    {
      sb.append( "<FeatureTypeName>" ).append( featureTypeName ).append( "</FeatureTypeName>" );
    }
    for( int i = 0; i < semanticTypeIdentifier.size(); i++ )
    {
      sb.append( "<SemanticTypeIdentifier>" ).append( semanticTypeIdentifier.get( i ) ).append(
          "</SemanticTypeIdentifier>" );
    }
    for( int i = 0; i < rules.size(); i++ )
    {
      sb.append( ( (Marshallable)rules.get( i ) ).exportAsXML() );
    }
    sb.append( "</FeatureTypeStyle>" );

    Debug.debugMethodEnd();
    return sb.toString();
  }
}